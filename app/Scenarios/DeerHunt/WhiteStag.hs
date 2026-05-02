{-# LANGUAGE DataKinds #-}

-- | Tier 2 of the unique-finds proposal: the white stag.  Each player
-- identity has exactly one across their lifetime.  Stature grows with
-- the wait — early sightings are a small Yearling; patient ones meet
-- an Elder, an Ancient, or (~1% of identities) a Myth.
--
-- This module owns the stag's tag vocabulary, stature math,
-- pubkey-derived rendering, encounter axiom, action surface, and the
-- end-of-hunt classifier the launcher uses to update the per-identity
-- progress file.
--
-- Eligibility is computed in 'Engine.Sync.Progress.lifetimeFindEligible';
-- this module decides what happens given an eligible hunt.
--
-- __Status (WIP, 2026-05-02):__ this module compiles and is unit-
-- testable in isolation, but it is __not yet wired into
-- 'Scenarios.DeerHunt'__.  Remaining work, in order:
--
--   1. Change 'SDL.Launcher.ScenarioEntry.entryMake' to pass through
--      'Progress' and 'Ed25519.PublicKey'.
--   2. Add 'encounterAxiom' to the DeerHunt scenario's axiom list and
--      'whiteStagActions' to its action list when @StagThisHunt@.
--   3. Add an end-of-hunt hook in the launcher that calls
--      'classifyEndOfHunt' on the final world and dispatches to the
--      right 'Engine.Sync.Progress' transition
--      ('recordLifetimeClaim', 'recordLifetimePass',
--      'recordLifetimeLinger').
--   4. Add e2e scenario tests covering claim, pass, fail-claim,
--      linger, and lost-stag paths.
module Scenarios.DeerHunt.WhiteStag
  ( -- * Stature
    Stature (..)
  , statureTier

    -- * Tag vocabulary
  , WhiteStagTag (..)
  , whiteStagPresent
  , whiteStagSeen
  , whiteStagClaimed
  , whiteStagPassed
  , whiteStagFailedClaim
  , whiteStagAncient
    -- ^ Marker emitted on Ancient claims so future hunts can place a
    -- discreet "you killed it here" location marker.  Persists across
    -- epoch rotations so the player carries the history.
  , statureTag

    -- * Eligibility plumbing
  , StagPresence (..)
  , presenceFor
  , stagLocationFor
  , initialStagTags

    -- * Rendering (pubkey-derived)
  , coatDescription
  , antlerDescription

    -- * Narration + journal
  , encounterNarration
  , journalEntry

    -- * Encounter / actions
  , encounterAxiom
  , whiteStagActions

    -- * End-of-hunt classification
  , EndOfHuntOutcome (..)
  , classifyEndOfHunt
  ) where

import qualified Crypto.PubKey.Ed25519   as Ed25519
import qualified Data.ByteArray          as BA
import qualified Data.ByteString         as BS
import           Data.Word               (Word8)
import qualified Data.Map.Strict         as Map

import           Engine.Author.DSL
import           Engine.Core.Conditions  (checkCondition)
import           Engine.Core.World       (characterLocation)
import           Engine.Sync.Progress    (LifetimeFindState (..), lifetimeFindEligible)
import           GameTypes
import           Scenarios.DeerHunt.World (HuntWorld (..))

-- ---------------------------------------------------------------------------
-- Stature
-- ---------------------------------------------------------------------------

-- | The stag's stature when encountered, derived from the player's
-- current hunt counter.  See the proposal for the per-tier narrative
-- weight.
data Stature
  = Yearling   -- ^ N = 1–5: small, unremarkable.  "You saw a white deer once."
  | Prime      -- ^ N = 6–15: a proper stag.  Distinct rack, real story.
  | Elder      -- ^ N = 16–30: heavy-bodied legend.  Multi-paragraph journal.
  | Ancient    -- ^ N = 31–60: scarred, white through-and-through.  Marks future maps.
  | Myth       -- ^ N > 60: once-in-a-playtime.  Story for the ages.
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Map a hunt counter to a stature tier.  Bands match the proposal.
statureTier :: Int -> Stature
statureTier n
  | n <= 5    = Yearling
  | n <= 15   = Prime
  | n <= 30   = Elder
  | n <= 60   = Ancient
  | otherwise = Myth

-- ---------------------------------------------------------------------------
-- Tag vocabulary
-- ---------------------------------------------------------------------------

-- | Per-hunt world tags carrying white-stag state.  Most are flags
-- the encounter axiom and Pass/Claim actions write; a few mark
-- stature so the renderer can pick prose without re-deriving from
-- N every tick.
data WhiteStagTag
  = WhiteStagPresent          -- ^ This hunt contains the stag (set at scenario init when eligible).
  | WhiteStagSeen             -- ^ Player crossed paths with the stag this hunt.
  | WhiteStagClaimed          -- ^ Player chose Claim and it landed.
  | WhiteStagPassed           -- ^ Player chose Pass.
  | WhiteStagFailedClaim      -- ^ Claim attempted and failed (counts as pass under the decay curve).
  | WhiteStagAncient          -- ^ A historical marker — Ancient or Myth was claimed in some prior epoch.
  | WhiteStagYearling
  | WhiteStagPrime
  | WhiteStagElder
  | WhiteStagAncientStature
  | WhiteStagMyth
  deriving (Show, Eq, Ord, Enum, Bounded)

whiteStagPresent, whiteStagSeen, whiteStagClaimed, whiteStagPassed,
  whiteStagFailedClaim, whiteStagAncient :: Tag
whiteStagPresent     = scenarioTag WhiteStagPresent
whiteStagSeen        = scenarioTag WhiteStagSeen
whiteStagClaimed     = scenarioTag WhiteStagClaimed
whiteStagPassed      = scenarioTag WhiteStagPassed
whiteStagFailedClaim = scenarioTag WhiteStagFailedClaim
whiteStagAncient     = scenarioTag WhiteStagAncient

-- | Stature tag for the active hunt.  Different constructor names
-- avoid colliding with the historical 'WhiteStagAncient' marker
-- above (which is "I have killed an Ancient at some point" rather
-- than "the stag in *this* hunt is an Ancient").
statureTag :: Stature -> Tag
statureTag Yearling = scenarioTag WhiteStagYearling
statureTag Prime    = scenarioTag WhiteStagPrime
statureTag Elder    = scenarioTag WhiteStagElder
statureTag Ancient  = scenarioTag WhiteStagAncientStature
statureTag Myth     = scenarioTag WhiteStagMyth

-- ---------------------------------------------------------------------------
-- Eligibility plumbing
-- ---------------------------------------------------------------------------

-- | What this hunt should do about the lifetime stag.  Built once at
-- scenario init from the player's progress + pubkey + hunt seed.
data StagPresence
  = NoStagThisHunt
    -- ^ Either the player has no eligibility this hunt, or the find
    -- is in 'FindClaimed' state and the next epoch hasn't begun.
  | StagThisHunt !Stature !Location
    -- ^ @StagThisHunt stature location@ — the stag is in play at the
    -- given location, with the given stature derived from the
    -- player's current hunt counter.
  deriving (Show, Eq)

-- | Decide whether this hunt should contain the white stag, and if so
-- pick its stature and location.  Pure given the inputs.
presenceFor
  :: Ed25519.PublicKey
  -> Int                 -- ^ epoch
  -> Int                 -- ^ hunt counter (after the start-of-hunt increment)
  -> LifetimeFindState
  -> HuntWorld
  -> StagPresence
presenceFor pubkey epoch n state hw
  | not eligible = NoStagThisHunt
  | otherwise    = StagThisHunt (statureTier n) (stagLocationFor pubkey epoch n hw)
  where
    eligible = lifetimeFindEligible pubkey epoch n state

-- | Pick the stag's location deterministically from the player and
-- the hunt.  Uses the seeded 'hwByClass' map and prefers ridge/bush
-- (cover terrain) — the stag is, after all, a hidden thing.
stagLocationFor :: Ed25519.PublicKey -> Int -> Int -> HuntWorld -> Location
stagLocationFor pubkey epoch n hw =
  case candidates of
    []    -> hwDeerStartLoc hw   -- defensive: any map should have at least one
    cs    -> cs !! (locRoll `mod` length cs)
  where
    -- Cover terrain only; the stag isn't on the road or in a field.
    coverClasses = ["CBush", "CRidge", "CCreek"]
    candidates =
      [ loc | (cls, locs) <- Map.toList (hwByClass hw)
            , show cls `elem` coverClasses
            , loc <- locs
            ]
    locRoll = stagLocationRoll pubkey epoch n (hwSeed hw)

-- | Initial world tags for a hunt that contains the stag.  Add these
-- to 'scenarioInitial.worldTags' so the encounter axiom can fire.
-- The stag's location is captured in the encounter axiom's closure
-- rather than encoded as a tag — it's per-hunt deterministic and
-- doesn't need to round-trip through JSON.
initialStagTags :: StagPresence -> [Tag]
initialStagTags NoStagThisHunt              = []
initialStagTags (StagThisHunt stature _loc) =
  [ whiteStagPresent
  , statureTag stature
  ]

-- ---------------------------------------------------------------------------
-- Pubkey-derived rendering
-- ---------------------------------------------------------------------------

-- | The stag's coat is described by the player's pubkey — the same
-- person always meets the same stag, even across epochs.  Coat cues
-- are mixed-and-matched from a small lexicon, four byte slots wide.
coatDescription :: Ed25519.PublicKey -> String
coatDescription pubkey =
  let bs = BA.convert pubkey :: BS.ByteString
      pickFrom xs i = xs !! (fromIntegral (BS.index bs i) `mod` length xs)
      base   = pickFrom [ "ivory-coated"
                        , "silver-flanked"
                        , "ash-grey-shouldered"
                        , "cream-and-bone"
                        , "moonlit-pale"
                        , "frost-mantled"
                        ] 0
      flank  = pickFrom [ "with a darker stripe along the spine"
                        , "with a clean, unmarked flank"
                        , "with one pale shoulder and one frost-grey"
                        , "with a bramble-scarred chest"
                        , "with a single dark hoof, the other three pale"
                        ] 1
  in base <> ", " <> flank

-- | Antler description varies with both the pubkey (rack form is the
-- player's identity) and the stature (scale grows with N).
antlerDescription :: Ed25519.PublicKey -> Stature -> String
antlerDescription pubkey stature =
  let bs = BA.convert pubkey :: BS.ByteString
      pickFrom xs i = xs !! (fromIntegral (BS.index bs i) `mod` length xs)
      form = pickFrom [ "a wide, even rack"
                      , "a tall, narrow crown"
                      , "an asymmetric set, one side reaching higher"
                      , "a heavy mainbeam with sweeping tines"
                      , "a tight, knife-edged set"
                      ] 2
      modifier = case stature of
        Yearling -> "small for the form, still growing"
        Prime    -> "fully grown, clean tines"
        Elder    -> "heavy and weather-marked, points worn smooth"
        Ancient  -> "scarred, the points snapped and re-grown over years"
        Myth     -> "an antler crown like nothing you've seen, half-real"
  in form <> ", " <> modifier

-- ---------------------------------------------------------------------------
-- Narration + journal
-- ---------------------------------------------------------------------------

-- | Prose for the encounter beat.  Player sees this *before* any
-- action is forced — the proposal is explicit that the choice has to
-- come after a real look.
encounterNarration :: Ed25519.PublicKey -> Stature -> String
encounterNarration pubkey stature =
  let coat   = coatDescription pubkey
      rack   = antlerDescription pubkey stature
      framing = case stature of
        Yearling ->
          "Through the brush, a flash of white. A small stag — younger than you'd have hoped. Still: the coat is unmistakable."
        Prime ->
          "He steps from cover at the edge of the clearing. White through-and-through, in his prime. A real stag."
        Elder ->
          "The brush parts and there he is. Heavier than you'd imagined, slow-moving, looking right at you. He has been here a long time."
        Ancient ->
          "He doesn't startle at the sound of you. Scarred, moon-pale, ancient. You feel small in front of him."
        Myth ->
          "You don't see him so much as feel the woods change. When he steps out, he is more story than animal. The light goes wrong around him."
  in framing
       <> "\n\n"
       <> coat
       <> "; " <> rack <> "."
       <> "\n\nNothing forces your hand. You can claim him, or let him pass."

-- | Journal text written when the stag is claimed.  Length and weight
-- grow with stature.
journalEntry :: Ed25519.PublicKey -> Stature -> Int -> String
journalEntry pubkey stature n =
  let coat = coatDescription pubkey
      rack = antlerDescription pubkey stature
      header = case stature of
        Yearling -> "The young white stag, Hunt #" <> show n
        Prime    -> "The white stag in his prime, Hunt #" <> show n
        Elder    -> "The Elder, Hunt #" <> show n
        Ancient  -> "The Ancient, Hunt #" <> show n
        Myth     -> "The Myth, Hunt #" <> show n
      body = case stature of
        Yearling ->
          "I saw a white deer once. He was small and bright and quick, and he didn't know me yet. " <> coat <> "; " <> rack <> "."
        Prime ->
          "He stepped out of the brush at the edge of the clearing and waited a beat too long. Distinct rack, real weight, full coat. " <> coat <> "; " <> rack <> "."
        Elder ->
          "I'd been hunting long enough to know him when I saw him.\n\n" <>
          "He came out of the bush slow, deliberate, looking at me before I looked at him. Heavier in the chest than I expected, with the patient stillness that comes from years.\n\n" <>
          coat <> "; " <> rack <> ".\n\nThe woods went quiet around him."
        Ancient ->
          "The Ancient came on Hunt #" <> show n <> ", and he didn't startle.\n\n" <>
          "He was scarred. Old wounds I'd never get to learn the story of. The points of his rack were snapped and re-grown more than once. Moon-pale and weathered, every step measured.\n\n" <>
          coat <> "; " <> rack <> ".\n\nI'll mark this place. He was here. He won't be again."
        Myth ->
          "I waited a long time for him.\n\n" <>
          "He didn't enter the clearing the way an animal enters a clearing. The light went wrong around him; the woods stopped being the woods. Whether what I saw was a stag at all is something I'll think about for years.\n\n" <>
          coat <> "; " <> rack <> ".\n\nI'm putting this down because I don't trust the memory to hold."
  in header <> "\n" <> body

-- ---------------------------------------------------------------------------
-- Encounter axiom
-- ---------------------------------------------------------------------------

-- | When the player co-locates with the stag for the first time this
-- hunt, narrate the encounter and add 'whiteStagSeen' so Pass/Claim
-- actions become available.  The stag's location and stature are
-- captured by closure at scenario init.
--
-- For 'NoStagThisHunt', returns an axiom that never fires — keeps
-- the @scenarioAxioms@ list shape uniform across eligible/ineligible
-- hunts so the rest of DeerHunt doesn't have to branch.
encounterAxiom :: CharacterId -> Ed25519.PublicKey -> StagPresence -> Axiom
encounterAxiom _you _pubkey NoStagThisHunt = Axiom
  { axiomId       = ScenarioAxiom "whiteStagEncounter"
  , axiomPriority = 5
  , axiomEvaluate = \_w _a _d -> []
  }
encounterAxiom you pubkey (StagThisHunt stature loc) = Axiom -- (stature, loc) used below
  { axiomId       = ScenarioAxiom "whiteStagEncounter"
  , axiomPriority = 5
  , axiomEvaluate = \world _actions diff ->
      if hasWorldTag whiteStagSeen world
         || not (locationChanged you diff)
         || characterLocation you world /= Just loc
        then []
        else
          [ immediate (Narrate (encounterNarration pubkey stature))
          , immediate (AddWorldTag whiteStagSeen)
          ]
  }
  where
    locationChanged cid d =
      any (\ld -> locationDeltaChar ld == cid) (diffLocations d)

-- ---------------------------------------------------------------------------
-- Actions: Claim, Pass, FailClaim
-- ---------------------------------------------------------------------------

-- | The three white-stag actions.  Available only when the stag is
-- present, has been seen, and hasn't already been resolved.
whiteStagActions :: CharacterId -> [AnyAction]
whiteStagActions you =
  [ AnyAction (claimStag you)
  , AnyAction (passStag you)
  , AnyAction (failClaim you)
  ]

claimStag :: CharacterId -> Action 'Once
claimStag _you = onceAction (ActionId "whiteStag.claim")
  "Claim the white stag"
  stagInteractionCondition
  [ immediate (AddWorldTag whiteStagClaimed)
  , immediate (Narrate "You take him.  The shot is clean.")
  ]

passStag :: CharacterId -> Action 'Once
passStag _you = onceAction (ActionId "whiteStag.pass")
  "Let him pass"
  stagInteractionCondition
  [ immediate (AddWorldTag whiteStagPassed)
  , immediate (Narrate "You watch him go.  The brush takes him back, slow.")
  ]

-- | A second \"shot\" path that fails — surfaces the proposal's fail-
-- claim case.  Authors can either expose this or hide it; for a first
-- cut it sits alongside the other two so the mechanic is reachable
-- in the action menu.
failClaim :: CharacterId -> Action 'Once
failClaim _you = onceAction (ActionId "whiteStag.fumble")
  "Take the shot (long)"
  stagInteractionCondition
  [ immediate (AddWorldTag whiteStagFailedClaim)
  , immediate (Narrate "Too far. The shot pulls; the brush takes him in an instant.")
  ]

-- | The white-stag actions all share the same gate: stag present,
-- player has seen the stag, and no resolution tag set yet.
stagInteractionCondition :: Condition
stagInteractionCondition =
  All [ HasWorldTag whiteStagPresent
      , HasWorldTag whiteStagSeen
      , Not (HasWorldTag whiteStagClaimed)
      , Not (HasWorldTag whiteStagPassed)
      , Not (HasWorldTag whiteStagFailedClaim)
      ]

-- ---------------------------------------------------------------------------
-- End-of-hunt classification
-- ---------------------------------------------------------------------------

-- | What the launcher should do with the player's progress file given
-- the final world state of a completed hunt.
data EndOfHuntOutcome
  = NoStagInPlay
    -- ^ Hunt didn't contain the stag.  No update.
  | StagClaimedThisHunt
    -- ^ Player tagged it.  Launcher calls 'recordLifetimeClaim' (which
    -- also rotates the epoch).
  | StagPassedThisHunt
    -- ^ Either chosen pass or fumbled claim.  Launcher calls
    -- 'recordLifetimePass'.
  | StagLingered
    -- ^ Stag was eligible but the player never crossed its path.
    -- Launcher calls 'recordLifetimeLinger' to undo the start-of-hunt
    -- counter increment.
  deriving (Show, Eq)

-- | Inspect the final world's tags to decide what to do.  Resolution
-- tags take precedence; the linger case fires only when the stag was
-- present but never seen.
classifyEndOfHunt :: GameWorld -> EndOfHuntOutcome
classifyEndOfHunt world
  | not (hasWorldTag whiteStagPresent world) = NoStagInPlay
  | hasWorldTag whiteStagClaimed world       = StagClaimedThisHunt
  | hasWorldTag whiteStagPassed world
      || hasWorldTag whiteStagFailedClaim world = StagPassedThisHunt
  | hasWorldTag whiteStagSeen world          = StagPassedThisHunt
    -- Saw the stag, didn't act, hunt ended.  Treat as a pass —
    -- the player walked away.
  | otherwise                                = StagLingered

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

hasWorldTag :: Tag -> GameWorld -> Bool
hasWorldTag t = checkCondition' (HasWorldTag t)
  where
    checkCondition' c w = checkCondition w c


-- | Per-thousand integer roll over @hash(pubkey || epoch || n || seed)@.
-- Same construction as 'Engine.Sync.Progress.eligibilityRoll', folded
-- in here so we don't expose that function publicly.
stagLocationRoll :: Ed25519.PublicKey -> Int -> Int -> Int -> Int
stagLocationRoll pubkey epoch n seed =
  let pk = BA.convert pubkey :: BS.ByteString
      bs = pk
        <> BS.pack (intBytes epoch)
        <> BS.pack (intBytes n)
        <> BS.pack (intBytes seed)
      sumBytes = BS.foldr ((+) . fromIntegral) (0 :: Int) (BS.take 8 bs)
  in sumBytes

-- | Little-endian byte serialization.  Fixed at 8 bytes so the hash
-- input is positional and stable across platforms.
intBytes :: Int -> [Word8]
intBytes x =
  [ fromIntegral ((x `div` (2 ^ (8 * i))) `mod` 256) | i <- [0..7 :: Int] ]
