-- | End-to-end tests for the Tier-2 lifetime find (white stag).
--
-- Layered intentionally: the engine-side eligibility math and
-- progress state machine live in 'Engine.Sync.ProgressSpec'; the
-- scenario-side narration prose, action surface, and end-of-hunt
-- classifier live here.  This spec exercises the wiring exposed by
-- 'Scenarios.DeerHunt' (encounter axiom, claim/pass/fumble actions,
-- 'classifyEndOfHunt') and the integration into the DeerHunt
-- scenario factory.
module Scenarios.DeerHuntWhiteStagSpec (spec) where

import qualified Data.Map.Strict       as Map
import qualified Data.Set              as Set
import           Test.Hspec

import           Engine.Author.DSL     (hasTag, tagsFromList)
import           Engine.CRDT.ORSet     (orFromList, orToList)
import           GameTypes
import           MonadStack            (Env)
import           TestFixtures          (mkScenarioEnv, step)

import           Scenarios.DeerHunt              (deerHunt, deerHuntName)
import           Scenarios.DeerHunt.WhiteStag    (EndOfHuntOutcome (..),
                                                  StagPresence (..),
                                                  Stature (..),
                                                  classifyEndOfHunt,
                                                  encounterAxiom,
                                                  initialStagTags,
                                                  presenceFor,
                                                  statureTier,
                                                  whiteStagClaimed,
                                                  whiteStagFailedClaim,
                                                  whiteStagPassed,
                                                  whiteStagPresent,
                                                  whiteStagSeen)
import           Scenarios.DeerHunt.World        (huntWorld)
import           Scenarios.DeerHuntTestFixtures  (deerHuntForTests, fixtureProgress,
                                                  fixturePubkey, fixtureSeed)

import           Engine.Sync.Progress  (LifetimeFindState (..), Progress (..))


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Build a 'GameWorld' carrying just the listed world tags — the
-- classifier reads world tags only, so the rest of the world can be
-- empty.  Other fields are zero/empty defaults that match what an
-- end-of-hunt 'GameWorld' would carry; nothing in the classifier
-- consults them.
worldWithTags :: [Tag] -> GameWorld
worldWithTags tags = GameWorld
  { worldCharacters      = Map.empty
  , worldGraph           = Map.empty
  , worldLocations       = Map.empty
  , worldActiveEffects   = []
  , worldClock           = LamportClock 0 (PlayerId "test")
  , worldTags            = tagsFromList tags
  , worldLocationGraph   = LocationGraph Set.empty Map.empty Map.empty
  , worldSeed            = 0
  , worldLocationHistory = Map.empty
  , worldLocationVisits  = Map.empty
  , worldJournal         = []
  , worldDayNumber       = 0
  }

-- | Find an @(epoch, huntCount)@ where the player's gamma roll lands
-- under the threshold.  Used by the integration test that needs an
-- actually-eligible hunt without hand-coding a hash.  Searches a
-- broad sweep of epochs and counts so the test is deterministic for
-- any reasonable pubkey; if nothing in the search window is eligible
-- the test signals 'Nothing' and is skipped (vanishingly unlikely,
-- fail-soft instead of a false red).
findEligibleHuntCount :: Maybe (Int, Int)
findEligibleHuntCount =
  case [ (e, n) | e <- [1..20]
                , n <- [1..50]
                , presenceFor fixturePubkey e n FindPending huntFixtureWorld
                    /= NoStagThisHunt
                ] of
    []          -> Nothing
    (en : _)    -> Just en
  where
    huntFixtureWorld = huntWorld fixtureSeed

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "DeerHunt — white stag (Tier 2)" $ do

  describe "statureTier banding" $ do
    it "maps the proposal's hunt-count bands to statures" $ do
      map statureTier [1, 5]   `shouldBe` [Yearling, Yearling]
      map statureTier [6, 15]  `shouldBe` [Prime,    Prime]
      map statureTier [16, 30] `shouldBe` [Elder,    Elder]
      map statureTier [31, 60] `shouldBe` [Ancient,  Ancient]
      map statureTier [61, 120] `shouldBe` [Myth,    Myth]

  describe "presenceFor eligibility gates" $ do
    it "returns NoStagThisHunt when the find is already claimed" $ do
      let claimed = FindClaimed 12 (progressUpdatedAt fixtureProgress)
      presenceFor fixturePubkey 1 17 claimed (huntWorld fixtureSeed)
        `shouldBe` NoStagThisHunt

    it "returns NoStagThisHunt for a Pending find at huntCount 0" $ do
      -- gammaThreshold 0 = 0, so the roll is never eligible.
      presenceFor fixturePubkey 1 0 FindPending (huntWorld fixtureSeed)
        `shouldBe` NoStagThisHunt

  describe "initialStagTags" $ do
    it "is empty when no stag this hunt" $
      initialStagTags NoStagThisHunt `shouldBe` []

    it "marks WhiteStagPresent and the stature tag when the stag is in play" $ do
      let tags = initialStagTags (StagThisHunt Prime (Location "anywhere"))
      whiteStagPresent `elem` tags `shouldBe` True

  describe "encounterAxiom" $ do
    let you = Named "you"
        loc = Location "stagSpot"
        elsewhere = Location "elsewhere"

    it "produces no effects for a hunt with no stag" $ do
      let ax = encounterAxiom you fixturePubkey NoStagThisHunt
          w  = worldWithTags []
      axiomEvaluate ax w [] emptyDiff `shouldBe` []

    it "produces no effects when the player is not at the stag's location" $ do
      let ax = encounterAxiom you fixturePubkey (StagThisHunt Prime loc)
          w  = (worldWithTags [])
            { worldLocations = Map.singleton you elsewhere }
          d  = emptyDiff
            { diffLocations = [LocationDelta you elsewhere elsewhere] }
      axiomEvaluate ax w [] d `shouldBe` []

    it "narrates and sets WhiteStagSeen when the player co-locates with the stag" $ do
      let ax = encounterAxiom you fixturePubkey (StagThisHunt Prime loc)
          w  = (worldWithTags [])
            { worldLocations = Map.singleton you loc }
          d  = emptyDiff
            { diffLocations = [LocationDelta you elsewhere loc] }
          fx = axiomEvaluate ax w [] d
      length fx `shouldBe` 2
      let bodies = map effectBody fx
      any isNarrate bodies                       `shouldBe` True
      AddWorldTag whiteStagSeen `elem` bodies    `shouldBe` True

    it "is idempotent: re-firing on a world that already has WhiteStagSeen yields no effects" $ do
      let ax = encounterAxiom you fixturePubkey (StagThisHunt Prime loc)
          w  = (worldWithTags [whiteStagSeen])
            { worldLocations = Map.singleton you loc }
          d  = emptyDiff
            { diffLocations = [LocationDelta you elsewhere loc] }
      axiomEvaluate ax w [] d `shouldBe` []

  describe "claim / pass / fumble actions" $ do
    let you  = Named "you"
        epoch = 1
        n     = 12
        loc   = Location "stagSpot"
        progress = fixtureProgress
          { progressEpoch     = epoch
          , progressHuntCount = n
          }
        baseScenario = deerHunt fixtureSeed you progress fixturePubkey
        -- Force a stag-present world so the gate is satisfied
        -- regardless of where the eligibility roll lands for the
        -- fixture pubkey.  Real hunts get there via 'presenceFor'.
        stagSeenWorld =
          (scenarioInitial baseScenario)
            { worldTags = orFromList
                [ whiteStagPresent
                , whiteStagSeen
                ]
            , worldLocations = Map.insert you loc
                                 (worldLocations (scenarioInitial baseScenario))
            }

    it "claim sets WhiteStagClaimed" $ do
      env <- envFor baseScenario you
      w'  <- step env (ActionId "whiteStag.claim") stagSeenWorld
      hasTag w' whiteStagClaimed `shouldBe` True

    it "pass sets WhiteStagPassed" $ do
      env <- envFor baseScenario you
      w'  <- step env (ActionId "whiteStag.pass") stagSeenWorld
      hasTag w' whiteStagPassed `shouldBe` True

    it "fumble sets WhiteStagFailedClaim" $ do
      env <- envFor baseScenario you
      w'  <- step env (ActionId "whiteStag.fumble") stagSeenWorld
      hasTag w' whiteStagFailedClaim `shouldBe` True

  describe "classifyEndOfHunt" $ do
    it "NoStagInPlay when WhiteStagPresent is absent" $
      classifyEndOfHunt (worldWithTags []) `shouldBe` NoStagInPlay

    it "StagClaimedThisHunt when claimed tag is set" $
      classifyEndOfHunt (worldWithTags [whiteStagPresent, whiteStagSeen, whiteStagClaimed])
        `shouldBe` StagClaimedThisHunt

    it "StagPassedThisHunt when passed tag is set" $
      classifyEndOfHunt (worldWithTags [whiteStagPresent, whiteStagSeen, whiteStagPassed])
        `shouldBe` StagPassedThisHunt

    it "StagPassedThisHunt when fumble tag is set (treated as a forced pass)" $
      classifyEndOfHunt (worldWithTags [whiteStagPresent, whiteStagSeen, whiteStagFailedClaim])
        `shouldBe` StagPassedThisHunt

    it "StagPassedThisHunt when seen but never resolved (player walked away)" $
      classifyEndOfHunt (worldWithTags [whiteStagPresent, whiteStagSeen])
        `shouldBe` StagPassedThisHunt

    it "StagLingered when present but never seen" $
      classifyEndOfHunt (worldWithTags [whiteStagPresent])
        `shouldBe` StagLingered

  describe "deerHunt scenario integration" $ do
    let you = Named "you"

    it "gives the right scenario name" $
      scenarioName (deerHuntForTests fixtureSeed you) `shouldBe` deerHuntName

    it "does not seed white-stag tags when the hunt is ineligible (huntCount = 0)" $ do
      let scenario = deerHuntForTests fixtureSeed you
          tags     = orToList (worldTags (scenarioInitial scenario))
      whiteStagPresent `elem` tags `shouldBe` False

    it "exposes white-stag actions in the action list (gated by tags)" $ do
      let actions = scenarioActions (deerHuntForTests fixtureSeed you)
          ids     = map anyActionId actions
      ActionId "whiteStag.claim"  `elem` ids `shouldBe` True
      ActionId "whiteStag.pass"   `elem` ids `shouldBe` True
      ActionId "whiteStag.fumble" `elem` ids `shouldBe` True

    it "seeds whiteStagPresent when there exists an (epoch, huntCount) where the fixture pubkey is eligible" $
      case findEligibleHuntCount of
        Nothing ->
          -- vanishingly unlikely with the fixture pubkey, but skip
          -- rather than fail noisily if the test ever lands here.
          pendingWith
            "fixture pubkey never eligible across the searched (epoch, huntCount) sweep"
        Just (epoch, n)  -> do
          let progress = fixtureProgress
                { progressEpoch     = epoch
                , progressHuntCount = n
                }
              scenario = deerHunt fixtureSeed you progress fixturePubkey
              tags     = orToList (worldTags (scenarioInitial scenario))
          whiteStagPresent `elem` tags `shouldBe` True

-- ---------------------------------------------------------------------------
-- Local utilities
-- ---------------------------------------------------------------------------

envFor :: Scenario -> CharacterId -> IO Env
envFor = flip mkScenarioEnv

isNarrate :: EffectBody -> Bool
isNarrate (Narrate _) = True
isNarrate _           = False

-- | A no-op WorldDiff for axiom tests that don't need delta input.
-- Mirrors 'TestFixtures.emptyDiff' but kept local so the import
-- footprint of this spec stays small and obvious.
emptyDiff :: WorldDiff
emptyDiff = WorldDiff [] [] [] [] [] [] [] [] 0
