module Scenarios.DeerHunt (deerHunt, deerHuntDisplay) where

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import           Engine.Author.DSL      (hasTag, olderThanDays, worldTagList)
import           Engine.Core.Conditions (checkCondition)
import           Engine.Core.Time       (currentHour)
import           SDL.Layout
import           SDL.Palette  (Color, zoneTintDefault)
import           SDL.Text
import           GameTypes
import           Scenarios.DeerHunt.Actions     (allActions)
import           Scenarios.DeerHunt.Axioms      (allAxioms, dawnRule,
                                                 hunterArrivalMergeAxiom)
import           Scenarios.DeerHunt.Constants
import           Scenarios.DeerHunt.Discoveries (discoveryCatalog)
import           Scenarios.DeerHunt.Generation  (TerrainClass(..))
import           Scenarios.DeerHunt.Narration   (sensoryFragment)
import           Scenarios.DeerHunt.Probability (experience)
import           Scenarios.DeerHunt.Signature   (SignatureArchetype(..),
                                                 archetypeHint,
                                                 parseSignatureLocTag,
                                                 parseSignatureArchetypeTag,
                                                 signatureFoundTag)
import           Scenarios.DeerHunt.World       (PositionHint(..), huntWorld)

-- | Build a full DeerHunt scenario from a seed.  The 'HuntWorld' is
-- constructed once here and captured by every axiom, action, and
-- display hook that needs to consult the generated map.
deerHunt :: Int -> CharId -> Scenario
deerHunt seed you =
  let hw = huntWorld seed
  in Scenario
       { scenarioName         = "Deer Hunt"
       , scenarioInitial      = initialWorld hw you
       , scenarioActions      = allActions hw you
       , scenarioAxioms       = allAxioms hw you
       , scenarioMergeAxioms  = [hunterArrivalMergeAxiom you]
       , scenarioRules        = [dawnRule you]
       , scenarioMergeRules   = []
       , scenarioTerminal     = Any [HasWorldTag seasonOver, HasWorldTag hunterShot]
       , scenarioDebugDefault = Off
       , scenarioPlayerCharId = you
       , scenarioTombstoneGC  = Just (olderThanDays 365)
         -- A year out, it's lore — not something the merge needs to
         -- keep re-adjudicating.  Matches Deer Hunt's "not this
         -- year" narrative cadence.
       }

-- | The scenario's display hooks.  Unlike 'deerHunt' these can't close
-- over a 'HuntWorld' built from a seed because the SDL runtime doesn't
-- pass a seed in — the 'GameWorld' carries what we need instead.  The
-- display hooks consult 'worldLocationGraph' directly for region
-- lookups and 'worldSeed' plus adjacency math for sparkle propagation.
deerHuntDisplay :: ScenarioDisplay
deerHuntDisplay = ScenarioDisplay
  { sdEndScreen       = endScreen
  , sdStatusLine      = deerHuntStatusLine
  , sdLayout          = defaultLayout
  , sdLocationSparkle = locationSparkle
  , sdZoneTintFor     = deerHuntZoneTint
  , sdSensoryFor      = deerHuntSensory
  , sdCatalog         = discoveryCatalog
  , sdDayLabel        = formatHuntDate
  }

-- | Current time line for the top status bar.  Shows the
-- scenario's day label plus HH:MM AM/PM with minute resolution —
-- the engine's per-entry time prefix only updates on the hour, so
-- without this the player sees 12 consecutive actions all labeled
-- the same way and can't tell time is advancing.  Every tick is
-- 5 minutes (ticksPerHour = 12); minutes are computed from the
-- world's Lamport tick modulo that.
deerHuntStatusLine :: GameWorld -> Maybe String
deerHuntStatusLine world =
  case currentHour world of
    Nothing -> Nothing
    Just h  ->
      let tick    = lcTick (worldClock world)
          minute  = (tick `mod` ticksPerHour) * (60 `div` ticksPerHour)
          hour12  | h == 0    = 12
                  | h > 12    = h - 12
                  | otherwise = h
          suffix  = if h < 12 then "AM" else "PM"
          mPad    = if minute < 10 then '0' : show minute else show minute
          dayLbl  = formatHuntDate (worldDayNumber world)
      in Just (dayLbl <> "  ·  " <> show hour12 <> ":" <> mPad <> " " <> suffix)


-- | Pick a fleeting sensory fragment for a neighbor label during the
-- incremental reveal.  Usually draws from the terrain's pool, but
-- when the world state suggests the deer is nearby (freshSign in
-- the zone) the fragment has a chance to land on a "hint" line —
-- a stick crack, a shape at the edge of vision — so the ambient
-- beat quietly doubles as information.  The signature find gets
-- an analogous archetype-flavored hint when the neighbor label is
-- the signature's cell (and hasn't been discovered yet).
deerHuntSensory :: GameWorld -> Location -> Int -> Maybe String
deerHuntSensory world loc salt =
  case Map.lookup loc (lgRegions (worldLocationGraph world)) of
    Just (Region name) ->
      let cls  = regionClassHint name
          hint = positionHintFor world loc
          base = sensoryFragment cls hint salt
      in Just (situationalOverride world loc salt base)
    Nothing -> Nothing

-- | Choose a context-sensitive override for a base sensory line.
-- A deterministic per-salt roll decides between the base line and a
-- hint line drawn from an appropriate pool.  Kept intentionally
-- low-frequency: a player who never sees a hint still reads a
-- coherent ambient fragment every turn.
situationalOverride :: GameWorld -> Location -> Int -> String -> String
situationalOverride world loc salt base
  | nearSignature && roll < 0.4 = pickFrom (archetypeHintsFor world)
  | inZone && roll < 0.35 = pickFrom huntHintFragments
  | spotted && roll < 0.6 = pickFrom sightingFragments
  | otherwise             = base
  where
    inZone        = hasTag world freshSign
    spotted       = hasTag world deerSpotted
    alreadyFound  = hasTag world signatureFoundTag
    -- The signature hint only fires if this label is the signature's
    -- cell *and* the player hasn't already discovered it.  Checking
    -- "this label is that cell" rather than "player is adjacent" is
    -- correct because the sensory fragment renders on the neighbour
    -- label — so "the cell you're about to step into" carries the
    -- hint, not the one you're standing on.
    nearSignature = not alreadyFound && signatureLoc == Just loc
    signatureLoc  = firstJust (map parseSignatureLocTag (worldTagList world))
    roll          = let r = abs salt `mod` 1000 in fromIntegral r / 1000 :: Double
    pickFrom xs
      | null xs   = base
      | otherwise = xs !! (abs salt `mod` length xs)

-- | Collect the archetype-flavored hint pool for the current hunt.
-- Falls back to an empty list if no archetype tag is present (a
-- malformed init) — the caller's @null xs@ guard turns that into a
-- plain base line, so there's no narrative surprise.
archetypeHintsFor :: GameWorld -> [String]
archetypeHintsFor world =
  maybe [] archetypeHint
    (firstJust (map parseSignatureArchetypeTag (worldTagList world)))

-- | Find the first 'Just' in a list of 'Maybe's.
firstJust :: [Maybe a] -> Maybe a
firstJust []             = Nothing
firstJust (Just x  : _)  = Just x
firstJust (Nothing : xs) = firstJust xs

-- | Ambient lines that plausibly hint at nearby deer without naming
-- them.  Used when freshSign is present in the player's zone.
huntHintFragments :: [String]
huntHintFragments =
  [ "a stick crack that wasn't your boot"
  , "pause — a shape that wasn't there"
  , "something alive in here with you"
  , "movement at the edge of vision"
  , "your hair goes up for no reason"
  , "the woods hold their breath a second"
  ]

-- | Fragments for when the deer is actually spotted — more direct,
-- less "maybe."  Punchier language because the player already knows.
sightingFragments :: [String]
sightingFragments =
  [ "you can see the breath on his nose"
  , "brown line of a back against the grey"
  , "he hasn't winded you yet"
  , "antlers catching what light there is"
  ]

-- | Approximate 'TerrainClass' from a generated region name's last
-- word.  The generator always tags regions with their class in the
-- name (e.g. @"North Field"@, @"East Bush"@).
regionClassHint :: String -> TerrainClass
regionClassHint name = case lastWord name of
  "Field" -> CField
  "Bush"  -> CBush
  "Ridge" -> CRidge
  "Creek" -> CCreek
  "Road"  -> CRoad
  _       -> CEmpty
  where
    lastWord s = case reverse (words s) of
      (w:_) -> w
      []    -> ""

-- | Approximate 'PositionHint' from the world's 'LocationGraph'
-- without pulling in a 'HuntWorld'.  A location is 'Bridge' if any
-- neighbour crosses a class boundary, 'Edge' if any 2-hop neighbour
-- does, otherwise 'Interior'.  Mirrors the logic in
-- 'Scenarios.DeerHunt.World.computePositionHints' but scoped to one
-- location at a time for use from the display hook.
positionHintFor :: GameWorld -> Location -> PositionHint
positionHintFor world loc =
  let lg     = worldLocationGraph world
      regs   = lgRegions lg
      myReg  = Map.lookup loc regs
      pairs  = Set.toList (lgEdges lg)
      ns l   = [ b | (a, b) <- pairs, a == l ]
            ++ [ a | (a, b) <- pairs, b == l ]
      cross l = any (\n -> Map.lookup n regs /= Map.lookup l regs) (ns l)
      isBridge = cross loc
      isEdge   = any cross (ns loc)
  in case (myReg, isBridge, isEdge) of
       (_, True, _) -> Bridge
       (_, _, True) -> Edge
       _            -> Interior

-- | Tint a neighbor label by the biome it leads into.  Looks up the
-- destination's region and hands off to the palette's zone-tint table.
-- Falls back to no tint if the location is absent from the graph or the
-- region has no default color.
deerHuntZoneTint :: GameWorld -> Location -> Maybe Color
deerHuntZoneTint world loc =
  case Map.lookup loc (lgRegions (worldLocationGraph world)) of
    Just (Region name) -> zoneTintDefault name
    Nothing            -> Nothing

-- ---------------------------------------------------------------------------
-- Shiny-sense sparkle
-- ---------------------------------------------------------------------------

locationSparkle :: GameWorld -> CharId -> Location -> Int
locationSparkle world you loc =
  let exp'     = experience you world
      directEv = discoveredEvidence world loc
      adjEv    = maximum (0 : [ discoveredEvidence world n
                              | n <- neighborsFromGraph world loc ])
      expTier :: Int
      expTier
        | exp' <= 2 = 0
        | exp' <= 5 = 1
        | otherwise = 2
      -- Direct discovered sign dominates; adjacent contributes at
      -- most +1 and only for experienced readers.
      base
        | directEv >= 4 = 3
        | directEv >= 2 = 2
        | directEv >= 1 = 1
        | adjEv    >= 3 && expTier >= 2 = 1
        | otherwise     = 0
      noise = locationNoise world loc expTier
      signal
        | base > 0 = base
        | otherwise = noise
  in max 0 (min 3 signal)
  where
    locationNoise w (Location name) tier =
      let tick = lcTick (worldClock w)
          salt = foldl (\acc c -> acc * 131 + fromEnum c) 7 name
          r    = (tick * 1103515245 + salt) `mod` 1000
          thresh = case tier of
            0 -> 100
            1 -> 60
            _ -> 30
      in if r < thresh then 1 else 0

-- | Neighbor locations as stored in the world's 'LocationGraph'.  Used
-- by the sparkle's adjacency-bleed heuristic.
neighborsFromGraph :: GameWorld -> Location -> [Location]
neighborsFromGraph world loc =
  let pairs = Set.toList (lgEdges (worldLocationGraph world))
  in [ b | (a, b) <- pairs, a == loc ]
     ++ [ a | (a, b) <- pairs, b == loc ]

endScreen :: GameWorld -> [String]
endScreen w
  | checkCondition w (HasWorldTag hunterShot) =
      [ ""
      , bold "  You shot a man."
      , ""
      , grey "  You hear him before you see what happened. Then you're running."
      , grey "  The rifle is still in your hands. You don't remember dropping the bolt."
      , ""
      , dim  "  The rest of it — the phone call, the ambulance, the questions —"
      , dim  "  doesn't feel like something that's happening to you."
      , ""
      ]
  -- The three positive paths: you got the deer, you got the thing,
  -- you got both.  They're ordered "both > deer > treasure" so the
  -- richer variant always wins the guard match.
  | killed && found =
      [ ""
      , bold "  The best day of the season."
      , ""
      , grey "  Meat in the freezer and a story you didn't go out looking for."
      , grey "  " <> signatureLineFor w
      , ""
      , dim  "  You'll be telling this one for a while."
      , ""
      ]
  | killed =
      [ ""
      , bold "  Clean kill."
      , ""
      , grey "  The buck went down fast. You walk up to it in the stubble"
      , grey "  and stand there for a minute before doing anything."
      , grey "  Steam rising off the body in the cold air."
      , ""
      , dim  "  Meat in the freezer. That's a good fall."
      , ""
      ]
  | found =
      [ ""
      , bold "  No deer this year."
      , ""
      , grey "  You came back with something else."
      , grey "  " <> signatureLineFor w
      , ""
      , dim  "  Some seasons are like that. You take what the woods give you."
      , ""
      ]
  -- Neither.  Existing misses still differentiate by how the deer
  -- slipped the hunter — shot and lost vs. never seen.
  | checkCondition w (HasWorldTag deerGone) =
      [ ""
      , bold "  Missed."
      , ""
      , grey "  The crack echoes off the ridge and then it's quiet."
      , grey "  You cycle the bolt but there's nothing to shoot at."
      , grey "  The buck is off the section and gone."
      , ""
      , dim  "  You walk back to the truck in the last of the light."
      , dim  "  The thermos is still warm."
      , ""
      ]
  | otherwise =
      [ ""
      , grey "  The hunt is over."
      , ""
      ]
  where
    killed = checkCondition w (HasWorldTag deerKilled)
    found  = checkCondition w (HasWorldTag signatureFoundTag)

-- | A one-line archetype-flavored sentence for the end-screen when
-- the player brought home a signature.  Pulls the archetype out of
-- the init tag so the line reads specific ("the skull with the
-- clean hole") rather than generic.  Falls through to a safe
-- default if the tag is missing for any reason.
signatureLineFor :: GameWorld -> String
signatureLineFor w =
  case firstJust (map parseSignatureArchetypeTag (worldTagList w)) of
    Just arch -> case arch of
      SigAntler  -> "An antler. Not one you'll forget the shape of."
      SigCairn   -> "A cairn nobody but you will visit this winter."
      SigCarving -> "A name carved into a tree sixty Novembers ago."
      SigSkull   -> "A skull with a story you didn't get to hear."
    Nothing -> "Something from the woods, brought home."
