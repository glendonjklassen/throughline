module Scenarios.DeerHunt (deerHunt, deerHuntDisplay) where

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import           Engine.Core.Conditions (checkCondition)
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
       }

-- | The scenario's display hooks.  Unlike 'deerHunt' these can't close
-- over a 'HuntWorld' built from a seed because the SDL runtime doesn't
-- pass a seed in — the 'GameWorld' carries what we need instead.  The
-- display hooks consult 'worldLocationGraph' directly for region
-- lookups and 'worldSeed' plus adjacency math for sparkle propagation.
deerHuntDisplay :: ScenarioDisplay
deerHuntDisplay = ScenarioDisplay
  { sdEndScreen       = endScreen
  , sdStatusLine      = const Nothing
  , sdLayout          = defaultLayout
  , sdLocationSparkle = locationSparkle
  , sdZoneTintFor     = deerHuntZoneTint
  , sdSensoryFor      = deerHuntSensory
  , sdCatalog         = discoveryCatalog
  }

-- | Pick a fleeting sensory fragment for a neighbor label during the
-- incremental reveal.  Uses the location's terrain class plus an
-- Interior/Edge/Bridge approximation derived from whether any of its
-- graph neighbours cross a class boundary.
deerHuntSensory :: GameWorld -> Location -> Int -> Maybe String
deerHuntSensory world loc salt =
  case Map.lookup loc (lgRegions (worldLocationGraph world)) of
    Just (Region name) ->
      let cls  = regionClassHint name
          hint = positionHintFor world loc
      in Just (sensoryFragment cls hint salt)
    Nothing -> Nothing

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
  | checkCondition w (HasWorldTag deerKilled) =
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
