module Scenarios.DeerHunt (deerHunt, deerHuntDisplay) where

import qualified Data.Map.Strict as Map

import           Engine.Core.Conditions (checkCondition)
import           SDL.Layout
import           SDL.Palette  (Color, zoneTintDefault)
import           SDL.Text
import           GameTypes
import           Scenarios.DeerHunt.Actions   (allActions)
import           Scenarios.DeerHunt.Axioms    (allAxioms, dawnRule,
                                               hunterArrivalMergeAxiom)
import           Scenarios.DeerHunt.Constants
import           Scenarios.DeerHunt.Locations  (adjacentTo, truckNorth)
import           Scenarios.DeerHunt.Probability (experience)

deerHunt :: Int -> CharId -> Scenario
deerHunt seed you = Scenario
  { scenarioName         = "Deer Hunt"
  , scenarioInitial      = initialWorld seed you truckNorth
  , scenarioActions      = allActions you
  , scenarioAxioms       = allAxioms you
  , scenarioMergeAxioms  = [hunterArrivalMergeAxiom you]
  , scenarioRules        = [dawnRule you]
  , scenarioMergeRules   = []
  , scenarioTerminal     = Any [HasWorldTag deerKilled, HasWorldTag hunterShot, HasWorldTag deerGone]
  , scenarioDebugDefault = Off
  , scenarioPlayerCharId = you
  }

deerHuntDisplay :: ScenarioDisplay
deerHuntDisplay = ScenarioDisplay
  { sdEndScreen       = endScreen
  , sdStatusLine      = const Nothing
  , sdLayout          = defaultLayout
  , sdLocationSparkle = locationSparkle
  , sdZoneTintFor     = deerHuntZoneTint
  }

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
--
-- For each location the player could move to, produce a sparkle level
-- 0-3 hinting at whether deer activity is likely there.  Signals used:
--
--   * Ground truth the player can *see*: if the deer is actually at
--     that location and the player is co-located, the sparkle is
--     capped — but in practice the action won't even appear as a
--     move target.  So we leave this as a weak seasoning.
--   * Player-discovered signs at that location — the main signal.
--     More types and rarer types raise the level.
--   * Adjacency echo: if a neighbour has discovered signs, bleed a
--     small faint hint through.
--   * Noise: low Understanding makes the sparkle unreliable — each
--     tick introduces false positives on a handful of locations and
--     may suppress a real one.  The noise is stable within a tick
--     (seeded by the world clock + location) so it doesn't strobe.
-- ---------------------------------------------------------------------------

locationSparkle :: GameWorld -> CharId -> Location -> Int
locationSparkle world you loc =
  let exp'     = experience you world
      directEv = discoveredEvidence world loc
      adjEv    = maximum (0 : [ discoveredEvidence world n
                              | n <- adjacentTo loc ])
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
    -- Deterministic per-tick noise for a location, keyed by name +
    -- tick so it shifts every tick but never strobes within one.
    locationNoise w (Location name) tier =
      let tick = lcTick (worldClock w)
          salt = foldl (\acc c -> acc * 131 + fromEnum c) 7 name
          r    = (tick * 1103515245 + salt) `mod` 1000
          -- Thresholds: tier 0 = 10%, tier 1 = 6%, tier 2 = 3%
          thresh = case tier of
            0 -> 100
            1 -> 60
            _ -> 30
      in if r < thresh then 1 else 0

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
