-- | Reusable scenario-level axiom builders for weather narration, stat influence, and mood drift.
module Engine.Author.CommonAxioms
  ( weatherNarrationAxiom
  , weatherInfluenceAxiom
  , moodDriftAxiom
  ) where

import           Engine.Author.DSL
import           Engine.Core.Conditions (getCharStat)
import           Engine.Core.World      (getWeather)
import           GameTypes

-- ---------------------------------------------------------------------------
-- Weather narration
-- ---------------------------------------------------------------------------

-- | Narrates a weather change whenever the active Weather tag changes.
-- The weatherDesc function is provided by the scenario so prose stays
-- grounded in the setting.
weatherNarrationAxiom :: (WeatherDesc -> String) -> Axiom
weatherNarrationAxiom weatherDesc = Axiom
  { axiomId       = ScenarioAxiom "weatherNarration"
  , axiomPriority = 1
  , axiomEvaluate = \_world _actions diff ->
      let newWeather = [ w | EngineTag (Weather w) <- diffWorldTagsAdded diff ]
      in case newWeather of
           [w] -> [immediate (Narrate (weatherDesc w))]
           _   -> []
  }

-- ---------------------------------------------------------------------------
-- Weather influence
-- ---------------------------------------------------------------------------

-- | Applies stat effects when the weather changes.
-- The scenario provides a function mapping weather to a list of
-- (stat, delta) pairs. Effects are immediate and fire once per change.
weatherInfluenceAxiom :: CharId -> (WeatherDesc -> [(StatType, Int)]) -> Axiom
weatherInfluenceAxiom cid influence = Axiom
  { axiomId       = ScenarioAxiom "weatherInfluence"
  , axiomPriority = 2
  , axiomEvaluate = \world _actions diff ->
      let changed = any isWeather (diffWorldTagsAdded diff)
      in if not changed then [] else
           case getWeather world of
             Nothing -> []
             Just w  -> map (uncurry (modifyCharacterStatEffect cid))
                            (influence w)
  }

-- ---------------------------------------------------------------------------
-- Mood drift toward baseline
-- ---------------------------------------------------------------------------

-- | Each hour, moves the given stats toward their baseline values by one step.
-- If the stat is already at baseline, no effect is emitted.
-- Use this to model emotions, tension, or alertness that settle over time.
moodDriftAxiom :: CharId -> [(StatType, Int)] -> Axiom
moodDriftAxiom cid baselines = Axiom
  { axiomId       = ScenarioAxiom "moodDrift"
  , axiomPriority = 8     -- after gameplay axioms, before tension
  , axiomEvaluate = \world _actions diff ->
      let hourTicked = any isTimeTag (diffWorldTagsAdded diff)
      in if not hourTicked then [] else
           concatMap (driftOne world) baselines
  }
  where
    driftOne world (stat, baseline) =
      case getCharStat cid stat world of
        Nothing  -> []
        Just cur
          | cur == baseline -> []
          | cur > baseline  -> [modifyCharacterStatEffect cid stat (-1)]
          | otherwise       -> [modifyCharacterStatEffect cid stat 1]
