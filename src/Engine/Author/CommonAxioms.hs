-- | Drop-in axioms that scenarios opt into rather than reinvent.
-- Each builder takes the scenario's parameters (prose tables,
-- thresholds) and returns an 'Axiom' to add to the scenario's axiom
-- list.  Skip a given axiom and the engine simply does not run the
-- corresponding behaviour — there is no global default.
module Engine.Author.CommonAxioms
  ( weatherNarrationAxiom
  , weatherInfluenceAxiom
  , moodDriftAxiom
  , timeOfDayNarrationAxiom
  ) where

import           Data.Maybe             (mapMaybe)

import           Engine.Author.DSL
import           Engine.Core.Conditions (getCharStat)
import           Engine.Core.Time       (TimePhase, timeOfDayPhase)
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
-- Time-of-day narration
-- ---------------------------------------------------------------------------

-- | Narrates a scenario-provided line when the clock crosses a
-- 'TimePhase' boundary (e.g. Dawn → Morning, GoldenHour → Dusk).
-- Scenarios supply a prose function over 'TimePhase'; returning
-- 'Nothing' for a phase suppresses the beat so authors can stay
-- silent on transitions that don't need calling out.
--
-- Triggers off 'diffWorldTagsAdded' for TimeOfDay tags — diff-driven,
-- not a point-in-time read — and fires exactly once per boundary
-- even when the hour advances several times in one tick.
timeOfDayNarrationAxiom :: (TimePhase -> Maybe String) -> Axiom
timeOfDayNarrationAxiom phaseProse = Axiom
  { axiomId       = ScenarioAxiom "timeOfDayNarration"
  , axiomPriority = 1
  , axiomEvaluate = \_world _actions diff ->
      let newHours = mapMaybe hourOfTag (diffWorldTagsAdded diff)
          transitions = [ timeOfDayPhase h
                        | h <- newHours
                        , timeOfDayPhase h /= timeOfDayPhase (h - 1)
                        ]
      in [ immediate (Narrate line)
         | phase <- transitions
         , Just line <- [phaseProse phase]
         ]
  }
  where
    hourOfTag (EngineTag (Clock (TimeOfDay h))) = Just h
    hourOfTag _                                 = Nothing

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
