module Engine.Core.Axioms.System
  ( -- Core system axioms
    locationTransitionAxiom
  , dayAdvanceAxiom
  , calendarEventText
    -- Shared helpers
  , charIsSleeping
    -- Biological axioms (re-exported)
  , fatigueSystemAxiom
  , tirednessSystemAxiom
  , hungerSystemAxiom
  , hungerStateSystemAxiom
  , charHasFatigue
  , charHasAnyFatigue
  , charHasHungerState
    -- Social axioms (re-exported)
  , socialEnergyAxiom
  , socialEnergyStateAxiom
  , perceptionDriftAxiom
  , trustedCompanionThreshold
  , coLocated
  , charHasSocialEnergy
    -- Aggregate list
  , systemAxioms
  ) where

import           Data.Maybe          (fromMaybe)

import           Engine.Author.DSL
import           Engine.Core.World
import           GameTypes

import           Engine.Core.Axioms.Shared (charIsSleeping)
import           Engine.Core.Axioms.Biological
import           Engine.Core.Axioms.Social

-- ---------------------------------------------------------------------------
-- Core system axioms
-- ---------------------------------------------------------------------------

-- | Narrates any location transition that appears in the world diff.
-- Covers player-action-driven moves; axiom-driven moves (e.g. a shift
-- change axiom) are expected to author their own prose.
locationTransitionAxiom :: Axiom
locationTransitionAxiom = Axiom
  { axiomId       = SystemAxiom "locationTransition"
  , axiomPriority = 1
  , axiomEvaluate = \_world _actions diff ->
      [ immediate (Narrate (show cid <> " \8594 " <> locationName newLoc))
      | LocationDelta cid _oldLoc newLoc <- diffLocations diff
      ]
  }

-- | Advances DayNumber, DayOfWeek, LunarPhase, and Season when midnight ticks.
-- Narrates solstice/equinox events on season transitions and significant moon phases.
dayAdvanceAxiom :: Axiom
dayAdvanceAxiom = Axiom
  { axiomId       = SystemAxiom "dayAdvance"
  , axiomPriority = 1
  , axiomEvaluate = \world _actions diff ->
      if EngineTag (Clock (TimeOfDay 0)) `notElem` diffWorldTagsAdded diff then [] else
        let day       = fromMaybe 0 (getDayNumber world)
            newDay    = day + 1
            newDow    = newDay `mod` daysPerWeek
            newLunar  = newDay `mod` lunarCycleDays
            oldSeason = fromMaybe 0 (getSeason world)
            newSeason = (newDay `div` daysPerSeason) `mod` seasonsPerYear
        in [ immediate (AddWorldTag (dayNumberTag newDay))
           , immediate (AddWorldTag (dayOfWeekTag newDow))
           , immediate (AddWorldTag (lunarPhaseTag newLunar))
           ] ++
           ( if newSeason /= oldSeason
               then [ immediate (AddWorldTag (seasonTag newSeason))
                    , immediate (Narrate (calendarEventText oldSeason newSeason))
                    ]
               else []
           ) ++
           [ immediate (Narrate ("The " <> phase <> "."))
           | Just phase <- [lunarPhaseName newLunar]
           ]
  }
  where
    daysPerWeek    = 7
    lunarCycleDays = 29
    daysPerSeason  = 91
    seasonsPerYear = 4

calendarEventText :: Int -> Int -> String
calendarEventText 3 0 = "The vernal equinox. Winter gives way to spring."
calendarEventText 0 1 = "The summer solstice. The longest day of the year."
calendarEventText 1 2 = "The autumnal equinox. Summer gives way to autumn."
calendarEventText 2 3 = "The winter solstice. The longest night of the year."
calendarEventText _ _ = "The seasons turn."

-- ---------------------------------------------------------------------------
-- Aggregate list -- the public API for all system axioms
-- ---------------------------------------------------------------------------

systemAxioms :: [Axiom]
systemAxioms =
  [ locationTransitionAxiom
  , dayAdvanceAxiom
  , fatigueSystemAxiom
  , tirednessSystemAxiom
  , hungerSystemAxiom
  , hungerStateSystemAxiom
  , socialEnergyAxiom
  , socialEnergyStateAxiom
  , perceptionDriftAxiom
  ]
