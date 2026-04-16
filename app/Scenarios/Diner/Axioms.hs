module Scenarios.Diner.Axioms (allAxioms, dawnRule, weatherDesc, weatherEffect) where

import           Engine.Author.CommonAxioms (weatherNarrationAxiom,
                                             weatherInfluenceAxiom, moodDriftAxiom)
import           Engine.Author.DSL
import           GameTypes
import           Scenarios.Diner.Constants

-- | Weather at a late-night diner.
weatherDesc :: WeatherDesc -> String
weatherDesc (WeatherDesc "Rainy")    = "Rain streaks the windows. The parking lot is a sheet of reflected neon."
weatherDesc (WeatherDesc "Drizzle")  = "The rain has eased to a drizzle. You can hear it more than see it."
weatherDesc (WeatherDesc "Overcast") = "The rain has stopped. The sky is low and grey, pressing down."
weatherDesc (WeatherDesc "Clearing") = "The clouds are breaking up. Between them, a few pale stars."
weatherDesc w                        = "The weather has shifted. " <> weatherName w <> "."

-- | Rainy weather drains Charisma slightly — harder to be sociable
-- when you're damp and cold.
weatherEffect :: WeatherDesc -> [(StatType, Int)]
weatherEffect (WeatherDesc "Rainy") = [(Capacity Charisma, -1)]
weatherEffect _                     = []

-- | Dawn narration — when 6 AM arrives, signal the end.
-- Takes a per-player completion tag so merged scenarios don't terminate each other.
dawnRule :: Tag -> AxiomRule
dawnRule completionTag = AxiomRule
  { ruleId       = ScenarioAxiom "dawn"
  , rulePriority = 2
  , ruleTrigger  = WhenWorldTagAdded (timeTag 6)
  , ruleGuard    = unconditional
  , ruleTarget   = SpecificChar Truth
  , ruleEffects  = [ immediate (Narrate "Light is creeping into the sky. The diner's fluorescence loses its monopoly on the room. Morning.")
                   , immediate (AddWorldTag dawnArrived)
                   , immediate (AddWorldTag completionTag)
                   ]
  }

allAxioms :: CharId -> [Axiom]
allAxioms you =
  [ weatherNarrationAxiom weatherDesc
  , weatherInfluenceAxiom you weatherEffect
  , moodDriftAxiom        you [(Capacity Charisma, 5)]
  ]
