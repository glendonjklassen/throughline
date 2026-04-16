{-# LANGUAGE DataKinds #-}
module Scenarios.Diner.Scenes.Outside (outsideActions) where

import           Engine.Author.DSL
import           GameTypes
import           Scenarios.Diner.Constants

outsideActions :: CharId -> [AnyAction]
outsideActions you =
  [ anyAction (standInRain you)
  , anyAction (watchStreet you)
  , anyAction (watchStarsClearing you)
  ]

standInRain :: CharId -> Action 'Once
standInRain you = onceAction (ActionId "standInRain")
  "Stand in the rain for a moment."
  (HasWorldTag (weatherTag (WeatherDesc "Rainy")))
  [ immediate (Narrate "The rain is cold and real. It hits your face and your shoulders. For a second you're just a body in weather, not a person with thoughts.")
  , immediate (think you "This is the first thing that's felt right all day.")
  , immediate (RemoveWorldTag restless)
  , modifyCharacterStatEffect you (Capacity Strength) (-1)
  ]

watchStreet :: CharId -> Action 'Once
watchStreet you = onceAction (ActionId "watchStreet")
  "Watch the empty street."
  unconditional
  [ immediate (Narrate "A single car passes, headlights sweeping the wet road. The neon diner sign buzzes and flickers. Across the street, every window is dark.")
  , immediate (think you "The world is so quiet at this hour. Like it's taking a breath.")
  ]

watchStarsClearing :: CharId -> Action 'Once
watchStarsClearing you = onceAction (ActionId "watchStarsClearing")
  "Look up at the sky."
  (HasWorldTag (weatherTag (WeatherDesc "Clearing")))
  [ immediate (Narrate "The clouds have broken enough to see a few stars. Faint, half-drowned by the parking lot light, but there.")
  , immediate (think you "Still there. They're always still there.")
  ]
