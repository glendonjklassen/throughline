{-# LANGUAGE DataKinds #-}
module Scenarios.Diner.Scenes.Booth (boothActions) where

import           Data.List.NonEmpty (NonEmpty(..))
import           Engine.Author.DSL
import           GameTypes
import           Scenarios.Diner.Constants

boothActions :: CharacterId -> [AnyAction]
boothActions you =
  [ anyAction (waitBooth you)
  , anyAction (orderCoffee you)
  , anyAction (lookAround you)
  , anyAction (thinkAboutSleep you)
  , anyAction (readGraffitiOnTable you)
  , anyAction (fourAM you)
  ]

waitBooth :: CharacterId -> Action 'Repeatable
waitBooth _you = repeatableAction (ActionId "waitBooth")
  "Sit with your thoughts."
  unconditional
  [immediate DoNothing]

orderCoffee :: CharacterId -> Action 'Once
orderCoffee you = onceAction (ActionId "orderCoffee")
  "Flag down the server for coffee."
  (Not (HasWorldTag orderedCoffee))
  $ conversation you maya
      ( (you,  "Could I get a coffee? Black.") :|
      [ (maya, "Coming right up.")
      ])
  ++ [ immediate (AddWorldTag orderedCoffee)
     , modifyTrust you maya 1
     ]

lookAround :: CharacterId -> Action 'Once
lookAround you = onceAction (ActionId "lookAround")
  "Take in the room."
  unconditional
  [ immediate (Narrate "Fluorescent light, the hum of the coffee machine, a country song you half-recognize from the speakers. There's a man at the counter — older, nursing something. The server moves behind the counter like she's done this ten thousand times.")
  , immediate (think you "It's the kind of place that doesn't try to be anything.")
  , modifyStat you (Capacity Understanding) 1
  ]

thinkAboutSleep :: CharacterId -> Action 'Once
thinkAboutSleep you = onceAction (ActionId "thinkAboutSleep")
  "Try to remember why you can't sleep."
  (HasWorldTag restless)
  [ immediate (think you "It's not insomnia. It's more like your apartment stopped feeling like a place you could be still in.")
  , immediate (Narrate "The rain taps against the window. You wrap your hands around the mug.")
  ]

readGraffitiOnTable :: CharacterId -> Action 'Once
readGraffitiOnTable you = onceAction (ActionId "readGraffitiOnTable")
  "Look at the marks on the table."
  (statAbove you (Capacity Understanding) 4)
  [ immediate (Narrate "Someone carved initials into the laminate — J + R, inside a lopsided heart. Below it, in different handwriting: \"we were here.\"")
  , immediate (think you "Everyone leaves a mark somewhere.")
  ]

fourAM :: CharacterId -> Action 'Once
fourAM you = onceAction (ActionId "fourAM")
  "Stare at the clock on the wall."
  (HasWorldTag (timeTag 4))
  [ immediate (Narrate "The clock reads 4:07. The deadest hour. Outside, even the street lights look tired.")
  , immediate (think you "This is the hour where everything either gets worse or starts to turn.")
  ]
