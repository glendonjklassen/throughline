{-# LANGUAGE DataKinds #-}
module Scenarios.Diner.Scenes.MayaOutside (mayaOutsideActions) where

import           Data.List.NonEmpty (NonEmpty(..))
import           Engine.Author.DSL
import           GameTypes
import           Scenarios.Diner.Constants

mayaOutsideActions :: CharId -> [AnyAction]
mayaOutsideActions mayaId =
  [ anyAction (leanAgainstWall mayaId)
  , anyAction (callBabysitter mayaId)
  ]

leanAgainstWall :: CharId -> Action 'Once
leanAgainstWall mayaId = onceAction (ActionId "maya:leanAgainstWall")
  "Lean against the wall and breathe."
  unconditional
  [ immediate (Narrate "The cold air is sharp after the warm grease-smell of the diner. Rain speckles your arms.")
  , immediate (think mayaId "I just need a minute.")
  ]

callBabysitter :: CharId -> Action 'Once
callBabysitter mayaId = onceAction (ActionId "maya:callBabysitter")
  "Call the babysitter."
  (All [HasWorldTag checkedOnKid, HasWorldTag (timeTag 5)])
  $ conversation mayaId (Named "babysitter")
      ( (mayaId, "Hey — sorry, I know it's early. How's Jamie?") :|
      [ (Named "babysitter", "She's fine, Maya. Fever broke around midnight. She's sleeping.")
      , (mayaId, "Okay. Okay, good. Thanks.")
      ])
  ++ [ immediate (think mayaId "She's okay. She's okay.")
     , modifyCharacterStatEffect mayaId (Capacity Strength) 1
     ]
