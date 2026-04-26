{-# LANGUAGE DataKinds #-}
module Scenarios.TopBuy.HomeScene where

import           Engine.Author.DSL
import           GameTypes

homeActions :: CharacterId -> [AnyAction]
homeActions _you =
  [ anyAction sleepAction
  ]

-- ---------------------------------------------------------------------------
-- Off-shift — advances the clock one tick at a time
-- ---------------------------------------------------------------------------

sleepAction :: Action 'Repeatable
sleepAction = repeatableAction (ActionId "sleep")
  "It's quiet. Get some rest."
  unconditional
  [immediate DoNothing]
