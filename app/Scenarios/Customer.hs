module Scenarios.Customer (customer) where

import           Engine.Author.Scene            (compileSceneGraph)
import           GameTypes
import           Scenarios.Customer.Constants    (initialWorld)
import           Scenarios.Customer.WalkScene    (customerGraph)

customer :: Int -> CharacterId -> Scenario
customer seed you = Scenario
  { scenarioName         = "customer"
  , scenarioInitial      = initialWorld seed you
  , scenarioActions      = compileSceneGraph you customerGraph
  , scenarioAxioms       = []
  , scenarioMergeAxioms  = []
  , scenarioRules        = []
  , scenarioMergeRules   = []
  , scenarioTerminal     = Any []
  , scenarioDebugDefault = Off
  , scenarioPlayerCharId = you
  , scenarioTombstoneGC  = Nothing
  }
