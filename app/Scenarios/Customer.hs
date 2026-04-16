module Scenarios.Customer (customer) where

import           Engine.Author.Scene            (buildActions)
import           GameTypes
import           Scenarios.Customer.Constants    (initialWorld)
import           Scenarios.Customer.WalkScene    (customerGraph)

customer :: Int -> CharId -> Scenario
customer seed you = Scenario
  { scenarioName         = "customer"
  , scenarioInitial      = initialWorld seed you
  , scenarioActions      = buildActions you customerGraph
  , scenarioAxioms       = []
  , scenarioMergeAxioms  = []
  , scenarioRules        = []
  , scenarioMergeRules   = []
  , scenarioTerminal     = Any []
  , scenarioDebugDefault = Off
  , scenarioPlayerCharId = you
  }
