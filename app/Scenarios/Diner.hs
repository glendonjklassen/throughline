module Scenarios.Diner (diner, dinerDisplay) where

import           Engine.Core.Conditions (checkCondition)
import           Terminal.Layout
import           Terminal.ANSI
import           GameTypes
import           Scenarios.Diner.Axioms    (allAxioms, dawnRule)
import           Scenarios.Diner.Constants (initialWorld, visitor,
                                            visitorDawn,
                                            frankOpened, mayaOpened, settled)
import           Scenarios.Diner.Scenes    (dinerActions)

diner :: Int -> CharId -> Scenario
diner seed _you = Scenario
  { scenarioName         = "Late Night Diner"
  , scenarioInitial      = initialWorld seed
  , scenarioActions      = dinerActions visitor
  , scenarioAxioms       = allAxioms visitor
  , scenarioMergeAxioms  = []
  , scenarioRules        = [dawnRule visitorDawn]
  , scenarioMergeRules   = []
  , scenarioTerminal     = HasWorldTag visitorDawn
  , scenarioDebugDefault = Off
  , scenarioPlayerCharId = visitor
  }

dinerDisplay :: ScenarioDisplay
dinerDisplay = ScenarioDisplay
  { sdEndScreen  = endScreen
  , sdStatusLine = const Nothing
  , sdLayout     = defaultLayout
  }

endScreen :: GameWorld -> [String]
endScreen w
  | connected && checkCondition w (HasWorldTag settled) =
      [ ""
      , bold "  Morning."
      , ""
      , grey "  Frank pays his tab. Maya starts the opening prep."
      , grey "  You leave a good tip and step out into cool air."
      , ""
      , dim  "  You don't feel fixed. But you feel less alone."
      , ""
      ]
  | connected =
      [ ""
      , bold "  The night is over."
      , ""
      , grey "  You talked to people. Real people, with real weight."
      , grey "  The coffee wasn't great. But you'll remember it."
      , ""
      , dim  "  Sometimes that's what a sleepless night is for."
      , ""
      ]
  | otherwise =
      [ ""
      , bold "  Dawn."
      , ""
      , grey "  You finish your coffee and leave cash on the table."
      , grey "  The server waves. The man at the counter doesn't look up."
      , ""
      , dim  "  You came here to not be alone. But you stayed in your head."
      , ""
      ]
  where
    connected = checkCondition w (HasWorldTag frankOpened)
             || checkCondition w (HasWorldTag mayaOpened)
