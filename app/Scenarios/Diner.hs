module Scenarios.Diner (diner, dinerDisplay) where

import           Engine.Core.Conditions (checkCondition)
import           SDL.Layout
import           SDL.Text
import           GameTypes
import           Scenarios.Diner.Axioms    (allAxioms, dawnRule)
import           Scenarios.Diner.Constants (initialWorld, visitor,
                                            visitorDawn,
                                            frankOpened, mayaOpened, settled)
import           Scenarios.Diner.Scenes    (dinerActions)

diner :: Int -> CharacterId -> Scenario
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
  , scenarioTombstoneGC  = Nothing
  }

dinerDisplay :: ScenarioDisplay
dinerDisplay = ScenarioDisplay
  { sdEndScreen       = endScreen
  , sdStatusLine      = const Nothing
  , sdLayout          = defaultLayout
  , sdLocationSparkle = \_ _ _ -> 0
  , sdZoneTintFor     = \_ _   -> Nothing
  , sdSensoryFor      = \_ _ _ -> Nothing
  , sdCatalog         = const []
  , sdDayLabel        = \n -> "Day " <> show n
  }

endScreen :: GameWorld -> [String]
endScreen w
  | connected && checkCondition w (HasWorldTag settled) =
      [ ""
      , ansiBold "  Morning."
      , ""
      , ansiGrey "  Frank pays his tab. Maya starts the opening prep."
      , ansiGrey "  You leave a good tip and step out into cool air."
      , ""
      , ansiDim  "  You don't feel fixed. But you feel less alone."
      , ""
      ]
  | connected =
      [ ""
      , ansiBold "  The night is over."
      , ""
      , ansiGrey "  You talked to people. Real people, with real weight."
      , ansiGrey "  The coffee wasn't great. But you'll remember it."
      , ""
      , ansiDim  "  Sometimes that's what a sleepless night is for."
      , ""
      ]
  | otherwise =
      [ ""
      , ansiBold "  Dawn."
      , ""
      , ansiGrey "  You finish your coffee and leave cash on the table."
      , ansiGrey "  The server waves. The man at the counter doesn't look up."
      , ""
      , ansiDim  "  You came here to not be alone. But you stayed in your head."
      , ""
      ]
  where
    connected = checkCondition w (HasWorldTag frankOpened)
             || checkCondition w (HasWorldTag mayaOpened)
