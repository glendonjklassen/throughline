module Scenarios.TopBuy (topBuy, topBuyDisplay) where

import           Engine.Core.Conditions (checkCondition)
import           SDL.Layout
import           SDL.Text
import           GameTypes
import           Scenarios.TopBuy.Actions   (allActions)
import           Scenarios.TopBuy.Axioms    (allAxioms, smallAskRule,
                                             kyleAuditRule, earlyReportRule)
import           Scenarios.TopBuy.Constants (initialWorld, playerCleared,
                                             playerSuspended, reportedToKyle,
                                             coveredForBradley)

topBuy :: Int -> CharacterId -> Scenario
topBuy seed you = Scenario
  { scenarioName         = "Top Buy"
  , scenarioInitial      = initialWorld seed you
  , scenarioActions      = allActions you
  , scenarioAxioms       = allAxioms you
  , scenarioMergeAxioms  = []
  , scenarioRules        = [smallAskRule, kyleAuditRule, earlyReportRule]
  , scenarioMergeRules   = []
  , scenarioTerminal     = Any [HasWorldTag playerCleared, HasWorldTag playerSuspended]
  , scenarioDebugDefault = Off
  , scenarioPlayerCharId = you
  , scenarioTombstoneGC  = Nothing
  }

topBuyDisplay :: ScenarioDisplay
topBuyDisplay = ScenarioDisplay
  { sdEndScreen       = endScreen
  , sdStatusLine      = \_ _ -> Nothing
  , sdLayout          = defaultLayout
  , sdLocationSparkle = \_ _ _ -> 0
  , sdZoneTintFor     = \_ _   -> Nothing
  , sdSensoryFor      = \_ _ _ -> Nothing
  , sdCatalog         = const []
  , sdDayLabel        = \n -> "Day " <> show n
  }

endScreen :: GameWorld -> [String]
endScreen w
  | checkCondition w (HasWorldTag playerCleared) && checkCondition w (HasWorldTag reportedToKyle) =
      [ ""
      , ansiBold "  You did the right thing."
      , ""
      , ansiGrey "  You reported the discrepancy. Kyle already had his suspicions."
      , ansiGrey "  Bradley doesn't come in the next day. You keep your job."
      , ""
      , ansiDim  "  It doesn't feel like winning. But it feels like the truth."
      , ""
      ]
  | checkCondition w (HasWorldTag playerCleared) =
      [ ""
      , ansiBold "  You kept your head down."
      , ""
      , ansiGrey "  Kyle figured it out on his own. You're not in any trouble."
      , ansiGrey "  Bradley is gone. The floor feels quieter."
      , ""
      , ansiDim  "  You wonder if you should have said something sooner."
      , ""
      ]
  | checkCondition w (HasWorldTag playerSuspended) && checkCondition w (HasWorldTag coveredForBradley) =
      [ ""
      , ansiBold "  You covered for him."
      , ""
      , ansiGrey "  There's a return logged under your ID. Kyle puts you on leave."
      , ansiGrey "  You sit in your car for a long time."
      , ""
      , ansiDim  "  Bradley knew exactly what he was doing. You were the paper trail."
      , ""
      ]
  | checkCondition w (HasWorldTag playerSuspended) =
      [ ""
      , ansiBold "  You got caught in the middle."
      , ""
      , ansiGrey "  The numbers don't add up, and your name is on one of them."
      , ansiGrey "  Kyle says he believes you. But you're on leave pending review."
      , ""
      , ansiDim  "  You did what felt right at the time. That's all you had."
      , ""
      ]
  | otherwise =
      [ ""
      , ansiGrey "  Game over."
      , ""
      ]
