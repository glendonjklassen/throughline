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

topBuy :: Int -> CharId -> Scenario
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
  }

topBuyDisplay :: ScenarioDisplay
topBuyDisplay = ScenarioDisplay
  { sdEndScreen  = endScreen
  , sdStatusLine = const Nothing
  , sdLayout     = defaultLayout
  }

endScreen :: GameWorld -> [String]
endScreen w
  | checkCondition w (HasWorldTag playerCleared) && checkCondition w (HasWorldTag reportedToKyle) =
      [ ""
      , bold "  You did the right thing."
      , ""
      , grey "  You reported the discrepancy. Kyle already had his suspicions."
      , grey "  Bradley doesn't come in the next day. You keep your job."
      , ""
      , dim  "  It doesn't feel like winning. But it feels like the truth."
      , ""
      ]
  | checkCondition w (HasWorldTag playerCleared) =
      [ ""
      , bold "  You kept your head down."
      , ""
      , grey "  Kyle figured it out on his own. You're not in any trouble."
      , grey "  Bradley is gone. The floor feels quieter."
      , ""
      , dim  "  You wonder if you should have said something sooner."
      , ""
      ]
  | checkCondition w (HasWorldTag playerSuspended) && checkCondition w (HasWorldTag coveredForBradley) =
      [ ""
      , bold "  You covered for him."
      , ""
      , grey "  There's a return logged under your ID. Kyle puts you on leave."
      , grey "  You sit in your car for a long time."
      , ""
      , dim  "  Bradley knew exactly what he was doing. You were the paper trail."
      , ""
      ]
  | checkCondition w (HasWorldTag playerSuspended) =
      [ ""
      , bold "  You got caught in the middle."
      , ""
      , grey "  The numbers don't add up, and your name is on one of them."
      , grey "  Kyle says he believes you. But you're on leave pending review."
      , ""
      , dim  "  You did what felt right at the time. That's all you had."
      , ""
      ]
  | otherwise =
      [ ""
      , grey "  Game over."
      , ""
      ]
