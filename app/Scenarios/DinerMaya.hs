module Scenarios.DinerMaya (dinerMaya, dinerMayaDisplay) where

import           Engine.Core.Conditions (checkCondition)
import           SDL.Layout
import           SDL.Text
import           GameTypes
import           Scenarios.Diner.MayaAxioms  (allAxiomsMaya, allRulesMaya)
import           Scenarios.Diner.Constants   (initialWorld, maya, mayaDawn,
                                              mayaOpened, checkedOnKid)
import           Scenarios.Diner.MayaScenes  (mayaActions)

dinerMaya :: Int -> CharId -> Scenario
dinerMaya seed _you = Scenario
  { scenarioName         = "Late Night Diner: Maya"
  , scenarioInitial      = initialWorld seed
  , scenarioActions      = mayaActions maya
  , scenarioAxioms       = allAxiomsMaya maya
  , scenarioMergeAxioms  = []
  , scenarioRules        = allRulesMaya
  , scenarioMergeRules   = []
  , scenarioTerminal     = HasWorldTag mayaDawn
  , scenarioDebugDefault = Off
  , scenarioPlayerCharId = maya
  }

dinerMayaDisplay :: ScenarioDisplay
dinerMayaDisplay = ScenarioDisplay
  { sdEndScreen  = endScreen
  , sdStatusLine = const Nothing
  , sdLayout     = defaultLayout
  }

endScreen :: GameWorld -> [String]
endScreen w
  | checkCondition w (HasWorldTag mayaOpened) && checkCondition w (HasWorldTag checkedOnKid) =
      [ ""
      , bold "  Morning."
      , ""
      , grey "  The first regulars start drifting in. You start a fresh pot."
      , grey "  Jamie's okay. And that person — they actually asked. Nobody asks."
      , ""
      , dim  "  Sometimes a shift is just a shift. This one wasn't."
      , ""
      ]
  | checkCondition w (HasWorldTag mayaOpened) =
      [ ""
      , bold "  Dawn."
      , ""
      , grey "  You wipe down the counter one last time."
      , grey "  They asked how you were doing. And for a second, you told the truth."
      , ""
      , dim  "  That counts for something."
      , ""
      ]
  | otherwise =
      [ ""
      , bold "  Morning."
      , ""
      , grey "  Another night. You cash out, wipe down, start the morning prep."
      , grey "  Someone came in around 2. Ordered a coffee. Left a decent tip."
      , ""
      , dim  "  Four nights a week. You get used to it."
      , ""
      ]
