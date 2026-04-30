module Scenarios.DinerMaya (dinerMaya, dinerMayaDisplay) where

import           Engine.Core.Conditions (checkCondition)
import           SDL.Layout
import           SDL.Sprites    (indoorRegistry)
import           SDL.Text
import           GameTypes
import           Scenarios.Diner.MayaAxioms  (allAxiomsMaya, allRulesMaya)
import           Scenarios.Diner.Constants   (initialWorld, maya, mayaDawn,
                                              mayaOpened, checkedOnKid)
import           Scenarios.Diner.MayaScenes  (mayaActions)

dinerMaya :: Int -> CharacterId -> Scenario
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
  , scenarioTombstoneGC  = Nothing
  }

dinerMayaDisplay :: ScenarioDisplay
dinerMayaDisplay = ScenarioDisplay
  { sdEndScreen       = endScreen
  , sdStatusLine      = \_ _ -> Nothing
  , sdLayout          = defaultLayout
  , sdLocationSparkle = \_ _ _ -> 0
  , sdZoneTintFor     = \_ _   -> Nothing
  , sdSensoryFor      = \_ _ _ -> Nothing
  , sdCatalog         = const []
  , sdDayLabel        = \n -> "Day " <> show n
  , sdSession         = defaultSessionNoun
  , sdSprites         = indoorRegistry
  }

endScreen :: GameWorld -> [String]
endScreen w
  | checkCondition w (HasWorldTag mayaOpened) && checkCondition w (HasWorldTag checkedOnKid) =
      [ ""
      , ansiBold "  Morning."
      , ""
      , ansiGrey "  The first regulars start drifting in. You start a fresh pot."
      , ansiGrey "  Jamie's okay. And that person — they actually asked. Nobody asks."
      , ""
      , ansiDim  "  Sometimes a shift is just a shift. This one wasn't."
      , ""
      ]
  | checkCondition w (HasWorldTag mayaOpened) =
      [ ""
      , ansiBold "  Dawn."
      , ""
      , ansiGrey "  You wipe down the counter one last time."
      , ansiGrey "  They asked how you were doing. And for a second, you told the truth."
      , ""
      , ansiDim  "  That counts for something."
      , ""
      ]
  | otherwise =
      [ ""
      , ansiBold "  Morning."
      , ""
      , ansiGrey "  Another night. You cash out, wipe down, start the morning prep."
      , ansiGrey "  Someone came in around 2. Ordered a coffee. Left a decent tip."
      , ""
      , ansiDim  "  Four nights a week. You get used to it."
      , ""
      ]
