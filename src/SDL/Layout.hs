-- | Layout configuration: panel sizing and scenario display options.
module SDL.Layout
  ( LayoutConfig (..)
  , defaultLayout
  , ScenarioDisplay (..)
  , defaultDisplay
  ) where

import GameTypes.Types (GameWorld)

data LayoutConfig = LayoutConfig
  { layoutLeftMaxWidth  :: Int  -- ^ hard cap on left panel width (columns)
  , layoutLeftPercent   :: Int  -- ^ left panel as a percentage of terminal width
  , layoutRightMinWidth :: Int  -- ^ minimum right panel width (columns)
  , layoutBottomMargin  :: Int  -- ^ rows reserved at the bottom of the screen
  }

defaultLayout :: LayoutConfig
defaultLayout = LayoutConfig
  { layoutLeftMaxWidth  = 93
  , layoutLeftPercent   = 68
  , layoutRightMinWidth = 10
  , layoutBottomMargin  = 2
  }

-- | Display configuration for a scenario, separate from the engine's Scenario type.
data ScenarioDisplay = ScenarioDisplay
  { sdEndScreen  :: GameWorld -> [String]
  , sdStatusLine :: GameWorld -> Maybe String
  , sdLayout     :: LayoutConfig
  }

-- | Sensible defaults: no end screen, no status line, default layout.
defaultDisplay :: ScenarioDisplay
defaultDisplay = ScenarioDisplay
  { sdEndScreen  = const []
  , sdStatusLine = const Nothing
  , sdLayout     = defaultLayout
  }
