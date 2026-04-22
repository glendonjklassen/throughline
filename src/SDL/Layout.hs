-- | Layout configuration: panel sizing and scenario display options.
module SDL.Layout
  ( LayoutConfig (..)
  , defaultLayout
  , ScenarioDisplay (..)
  , defaultDisplay
  ) where

import GameTypes.Types (CharId, GameWorld, Location)
import qualified SDL.Palette

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
  { sdEndScreen       :: GameWorld -> [String]
  , sdStatusLine      :: GameWorld -> Maybe String
  , sdLayout          :: LayoutConfig
  , sdLocationSparkle :: GameWorld -> CharId -> Location -> Int
    -- ^ "shiny-sense" level for a location, shown on the spatial HUD.
    -- 0 = no sparkle, 1 = faint hint, 2 = clear sign, 3 = strong pull.
    -- Scenarios use this to hint at deer presence or other points of
    -- interest based on world state, player experience, and noise.
  , sdZoneTintFor     :: GameWorld -> Location -> Maybe SDL.Palette.Color
    -- ^ Optional halo color for a neighbor label — e.g. the biome it
    -- leads into.  Returning 'Nothing' leaves the label untinted.
  , sdSensoryFor      :: GameWorld -> Location -> Int -> Maybe String
    -- ^ Optional fleeting one-liner rendered under a neighbor label
    -- during the incremental reveal animation.  The 'Int' is a
    -- per-arrival salt so repeated arrivals read differently.
    -- Returning 'Nothing' suppresses the sensory line.
  , sdCatalog         :: GameWorld -> [(String, [String])]
    -- ^ The journal's catalog tab, grouped as @[(categoryLabel,
    -- [entryName])]@.  Scenarios that don't use a discovery catalog
    -- return @[]@; the overlay collapses the tab to a one-line hint
    -- in that case.  Empty groups are dropped by the overlay before
    -- rendering, so scenarios can safely return all four categories.
  }

-- | Sensible defaults: no end screen, no status line, default layout,
-- no sparkle hints, no zone tinting.
defaultDisplay :: ScenarioDisplay
defaultDisplay = ScenarioDisplay
  { sdEndScreen       = const []
  , sdStatusLine      = const Nothing
  , sdLayout          = defaultLayout
  , sdLocationSparkle = \_ _ _ -> 0
  , sdZoneTintFor     = \_ _   -> Nothing
  , sdSensoryFor      = \_ _ _ -> Nothing
  , sdCatalog         = const []
  }
