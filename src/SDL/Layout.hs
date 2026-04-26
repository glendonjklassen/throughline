-- | Per-scenario rendering hooks.  'ScenarioDisplay' lets a
-- scenario customize spatial-HUD slots and status-line cells;
-- 'defaultDisplay' supplies engine defaults that scenarios pass
-- through if they have no opinion.  'LayoutConfig' controls panel
-- sizing.
module SDL.Layout
  ( LayoutConfig (..)
  , defaultLayout
  , ScenarioDisplay (..)
  , defaultDisplay
  ) where

import GameTypes.Types (CharacterId, GameWorld, Location)
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
  , sdLocationSparkle :: GameWorld -> CharacterId -> Location -> Int
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
  , sdCatalog         :: GameWorld -> [String]
    -- ^ The journal's index tab: each element is one already-formatted
    -- diary paragraph written in the scenario's own voice (e.g.
    -- @"Thu, Nov 7 — a raven. Clever bird; pairs stay together for years."@).
    -- The overlay renders them with blank rows between as loose
    -- field-notebook prose — scenarios that don't keep a catalog
    -- return @[]@ and the tab shows a single hint line.
  , sdDayLabel        :: Int -> String
    -- ^ Human-readable label for a given 1-based day number.  The
    -- notebook uses this for day headers (e.g. "Thu, Nov 7" for
    -- DeerHunt; "Day 1" as a generic default).  Also used in the
    -- day-end transition overlay so the scenario controls the
    -- vocabulary of its passage of time.
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
  , sdDayLabel        = \n -> "Day " <> show n
  }
