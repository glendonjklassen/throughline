-- | Player-facing settings: display, text, input, audio.
--
-- Persisted as JSON under the OS's standard config directory so the
-- file survives reinstalls and travels per-user, not per-install.  The
-- on-disk format is conservative — each field is optional on load and
-- defaults to the 'defaultSettings' value when absent, so a shipped
-- build can add fields without invalidating older saved configs.
{-# LANGUAGE OverloadedStrings #-}
module SDL.Settings
  ( Settings(..)
  , DisplayMode(..)
  , ViewportPreset(..)
  , defaultSettings
  , settingsPath
  , loadSettings
  , saveSettings
  , viewportSize
  , viewportRecommendedFontScale
  , allViewportPresets
  , viewportLabel
  ) where

import           Control.Exception      (SomeException, try)
import qualified Data.Aeson             as Aeson
import           Data.Aeson             ((.:?), (.!=), (.=))
import qualified Data.ByteString.Lazy   as BL
import           System.Directory       (XdgDirectory(..), createDirectoryIfMissing,
                                         doesFileExist, getXdgDirectory)
import           System.FilePath        ((</>), takeDirectory)

-- | How the game window presents itself at startup.
data DisplayMode = Windowed | Fullscreen
  deriving (Show, Eq)

-- | Canonical viewport resolutions.  Picked so the player gets sane
-- defaults for common display types without needing to type numbers.
-- 'ViewportDeck' is the Steam Deck native and remains the shipping
-- default so Deck verification isn't affected by a settings change.
-- 'ViewportLow' targets older / integrated-GPU machines.
data ViewportPreset
  = ViewportLow     -- 1024 × 640
  | ViewportDeck    -- 1280 × 800   (Steam Deck native)
  | Viewport1080p   -- 1920 × 1080
  | Viewport1440p   -- 2560 × 1440
  | Viewport4K      -- 3840 × 2160
  deriving (Show, Eq, Enum, Bounded)

-- | All player-configurable settings in one record.
--
-- Kept small on purpose: new fields are cheap to add because 'FromJSON'
-- defaults each one.  Anything the renderer needs at runtime gets read
-- once from here rather than scattering reads across the codebase.
data Settings = Settings
  { sDisplayMode     :: DisplayMode
  , sViewport        :: ViewportPreset
  , sFontScale       :: Double
    -- ^ User multiplier on top of the viewport's recommended default
    --   font scale.  1.0 means "whatever the viewport recommends";
    --   values > 1 bump text size larger, < 1 smaller.  This keeps
    --   text a consistent physical size across resolutions without
    --   forcing the user to re-tune every time they pick a new preset.
  , sHighContrast    :: Bool
  , sRevealSpeed     :: Double          -- ^ 1.0 = default; >1 faster per-cell fades
  , sMasterVolume    :: Double          -- ^ 0.0-1.0
  , sMusicVolume     :: Double
  , sSfxVolume       :: Double
  , sSharedFolder    :: Maybe FilePath
    -- ^ When set, the "text your friends" action in the journal
    --   copies a snapshot of the current log to
    --   @sharedFolder/<playerId>/events.jsonl@, and the scenario
    --   start reads every subdirectory there as a foreign log to
    --   merge into the world.  Absent = no broadcast / no merge.
  } deriving (Show, Eq)

defaultSettings :: Settings
defaultSettings = Settings
  { sDisplayMode   = Windowed
  , sViewport      = ViewportDeck
  , sFontScale     = 1.0
  , sHighContrast  = False
  , sRevealSpeed   = 1.0
  , sMasterVolume  = 0.8
  , sMusicVolume   = 0.6
  , sSfxVolume     = 0.8
  , sSharedFolder  = Nothing
  }

instance Aeson.ToJSON DisplayMode where
  toJSON Windowed   = Aeson.String "windowed"
  toJSON Fullscreen = Aeson.String "fullscreen"

instance Aeson.FromJSON DisplayMode where
  parseJSON = Aeson.withText "DisplayMode" $ \t -> case t of
    "windowed"   -> pure Windowed
    "fullscreen" -> pure Fullscreen
    other        -> fail ("unknown DisplayMode: " <> show other)

instance Aeson.ToJSON ViewportPreset where
  toJSON p = Aeson.String $ case p of
    ViewportLow   -> "low"
    ViewportDeck  -> "deck"
    Viewport1080p -> "1080p"
    Viewport1440p -> "1440p"
    Viewport4K    -> "4k"

instance Aeson.FromJSON ViewportPreset where
  parseJSON = Aeson.withText "ViewportPreset" $ \t -> case t of
    "low"   -> pure ViewportLow
    "deck"  -> pure ViewportDeck
    "1080p" -> pure Viewport1080p
    "1440p" -> pure Viewport1440p
    "4k"    -> pure Viewport4K
    other   -> fail ("unknown ViewportPreset: " <> show other)

instance Aeson.ToJSON Settings where
  toJSON s = Aeson.object
    [ "displayMode"  .= sDisplayMode s
    , "viewport"     .= sViewport s
    , "fontScale"    .= sFontScale s
    , "highContrast" .= sHighContrast s
    , "revealSpeed"  .= sRevealSpeed s
    , "masterVolume" .= sMasterVolume s
    , "musicVolume"  .= sMusicVolume s
    , "sfxVolume"    .= sSfxVolume s
    , "sharedFolder" .= sSharedFolder s
    ]

instance Aeson.FromJSON Settings where
  parseJSON = Aeson.withObject "Settings" $ \o -> Settings
    <$> o .:? "displayMode"  .!= sDisplayMode defaultSettings
    <*> o .:? "viewport"     .!= sViewport defaultSettings
    <*> o .:? "fontScale"    .!= sFontScale defaultSettings
    <*> o .:? "highContrast" .!= sHighContrast defaultSettings
    <*> o .:? "revealSpeed"  .!= sRevealSpeed defaultSettings
    <*> o .:? "masterVolume" .!= sMasterVolume defaultSettings
    <*> o .:? "musicVolume"  .!= sMusicVolume defaultSettings
    <*> o .:? "sfxVolume"    .!= sSfxVolume defaultSettings
    <*> o .:? "sharedFolder" .!= sSharedFolder defaultSettings

-- | Resolve the on-disk path for the settings file.  Uses the OS's
-- standard user-config location: @~\/.config\/throughline@ on Linux,
-- @%APPDATA%\/throughline@ on Windows, @~\/Library\/Application
-- Support\/throughline@ on macOS.  Caller is responsible for creating
-- the directory before writing.
settingsPath :: IO FilePath
settingsPath = do
  dir <- getXdgDirectory XdgConfig "throughline"
  pure (dir </> "settings.json")

-- | Load settings from disk, falling back to 'defaultSettings' on any
-- failure (file missing, parse error, IO error).  A player who hand-
-- edits the file into garbage gets defaults back rather than a crash.
loadSettings :: IO Settings
loadSettings = do
  path   <- settingsPath
  exists <- doesFileExist path
  if not exists
    then pure defaultSettings
    else do
      result <- try (BL.readFile path) :: IO (Either SomeException BL.ByteString)
      pure $ case result of
        Left  _   -> defaultSettings
        Right raw -> case Aeson.eitherDecode raw of
          Left  _ -> defaultSettings
          Right s -> s

-- | Persist settings to disk, creating the config directory if needed.
-- Any IO failure is propagated — the caller (a settings-menu commit
-- action) should surface it to the player.
saveSettings :: Settings -> IO ()
saveSettings s = do
  path <- settingsPath
  createDirectoryIfMissing True (takeDirectory path)
  BL.writeFile path (Aeson.encode s)

-- ---------------------------------------------------------------------------
-- Viewport presets
-- ---------------------------------------------------------------------------

-- | Window pixel dimensions for a viewport preset.
viewportSize :: ViewportPreset -> (Int, Int)
viewportSize ViewportLow   = (1024,  640)
viewportSize ViewportDeck  = (1280,  800)
viewportSize Viewport1080p = (1920, 1080)
viewportSize Viewport1440p = (2560, 1440)
viewportSize Viewport4K   = (3840, 2160)

-- | Font scale the viewport recommends so text reads at roughly the
-- same physical size regardless of resolution.  The baseline is Deck
-- (scale 1.0).  A 1080p screen has ~1.35× the vertical pixels; a 4K
-- screen has ~2.7×.  Without scaling, glyphs at a 4K monitor would
-- render about 37% of their Deck size — unreadable.  These numbers
-- are derived from the vertical ratio, rounded to sensible steps.
--
-- The user's 'sFontScale' multiplies on top of this; 1.0 means "let
-- the viewport decide," so picking a new preset never strands the
-- player at an unreadable text size.
viewportRecommendedFontScale :: ViewportPreset -> Double
viewportRecommendedFontScale ViewportLow   = 0.85
viewportRecommendedFontScale ViewportDeck  = 1.0
viewportRecommendedFontScale Viewport1080p = 1.35
viewportRecommendedFontScale Viewport1440p = 1.75
viewportRecommendedFontScale Viewport4K   = 2.7

-- | All presets in display order, for settings-menu cycling.
allViewportPresets :: [ViewportPreset]
allViewportPresets = [minBound .. maxBound]

-- | Short human-readable label for the settings menu.
viewportLabel :: ViewportPreset -> String
viewportLabel p =
  let (w, h) = viewportSize p
      name   = case p of
        ViewportLow   -> "low"
        ViewportDeck  -> "deck"
        Viewport1080p -> "1080p"
        Viewport1440p -> "1440p"
        Viewport4K   -> "4k"
  in name <> " (" <> show w <> "×" <> show h <> ")"
