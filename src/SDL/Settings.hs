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
  , defaultSettings
  , settingsPath
  , loadSettings
  , saveSettings
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

-- | All player-configurable settings in one record.
--
-- Kept small on purpose: new fields are cheap to add because 'FromJSON'
-- defaults each one.  Anything the renderer needs at runtime gets read
-- once from here rather than scattering reads across the codebase.
data Settings = Settings
  { sDisplayMode     :: DisplayMode
  , sFontScale       :: Double          -- ^ 1.0 = default; >1 bigger, <1 smaller
  , sHighContrast    :: Bool
  , sRevealSpeed     :: Double          -- ^ 1.0 = default; >1 faster per-cell fades
  , sMasterVolume    :: Double          -- ^ 0.0-1.0
  , sMusicVolume     :: Double
  , sSfxVolume       :: Double
  } deriving (Show, Eq)

defaultSettings :: Settings
defaultSettings = Settings
  { sDisplayMode   = Windowed
  , sFontScale     = 1.0
  , sHighContrast  = False
  , sRevealSpeed   = 1.0
  , sMasterVolume  = 0.8
  , sMusicVolume   = 0.6
  , sSfxVolume     = 0.8
  }

instance Aeson.ToJSON DisplayMode where
  toJSON Windowed   = Aeson.String "windowed"
  toJSON Fullscreen = Aeson.String "fullscreen"

instance Aeson.FromJSON DisplayMode where
  parseJSON = Aeson.withText "DisplayMode" $ \t -> case t of
    "windowed"   -> pure Windowed
    "fullscreen" -> pure Fullscreen
    other        -> fail ("unknown DisplayMode: " <> show other)

instance Aeson.ToJSON Settings where
  toJSON s = Aeson.object
    [ "displayMode"  .= sDisplayMode s
    , "fontScale"    .= sFontScale s
    , "highContrast" .= sHighContrast s
    , "revealSpeed"  .= sRevealSpeed s
    , "masterVolume" .= sMasterVolume s
    , "musicVolume"  .= sMusicVolume s
    , "sfxVolume"    .= sSfxVolume s
    ]

instance Aeson.FromJSON Settings where
  parseJSON = Aeson.withObject "Settings" $ \o -> Settings
    <$> o .:? "displayMode"  .!= sDisplayMode defaultSettings
    <*> o .:? "fontScale"    .!= sFontScale defaultSettings
    <*> o .:? "highContrast" .!= sHighContrast defaultSettings
    <*> o .:? "revealSpeed"  .!= sRevealSpeed defaultSettings
    <*> o .:? "masterVolume" .!= sMasterVolume defaultSettings
    <*> o .:? "musicVolume"  .!= sMusicVolume defaultSettings
    <*> o .:? "sfxVolume"    .!= sSfxVolume defaultSettings

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
