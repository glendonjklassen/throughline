-- | Settings menu overlay for the launcher.
--
-- Renders a navigable list of rows.  Each row is an editable field
-- backed by 'Settings'.  The menu owns its own mutable copy of the
-- settings; committing writes it to disk via 'saveSettings'.  Canceling
-- discards the edits, so the on-disk file is never clobbered by a
-- player who accidentally opens the menu.
--
-- Display-mode and palette changes take effect on the next launch
-- (they affect 'initSDLWith'), which the footer reminds the player of.
-- Volumes and reveal-speed are live once the relevant subsystems read
-- them from disk at runtime.
module SDL.SettingsMenu
  ( settingsMenu
  ) where

import           Data.IORef         (newIORef, readIORef, writeIORef)

import           SDL.FontContext    (renderText)
import           SDL.InputHandler   (awaitKeySDL)
import           SDL.Palette        (defaultText, dimText, greyText, warningColor)
import           SDL.Renderer       (SDLContext(..), clearSDL, presentSDL)
import           SDL.Settings       (DisplayMode(..), Settings(..), loadSettings, saveSettings)

-- | Open the settings menu and loop until the player commits or
-- cancels.  On commit, persists to disk.  Returns the resulting
-- settings (committed or unchanged) so the caller can refresh any
-- live-editable in-memory state.
settingsMenu :: SDLContext -> IO Settings
settingsMenu ctx = do
  baseline <- loadSettings
  stateRef <- newIORef (baseline, 0 :: Int)   -- (current settings, selected row)
  loop baseline stateRef
  where
    loop baseline stateRef = do
      (s, sel) <- readIORef stateRef
      renderMenu ctx s sel
      mc <- awaitKeySDL
      case mc of
        Nothing                    -> pure baseline
        Just '\x1B'                -> pure baseline
        Just 'q'                   -> pure baseline
        Just '\n'                  -> commit s
        Just '\r'                  -> commit s
        Just c | c `elem` "jJ"     -> move stateRef (+ 1)    >> loop baseline stateRef
               | c `elem` "kK"     -> move stateRef (subtract 1) >> loop baseline stateRef
               | c `elem` "hH"     -> adjust stateRef (-1)    >> loop baseline stateRef
               | c `elem` "lL"     -> adjust stateRef 1       >> loop baseline stateRef
               | otherwise         -> loop baseline stateRef
    commit s = do
      saveSettings s
      pure s

    move stateRef f = do
      (s, sel) <- readIORef stateRef
      let sel' = wrap (f sel)
      writeIORef stateRef (s, sel')

    adjust stateRef d = do
      (s, sel) <- readIORef stateRef
      writeIORef stateRef (adjustRow sel d s, sel)

    wrap n =
      let total = length rowLabels
      in ((n `mod` total) + total) `mod` total

-- ---------------------------------------------------------------------------
-- Row model
-- ---------------------------------------------------------------------------
--
-- Each row is a (label, value-renderer, adjuster) triple.  Keeping the
-- list concrete means the menu stays readable — adding a field is one
-- entry here plus its 'Settings' field.

rowLabels :: [String]
rowLabels =
  [ "Display mode"
  , "Font scale"
  , "High contrast"
  , "Reveal speed"
  , "Master volume"
  , "Music volume"
  , "SFX volume"
  ]

-- | Render a value string for a given row of 'Settings'.
valueOf :: Int -> Settings -> String
valueOf 0 s = case sDisplayMode s of
  Windowed   -> "windowed"
  Fullscreen -> "fullscreen"
valueOf 1 s = pct (sFontScale s)
valueOf 2 s = if sHighContrast s then "on" else "off"
valueOf 3 s = pct (sRevealSpeed s)
valueOf 4 s = pct (sMasterVolume s)
valueOf 5 s = pct (sMusicVolume s)
valueOf 6 s = pct (sSfxVolume s)
valueOf _ _ = ""

-- | Apply a +1 or -1 adjustment to the row's backing field.  Float
-- rows step by 0.05; the display-mode and high-contrast toggles ignore
-- magnitude.  Values are clamped to sane ranges so the menu can't
-- produce a settings file that renders the game unusable.
adjustRow :: Int -> Int -> Settings -> Settings
adjustRow 0 _ s = s { sDisplayMode = toggleDisplay (sDisplayMode s) }
adjustRow 1 d s = s { sFontScale    = clamp 0.75 2.0  (sFontScale s    + step d) }
adjustRow 2 _ s = s { sHighContrast = not (sHighContrast s) }
adjustRow 3 d s = s { sRevealSpeed  = clamp 0.25 3.0  (sRevealSpeed s  + step d) }
adjustRow 4 d s = s { sMasterVolume = clamp 0.0  1.0  (sMasterVolume s + step d) }
adjustRow 5 d s = s { sMusicVolume  = clamp 0.0  1.0  (sMusicVolume s  + step d) }
adjustRow 6 d s = s { sSfxVolume    = clamp 0.0  1.0  (sSfxVolume s    + step d) }
adjustRow _ _ s = s

toggleDisplay :: DisplayMode -> DisplayMode
toggleDisplay Windowed   = Fullscreen
toggleDisplay Fullscreen = Windowed

step :: Int -> Double
step d = fromIntegral d * 0.05

clamp :: Double -> Double -> Double -> Double
clamp lo hi = max lo . min hi

pct :: Double -> String
pct v = show (round (v * 100) :: Int) <> "%"

-- ---------------------------------------------------------------------------
-- Rendering
-- ---------------------------------------------------------------------------

renderMenu :: SDLContext -> Settings -> Int -> IO ()
renderMenu ctx s sel = do
  clearSDL ctx
  let fc = sdlFont ctx
  renderText fc "— settings —"                       defaultText (3, 2)
  renderText fc ""                                   dimText     (3, 3)
  mapM_ (renderRow fc) (zip [0 :: Int ..] rowLabels)
  let footerStart = fromIntegral (5 + length rowLabels * 2 + 1)
  renderText fc "j / k       select row"             greyText (3, footerStart)
  renderText fc "h / l       adjust value"           greyText (3, footerStart + 1)
  renderText fc "enter       save and close"         greyText (3, footerStart + 2)
  renderText fc "esc         cancel"                 greyText (3, footerStart + 3)
  renderText fc ""                                   dimText  (3, footerStart + 4)
  renderText fc "display / contrast take effect on next launch"
                                                     warningColor (3, footerStart + 5)
  presentSDL ctx
  where
    renderRow fc (i, label) = do
      let row       = fromIntegral (5 + i * 2)
          marker    = if i == sel then "> " else "  "
          labelCol  = defaultText
          valueCol  = if i == sel then defaultText else dimText
      renderText fc (marker <> label) labelCol (3, row)
      renderText fc (valueOf i s)     valueCol (28, row)
