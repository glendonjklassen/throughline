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
--
-- Supports both keyboard and pointer input:
--   keyboard — j/k to move between rows, h/l to adjust, enter to
--              save, escape to cancel.
--   pointer  — click a row to focus it; click the \'<\' or \'>\'
--              beside the value to step it; click the footer rows
--              to save or cancel.  Touches work the same way.
module SDL.SettingsMenu
  ( settingsMenu
  ) where

import           Data.Char          (isAsciiLower, isAsciiUpper, isDigit)
import           Data.IORef         (newIORef, readIORef, writeIORef)

import           SDL.ClickMap       (ClickMap, gridRect, gridRowRect, hitTest)
import           SDL.FontContext    (renderText)
import           SDL.InputHandler   (InputEvent(..), awaitInputSDL)
import           SDL.Palette        (defaultText, dimText, greyText, warningColor)
import           SDL.Renderer       (SDLContext(..), clearSDL, presentSDL)
import           SDL.Settings       (DisplayMode(..), Settings(..), ViewportPreset,
                                     allViewportPresets, loadSettings,
                                     saveSettings, viewportLabel)

-- | Internal event the dispatch loop reacts to.  Clicks and keys both
-- resolve to one of these; the loop then handles each symmetrically.
data MenuCmd
  = SelectRow !Int
  | AdjustRow !Int !Int    -- row, direction (-1 or +1)
  | Commit
  | Cancel
  | MoveBy !Int            -- relative row move (+1 / -1)
  deriving (Eq)

-- | Open the settings menu and loop until the player commits or
-- cancels.  On commit, persists to disk.  Returns the resulting
-- settings (committed or unchanged) so the caller can refresh any
-- live-editable in-memory state.
settingsMenu :: SDLContext -> IO Settings
settingsMenu ctx = do
  baseline <- loadSettings
  stateRef <- newIORef (baseline, 0 :: Int)
  loop baseline stateRef
  where
    loop baseline stateRef = do
      (s, sel) <- readIORef stateRef
      cm <- renderMenu ctx s sel
      me <- awaitInputSDL (sdlWindow ctx)
      case me of
        Nothing              -> pure baseline
        Just (KeyPress c)    -> handleCmd baseline stateRef (keyToCmd c sel)
        Just (ClickAt px py) -> case hitTest cm px py of
          Just ch -> handleCmd baseline stateRef (clickToCmd ch)
          Nothing -> loop baseline stateRef

    -- Click dispatches encode the command as a single character so
    -- the click-map shape stays flat: 'A'/'B'/... = adjust row N+,
    -- 'a'/'b'/... = adjust row N-, digits = select row, '.' = save,
    -- ',' = cancel.  This is internal; never seen by the player.
    clickToCmd ch
      | ch == '.'         = Commit
      | ch == ','         = Cancel
      | isDigit ch        = SelectRow (fromEnum ch - fromEnum '0')
      | isAsciiLower ch   = AdjustRow (fromEnum ch - fromEnum 'a') (-1)
      | isAsciiUpper ch   = AdjustRow (fromEnum ch - fromEnum 'A')   1
      | otherwise         = MoveBy 0   -- no-op

    keyToCmd c sel
      | c == '\x1B'          = Cancel
      | c == 'q'             = Cancel
      | c == '\n' || c == '\r' = Commit
      | c `elem` "jJ"        = MoveBy 1
      | c `elem` "kK"        = MoveBy (-1)
      | c `elem` "hH"        = AdjustRow sel (-1)
      | c `elem` "lL"        = AdjustRow sel 1
      | otherwise            = MoveBy 0

    handleCmd baseline stateRef cmd = case cmd of
      Cancel -> pure baseline
      Commit -> do
        (s, _) <- readIORef stateRef
        saveSettings s
        pure s
      SelectRow r -> do
        (s, _) <- readIORef stateRef
        writeIORef stateRef (s, wrap r)
        loop baseline stateRef
      AdjustRow r d -> do
        (s, sel) <- readIORef stateRef
        writeIORef stateRef (adjustRow r d s, sel)
        loop baseline stateRef
      MoveBy delta -> do
        (s, sel) <- readIORef stateRef
        writeIORef stateRef (s, wrap (sel + delta))
        loop baseline stateRef

    wrap n =
      let total = length rowLabels
      in ((n `mod` total) + total) `mod` total

-- ---------------------------------------------------------------------------
-- Row model
-- ---------------------------------------------------------------------------

rowLabels :: [String]
rowLabels =
  [ "Display mode"
  , "Viewport"
  , "Font scale"
  , "High contrast"
  , "Reveal speed"
  , "Master volume"
  , "Music volume"
  , "SFX volume"
  , "Shared folder"
  ]

valueOf :: Int -> Settings -> String
valueOf 0 s = case sDisplayMode s of
  Windowed   -> "windowed"
  Fullscreen -> "fullscreen"
valueOf 1 s = viewportLabel (sViewport s)
valueOf 2 s = pct (sFontScale s)
valueOf 3 s = if sHighContrast s then "on" else "off"
valueOf 4 s = pct (sRevealSpeed s)
valueOf 5 s = pct (sMasterVolume s)
valueOf 6 s = pct (sMusicVolume s)
valueOf 7 s = pct (sSfxVolume s)
valueOf 8 s = maybe "none" shortenPath (sSharedFolder s)
valueOf _ _ = ""

-- | Truncate a path to the last ~40 chars so it fits in the value
-- column.  Settings-menu cells are narrow; showing the tail of a
-- path is more informative than its head (usually the mount prefix).
shortenPath :: FilePath -> String
shortenPath p
  | length p <= 40 = p
  | otherwise      = "…" <> drop (length p - 39) p

-- | Apply a +1 or -1 adjustment to the row's backing field.  Float
-- rows step by 0.05; the display-mode and high-contrast toggles
-- ignore magnitude; the viewport cycles through the preset list.
-- Values are clamped to sane ranges so the menu can't produce a
-- settings file that renders the game unusable.
adjustRow :: Int -> Int -> Settings -> Settings
adjustRow 0 _ s = s { sDisplayMode = toggleDisplay (sDisplayMode s) }
adjustRow 1 d s = s { sViewport    = cycleViewport d (sViewport s) }
adjustRow 2 d s = s { sFontScale    = clamp 0.6 1.6 (sFontScale s + step d) }
adjustRow 3 _ s = s { sHighContrast = not (sHighContrast s) }
adjustRow 4 d s = s { sRevealSpeed  = clamp 0.25 3.0 (sRevealSpeed s + step d) }
adjustRow 5 d s = s { sMasterVolume = clamp 0.0  1.0 (sMasterVolume s + step d) }
adjustRow 6 d s = s { sMusicVolume  = clamp 0.0  1.0 (sMusicVolume s + step d) }
adjustRow 7 d s = s { sSfxVolume    = clamp 0.0  1.0 (sSfxVolume s + step d) }
adjustRow 8 d s = s { sSharedFolder = cycleSharedFolder d (sSharedFolder s) }
adjustRow _ _ s = s

-- | Cycle through a small preset list of common cloud-folder paths.
-- A player who wants something custom can edit 'settings.json'
-- directly; this keyboard-friendly cycler covers the common cases
-- (Dropbox, Google Drive, OneDrive, iCloud) without an in-game
-- text editor.
cycleSharedFolder :: Int -> Maybe FilePath -> Maybe FilePath
cycleSharedFolder dir cur =
  let presets =
        [ Nothing
        , Just "~/Dropbox/throughline"
        , Just "~/Google Drive/throughline"
        , Just "~/OneDrive/throughline"
        , Just "~/Library/Mobile Documents/com~apple~CloudDocs/throughline"
        , Just "~/Sync/throughline"
        ]
      idx   = length (takeWhile (/= cur) presets)
      idx'  = if idx >= length presets then 0 else idx
      total = length presets
      next  = ((idx' + dir) `mod` total + total) `mod` total
  in presets !! next

toggleDisplay :: DisplayMode -> DisplayMode
toggleDisplay Windowed   = Fullscreen
toggleDisplay Fullscreen = Windowed

-- | Cycle through the preset list by 'dir' (-1 or +1), wrapping at
-- either end.  Makes h/l feel like a dial instead of bumping into a
-- boundary at the first or last preset.
cycleViewport :: Int -> ViewportPreset -> ViewportPreset
cycleViewport dir cur =
  let presets = allViewportPresets
      idx     = length (takeWhile (/= cur) presets)
      total   = length presets
      next    = ((idx + dir) `mod` total + total) `mod` total
  in presets !! next

step :: Int -> Double
step d = fromIntegral d * 0.05

clamp :: Double -> Double -> Double -> Double
clamp lo hi = max lo . min hi

pct :: Double -> String
pct v = show (round (v * 100) :: Int) <> "%"

-- ---------------------------------------------------------------------------
-- Rendering
-- ---------------------------------------------------------------------------

renderMenu :: SDLContext -> Settings -> Int -> IO ClickMap
renderMenu ctx s sel = do
  clearSDL ctx
  let fc = sdlFont ctx
  renderText fc "— settings —"                       defaultText (3, 2)
  renderText fc ""                                   dimText     (3, 3)
  mapM_ (renderRow fc) (zip [0 :: Int ..] rowLabels)
  let footerRowStart = 5 + length rowLabels * 2 + 1 :: Int
      footerStart    = fromIntegral footerRowStart
  renderText fc "j / k       select row"             greyText (3, footerStart)
  renderText fc "h / l       adjust value (< / >)"   greyText (3, footerStart + 1)
  renderText fc "enter       save and close"         greyText (3, footerStart + 2)
  renderText fc "esc         cancel"                 greyText (3, footerStart + 3)
  renderText fc ""                                   dimText  (3, footerStart + 4)
  renderText fc "display / contrast take effect on next launch"
                                                     warningColor (3, footerStart + 5)
  presentSDL ctx
  -- Build click targets:
  --   * each label region selects the row (digit character)
  --   * the '<' glyph adjusts row -1 (lowercase letter)
  --   * the '>' glyph adjusts row +1 (uppercase letter)
  --   * footer "enter" row commits ('.'), "esc" row cancels (',')
  let selectChar i = toEnum (fromEnum '0' + i)
      minusChar  i = toEnum (fromEnum 'a' + i)
      plusChar   i = toEnum (fromEnum 'A' + i)
      rows = [ (i, 5 + i * 2) | i <- [0 .. length rowLabels - 1] ]
      selectRects = [ gridRect fc 3 r 23 1 (selectChar i) | (i, r) <- rows ]
      minusRects  = [ gridRect fc 26 r 2 1 (minusChar i)  | (i, r) <- rows ]
      plusRects   = [ gridRect fc 45 r 2 1 (plusChar i)   | (i, r) <- rows ]
      enterRect   = gridRowRect fc 0 (footerRowStart + 2) 80 '.'
      escRect     = gridRowRect fc 0 (footerRowStart + 3) 80 ','
  pure (selectRects <> minusRects <> plusRects <> [enterRect, escRect])
  where
    renderRow fc (i, label) = do
      let row       = fromIntegral (5 + i * 2)
          marker    = if i == sel then "> " else "  "
          labelCol  = defaultText
          valueCol  = if i == sel then defaultText else dimText
      renderText fc (marker <> label) labelCol (3, row)
      renderText fc "<"               greyText (26, row)
      renderText fc (valueOf i s)     valueCol (28, row)
      renderText fc ">"               greyText (45, row)