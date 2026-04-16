{-# LANGUAGE OverloadedStrings #-}
-- | SDL2 text-grid renderer. Full-width layout: top status bar, scrolling
-- message history in the middle, action HUD at the bottom.
module SDL.Renderer
  ( SDLContext(..)
  , initSDL
  , freeSDL
  , renderWorldSDL
  , clearSDL
  , presentSDL
  , gridCols
  , gridRows
  , drawHLine
  , topBarRows
  , chunksOf
  ) where

import           Data.IORef
import           Data.List (intercalate)
import           Data.Maybe (fromMaybe)
import           Foreign.C.Types (CInt)
import qualified SDL
import           SDL.FontContext
import           SDL.Palette
import           Terminal.ANSI (stripAnsi, wrapWords)
import           Engine.Core.World (playerLocationName, engineTimeStatus, exitBearings)
import           Engine.Core.NarrativeMessage
import           Terminal.Layout (LayoutConfig(..))
import           Terminal.Debug (learningModeLines)
import           GameTypes

-- ---------------------------------------------------------------------------
-- Context
-- ---------------------------------------------------------------------------

data SDLContext = SDLContext
  { sdlWindow   :: SDL.Window
  , sdlRenderer :: SDL.Renderer
  , sdlFont     :: FontContext
  , sdlGridCols :: !CInt
  , sdlGridRows :: !CInt
  }

gridCols :: SDLContext -> Int
gridCols = fromIntegral . sdlGridCols

gridRows :: SDLContext -> Int
gridRows = fromIntegral . sdlGridRows

windowWidth, windowHeight :: CInt
windowWidth  = 1280
windowHeight = 800

fontSize :: Int
fontSize = 16

initSDL :: FilePath -> IO SDLContext
initSDL fontPath = do
  SDL.initializeAll
  window <- SDL.createWindow "Throughline"
    SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 windowWidth windowHeight }
  renderer <- SDL.createRenderer window (-1)
    SDL.defaultRenderer { SDL.rendererType = SDL.AcceleratedVSyncRenderer }
  fc <- initFont renderer fontPath fontSize
  let cols = windowWidth  `div` cellWidth fc
      rows = windowHeight `div` cellHeight fc
  pure SDLContext
    { sdlWindow   = window
    , sdlRenderer = renderer
    , sdlFont     = fc
    , sdlGridCols = cols
    , sdlGridRows = rows
    }

freeSDL :: SDLContext -> IO ()
freeSDL ctx = do
  freeFont (sdlFont ctx)
  SDL.destroyRenderer (sdlRenderer ctx)
  SDL.destroyWindow (sdlWindow ctx)
  SDL.quit

clearSDL :: SDLContext -> IO ()
clearSDL ctx = do
  let Color r g b a = bgColor
  SDL.rendererDrawColor (sdlRenderer ctx) SDL.$= SDL.V4 r g b a
  SDL.clear (sdlRenderer ctx)

presentSDL :: SDLContext -> IO ()
presentSDL ctx = SDL.present (sdlRenderer ctx)

-- | Draw a horizontal line of a repeated character across the full width.
drawHLine :: FontContext -> Int -> CInt -> IO ()
drawHLine fc cols row =
  renderText fc (replicate cols '-') separatorColor (0, row)

-- ---------------------------------------------------------------------------
-- Layout constants
-- ---------------------------------------------------------------------------

-- | Left margin for all content (columns).
marginLeft :: CInt
marginLeft = 2

-- | Top bar height (rows): status + compass + separator line.
topBarRows :: Int
topBarRows = 3


-- ---------------------------------------------------------------------------
-- World rendering — new layout
-- ---------------------------------------------------------------------------

renderWorldSDL
  :: SDLContext
  -> LayoutConfig
  -> (GameWorld -> Maybe String)
  -> CharId
  -> GameWorld
  -> [AnyAction]
  -> IORef [NarrativeEntry]
  -> IORef DebugMode
  -> IORef [AxiomTrace]
  -> IO ()
renderWorldSDL ctx _layout statusLine you world actions logRef debugRef traceRef = do
  clearSDL ctx
  allMsgs   <- readIORef logRef
  debugMode <- readIORef debugRef
  traces    <- readIORef traceRef
  let cols    = gridCols ctx
      rows    = gridRows ctx
      fc      = sdlFont ctx
      contentW = cols - fromIntegral marginLeft * 2 - 8  -- usable text width

  -- -----------------------------------------------------------------------
  -- Top bar (rows 0-2)
  -- -----------------------------------------------------------------------
  let locName      = fromMaybe "" (playerLocationName you world)
      timeStr      = fromMaybe "" (engineTimeStatus world)
      timeCol      = max (marginLeft + 1)
                       (fromIntegral cols - fromIntegral (length timeStr) - marginLeft)
      compassExits = [ (lbl, b) | (_, lbl, b) <- exitBearings you world ]
      compassStr   = case compassExits of
        [] -> ""
        es -> intercalate "  ·  " (map fst (sortExits es))

  renderText fc locName defaultText (marginLeft, 0)
  case timeStr of
    "" -> pure ()
    _  -> renderText fc timeStr dimText (timeCol, 0)
  renderText fc compassStr dimText (marginLeft, 1)
  drawHLine fc cols 2

  -- -----------------------------------------------------------------------
  -- Bottom HUD — compute first so we know how many rows history gets
  -- -----------------------------------------------------------------------
  let actionLabels = zipWith (\n a -> show n <> ") " <> stripAnsi (anyActionLabel a))
                       [1 :: Int ..] actions
      -- Lay out actions in columns: pick column count based on longest label
      maxLabelLen  = if null actionLabels then 0
                     else maximum (map length actionLabels)
      colWidth     = maxLabelLen + 3  -- padding between columns
      numCols      = max 1 (max 1 (cols - 4) `div` max 1 colWidth)
      actionRows   = chunksOf numCols actionLabels
      hudRows      = 1 + length actionRows + 1  -- separator + prompt gap + action rows + bottom pad
      hudStartRow  = fromIntegral (rows - hudRows)

  -- Draw HUD separator
  drawHLine fc cols hudStartRow
  -- Prompt
  renderText fc "What do you do?" defaultText (marginLeft, hudStartRow + 1)
  -- Action grid
  mapM_ (\(rowIdx, rowActions) -> do
    let y = hudStartRow + 2 + fromIntegral rowIdx
    mapM_ (\(colIdx, label) -> do
      let x = marginLeft + 1 + fromIntegral colIdx * fromIntegral colWidth
      renderText fc label greyText (x, y)
      ) (zip [0 :: Int ..] rowActions)
    ) (zip [0 :: Int ..] actionRows)

  -- -----------------------------------------------------------------------
  -- Learning mode (optional, between top bar and history)
  -- -----------------------------------------------------------------------
  let learnLines = if debugMode == Learning
                     then learningModeLines traces you world
                     else []
      learnRowCount = length learnLines
      historyTop    = topBarRows + learnRowCount

  mapM_ (\(idx, line) ->
    renderText fc (stripAnsi line) dimText (marginLeft, fromIntegral (topBarRows + idx))
    ) (zip [0 :: Int ..] learnLines)

  -- -----------------------------------------------------------------------
  -- Message history (fills the middle)
  -- -----------------------------------------------------------------------
  let historyRows  = fromIntegral hudStartRow - historyTop - 1
      -- Build display lines from messages (oldest first)
      allOrdered   = reverse allMsgs
      labelW       = maximum (0 : map (length . neTimeLabel) allOrdered)
      dispLines    = concatMap (fmtEntry contentW labelW) allOrdered
      -- Take the last N lines that fit
      visible      = takeLast historyRows dispLines

  mapM_ (\(idx, (color, line)) ->
    renderText fc (take (cols - 2) line) color (marginLeft, fromIntegral (historyTop + idx))
    ) (zip [0 :: Int ..] visible)

  -- Scenario status line (if any) — render in the top bar area
  case statusLine world of
    Just s  -> renderText fc s dimText (marginLeft, 1)
    Nothing -> pure ()

  presentSDL ctx

-- ---------------------------------------------------------------------------
-- Message formatting (SDL-native, no ANSI codes)
-- ---------------------------------------------------------------------------

-- | Format a single narrative entry into colored, word-wrapped lines.
-- Returns (Color, String) pairs ready for rendering.
fmtEntry :: Int -> Int -> NarrativeEntry -> [(Color, String)]
fmtEntry contentW labelW entry =
  let tension = neTension entry
      label   = neTimeLabel entry
      color   = msgColor (neMessage entry) tension
      raw     = msgLines (neMessage entry)
      wrapped = concatMap (wrapWords (max 10 (contentW - labelW - 2))) raw
      pad     = replicate (labelW + 2) ' '
      labelPad = padTo (labelW + 2) label
  in case wrapped of
       []     -> []
       (l:ls) -> (color, labelPad <> l) : map (\r -> (color, pad <> r)) ls

-- | Color for a message based on its type and tension.
msgColor :: NarrativeMessage -> Int -> Color
msgColor MsgSay {}       _ = dialogueColor
msgColor (MsgThink _ _)  _ = thoughtColor
msgColor (MsgNarrate _)  t = tensionColor t
msgColor (MsgEffect _)   t = tensionColor t
msgColor (MsgDialogue _) _ = dialogueColor

-- | Extract raw text lines from a message (before word-wrapping).
msgLines :: NarrativeMessage -> [String]
msgLines (MsgSay _ sName _ lNames text) =
  [sName <> fmtListeners lNames <> ": " <> text]
msgLines (MsgThink _ text)     = ["~ " <> text]
msgLines (MsgNarrate text)     = ["> " <> text]
msgLines (MsgEffect text)      = ["  " <> text]
msgLines (MsgDialogue dls)     = map fmtDLine dls
  where fmtDLine (_, sName, _, lNames, text) =
          sName <> fmtListeners lNames <> ": " <> text

fmtListeners :: [String] -> String
fmtListeners [] = ""
fmtListeners ns = " (to " <> intercalate ", " ns <> ")"

-- | Pad a string to N characters with trailing spaces.
padTo :: Int -> String -> String
padTo n s
  | length s >= n = s
  | otherwise     = s <> replicate (n - length s) ' '

-- | Sort exit labels clockwise by bearing.
sortExits :: [(String, Double)] -> [(String, Double)]
sortExits = insertionSort
  where
    insertionSort [] = []
    insertionSort (x:xs) = insert x (insertionSort xs)
    insert x [] = [x]
    insert x (y:ys)
      | snd x <= snd y = x : y : ys
      | otherwise       = y : insert x ys

-- | Split a list into chunks of N.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Re-export for Runner.
takeLast :: Int -> [a] -> [a]
takeLast n xs = drop (max 0 (length xs - n)) xs
