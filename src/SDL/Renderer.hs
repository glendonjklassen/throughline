{-# LANGUAGE OverloadedStrings #-}
-- | SDL2 text-grid renderer. Full-width layout: top status bar, scrolling
-- message history in the middle, action HUD at the bottom.
module SDL.Renderer
  ( SDLContext(..)
  , initSDL
  , freeSDL
  , renderWorldSDL
  , renderWorldFrame
  , clearSDL
  , presentSDL
  , gridCols
  , gridRows
  , drawHLine
  , topBarRows
  , marginLeft
  , chunksOf
  ) where

import           Control.Monad (when)
import           Data.IORef
import           Data.List (intercalate)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Foreign.C.Types (CInt)
import qualified SDL
import           SDL.FontContext
import           SDL.Palette
import           SDL.Text (stripAnsi, wrapWords)
import           SDL.Primitives (fillCellsAlpha)
import           SDL.SpatialHUD (SpatialHUD(..), HUDCell(..), TerrainSprite(..),
                                 TrailMark(..), layoutHUD, terrainSprites,
                                 trailMarks)
import           Engine.Core.World (playerLocationName, engineTimeStatus, exitBearings)
import           Engine.Core.NarrativeMessage
import           SDL.Layout (LayoutConfig(..))
import           SDL.Debug (learningModeLines)
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

-- | Render the world and present.
renderWorldSDL
  :: SDLContext
  -> LayoutConfig
  -> (GameWorld -> Maybe String)
  -> (Location -> Int)
  -> (Location -> Maybe Color)
  -> CharId
  -> GameWorld
  -> [AnyAction]
  -> IORef [NarrativeEntry]
  -> IORef DebugMode
  -> IORef [AxiomTrace]
  -> IO ()
renderWorldSDL ctx _layout statusLine sparkleFn zoneTintFn you world actions logRef debugRef traceRef = do
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

  -- Center location name
  let locCol = max marginLeft (fromIntegral (cols - length locName) `div` 2)
  renderText fc locName defaultText (locCol, 0)
  -- Time right-aligned
  case timeStr of
    "" -> pure ()
    _  -> renderText fc timeStr dimText (timeCol, 0)
  -- Center compass
  let compassCol = max marginLeft (fromIntegral (cols - length compassStr) `div` 2)
  renderText fc compassStr dimText (compassCol, 1)
  drawHLine fc cols 2

  -- -----------------------------------------------------------------------
  -- Bottom HUD — spatial layout when available, flat grid otherwise
  -- -----------------------------------------------------------------------
  let hud = layoutHUD you world actions cols
      hasSpatial = not (null (shSpatialCells hud))
      -- General (non-movement) action labels in columns
      genLabels    = shGeneralLabels hud
      maxGenLen    = if null genLabels then 0
                     else maximum (map length genLabels)
      genColW      = maxGenLen + 3
      genNumCols   = max 1 ((cols - 4) `div` max 1 genColW)
      genRows      = chunksOf genNumCols genLabels
      genRowCount  = length genRows
      -- HUD sizing
      spatialH     = if hasSpatial then shBoxHeight hud else 0
      -- separator + prompt + general rows + gap + spatial area + bottom pad
      hudRows      = 1 + 1 + genRowCount
                       + (if hasSpatial then 1 + spatialH else 0)
                       + 1
      hudStartRow  = fromIntegral (rows - hudRows)

  -- Draw HUD separator
  drawHLine fc cols hudStartRow
  -- Prompt
  renderText fc "What do you do?" defaultText (marginLeft, hudStartRow + 1)
  -- General actions (linear, at top of HUD)
  mapM_ (\(rowIdx, rowActions) -> do
    let y = hudStartRow + 2 + fromIntegral rowIdx
    mapM_ (\(colIdx, label) -> do
      let x = marginLeft + 1 + fromIntegral colIdx * fromIntegral genColW
      renderText fc label greyText (x, y)
      ) (zip [0 :: Int ..] rowActions)
    ) (zip [0 :: Int ..] genRows)

  -- Spatial area (movement actions positioned by direction, centered on screen)
  let spatialTopRow = hudStartRow + 2 + fromIntegral genRowCount
                        + (if hasSpatial then 1 else 0)
      spatialLeft   = fromIntegral ((cols - shBoxWidth hud) `div` 2)
  when hasSpatial $
    drawSpatialHUD fc sparkleFn zoneTintFn you world hud spatialLeft spatialTopRow

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
      visible      = takeLast historyRows dispLines
      faded        = ageFadeVisible visible

  mapM_ (\(idx, (color, line)) ->
    renderText fc (take (cols - 2) line) color (marginLeft, fromIntegral (historyTop + idx))
    ) (zip [0 :: Int ..] faded)

  -- Scenario status line (if any) — render in the top bar area
  case statusLine world of
    Just s  -> renderText fc s dimText (marginLeft, 1)
    Nothing -> pure ()

  presentSDL ctx

-- | Render the world to the back buffer WITHOUT presenting.
-- Used by the typewriter loop so it can add text on top before presenting.
renderWorldFrame
  :: SDLContext
  -> LayoutConfig
  -> (GameWorld -> Maybe String)
  -> (Location -> Int)
  -> (Location -> Maybe Color)
  -> CharId
  -> GameWorld
  -> [AnyAction]
  -> IORef [NarrativeEntry]
  -> IORef DebugMode
  -> IORef [AxiomTrace]
  -> IO ()
renderWorldFrame ctx _layout statusLine sparkleFn zoneTintFn you world actions logRef debugRef traceRef = do
  clearSDL ctx
  allMsgs   <- readIORef logRef
  debugMode <- readIORef debugRef
  traces    <- readIORef traceRef
  let cols    = gridCols ctx
      rows    = gridRows ctx
      fc      = sdlFont ctx
      contentW = cols - fromIntegral marginLeft * 2 - 8

  -- Top bar (centered)
  let locName      = fromMaybe "" (playerLocationName you world)
      timeStr      = fromMaybe "" (engineTimeStatus world)
      timeCol      = max (marginLeft + 1)
                       (fromIntegral cols - fromIntegral (length timeStr) - marginLeft)
      compassExits = [ (lbl, b) | (_, lbl, b) <- exitBearings you world ]
      compassStr   = case compassExits of
        [] -> ""
        es -> intercalate "  ·  " (map fst (sortExits es))
  let locCol = max marginLeft (fromIntegral (cols - length locName) `div` 2)
  renderText fc locName defaultText (locCol, 0)
  case timeStr of
    "" -> pure ()
    _  -> renderText fc timeStr dimText (timeCol, 0)
  let compassCol = max marginLeft (fromIntegral (cols - length compassStr) `div` 2)
  renderText fc compassStr dimText (compassCol, 1)
  drawHLine fc cols 2

  -- Bottom HUD
  let hud = layoutHUD you world actions cols
      hasSpatial = not (null (shSpatialCells hud))
      genLabels    = shGeneralLabels hud
      maxGenLen    = if null genLabels then 0
                     else maximum (map length genLabels)
      genColW      = maxGenLen + 3
      genNumCols   = max 1 ((cols - 4) `div` max 1 genColW)
      genRows      = chunksOf genNumCols genLabels
      genRowCount  = length genRows
      spatialH     = if hasSpatial then shBoxHeight hud else 0
      hudRows      = 1 + 1 + genRowCount
                       + (if hasSpatial then 1 + spatialH else 0)
                       + 1
      hudStartRow  = fromIntegral (rows - hudRows)

  drawHLine fc cols hudStartRow
  renderText fc "What do you do?" defaultText (marginLeft, hudStartRow + 1)
  mapM_ (\(rowIdx, rowActions) -> do
    let y = hudStartRow + 2 + fromIntegral rowIdx
    mapM_ (\(colIdx, label) -> do
      let x = marginLeft + 1 + fromIntegral colIdx * fromIntegral genColW
      renderText fc label greyText (x, y)
      ) (zip [0 :: Int ..] rowActions)
    ) (zip [0 :: Int ..] genRows)

  let spatialTopRow = hudStartRow + 2 + fromIntegral genRowCount
                        + (if hasSpatial then 1 else 0)
      spatialLeft   = fromIntegral ((cols - shBoxWidth hud) `div` 2)
  when hasSpatial $
    drawSpatialHUD fc sparkleFn zoneTintFn you world hud spatialLeft spatialTopRow

  -- Learning mode
  let learnLines = if debugMode == Learning
                     then learningModeLines traces you world
                     else []
      learnRowCount = length learnLines
      historyTop    = topBarRows + learnRowCount
  mapM_ (\(idx, line) ->
    renderText fc (stripAnsi line) dimText (marginLeft, fromIntegral (topBarRows + idx))
    ) (zip [0 :: Int ..] learnLines)

  -- Message history
  let historyRows  = fromIntegral hudStartRow - historyTop - 1
      allOrdered   = reverse allMsgs
      labelW       = maximum (0 : map (length . neTimeLabel) allOrdered)
      dispLines    = concatMap (fmtEntry contentW labelW) allOrdered
      visible      = takeLast historyRows dispLines
      faded        = ageFadeVisible visible
  mapM_ (\(idx, (color, line)) ->
    renderText fc (take (cols - 2) line) color (marginLeft, fromIntegral (historyTop + idx))
    ) (zip [0 :: Int ..] faded)

  case statusLine world of
    Just s  -> renderText fc s dimText (marginLeft, 1)
    Nothing -> pure ()

-- ---------------------------------------------------------------------------
-- History age-fade
-- ---------------------------------------------------------------------------

-- | Fade older visible history lines toward the background while
-- keeping the newest lines at full brightness. Input is the list of
-- visible (color, line) pairs in oldest-first order.
-- The top (oldest) line lands at ~0.55 of full tone; the bottom
-- (newest) keeps its full colour. The gradient is linear in between.
ageFadeVisible :: [(Color, String)] -> [(Color, String)]
ageFadeVisible xs =
  let n = length xs
      floorFade = 0.55 :: Double
      peakFade  = 1.0  :: Double
      factor i
        | n <= 1    = peakFade
        | otherwise =
            floorFade + (peakFade - floorFade)
              * fromIntegral i / fromIntegral (n - 1)
  in [ (ageFadeColor c (factor i), s)
     | (i, (c, s)) <- zip [0 :: Int ..] xs ]

-- ---------------------------------------------------------------------------
-- Sparkle overlay
-- ---------------------------------------------------------------------------

-- | Draw the spatial HUD box: terrain scatter, player marker, neighbor
-- labels (with optional halo + tint), sparkle hints, and trail marks.
-- Shared by the main and typewriter render paths.
drawSpatialHUD
  :: FontContext
  -> (Location -> Int)
  -> (Location -> Maybe Color)
  -> CharId
  -> GameWorld
  -> SpatialHUD
  -> CInt        -- ^ spatialLeft (col)
  -> CInt        -- ^ spatialTopRow (row)
  -> IO ()
drawSpatialHUD fc sparkleFn zoneTintFn you world hud spatialLeft spatialTopRow = do
  let sprites = terrainSprites you world (shBoxWidth hud) (shBoxHeight hud)
  mapM_ (\ts -> do
    let Color r g b a = tsColor ts
        -- Fold alpha into the color so renderText picks it up uniformly.
        effA = min a (round (fromIntegral a * tsAlpha ts :: Double))
    renderText fc (tsGlyph ts) (Color r g b effA)
      ( spatialLeft   + fromIntegral (tsCol ts)
      , spatialTopRow + fromIntegral (tsRow ts)
      )
    ) sprites
  -- Player marker.
  let (pcol, prow) = shPlayerMarker hud
      markerText = "@ You"
      markerCol  = max 0 (pcol - length markerText `div` 2)
  renderText fc markerText dimText
    (spatialLeft + fromIntegral markerCol, spatialTopRow + fromIntegral prow)
  -- Neighbor labels with familiarity tint + zone halo.  Cells come in
  -- from the layout with Nothing for both; we derive them here so the
  -- layout stays a pure positional computation.
  let enrich cell =
        let visits = case hudTarget cell of
              Just loc -> lookupVisits you loc world
              Nothing  -> 0
            tint   = Just (familiarityColor visits)
            halo   = hudTarget cell >>= zoneTintFn
        in cell { hudTint = tint, hudHalo = halo }
  mapM_ (\cell0 -> do
    let cell = enrich cell0
        row = spatialTopRow + fromIntegral (hudRow cell)
        col = spatialLeft   + fromIntegral (hudCol cell)
        fg  = fromMaybe greyText (hudTint cell)
    case hudHalo cell of
      Just halo -> fillCellsAlpha fc halo 1.0 (length (hudLabel cell)) 1
                     (fromIntegral col, fromIntegral row)
      Nothing   -> pure ()
    renderText fc (hudLabel cell) fg (col, row)
    ) (shSpatialCells hud)
  -- Sparkles.
  mapM_ (\(cell, level, glyph) -> do
    let row = spatialTopRow + fromIntegral (hudRow cell)
        col = spatialLeft   + fromIntegral (hudCol cell)
        gCol = max 0 (col - fromIntegral (length glyph) - 1)
    renderText fc glyph (sparkleColor level) (gCol, row)
    ) (cellSparkles sparkleFn (shSpatialCells hud))
  -- Trail marks: breadcrumbs at neighbor cells the player recently
  -- departed.  Age fades the alpha so the freshest step reads strongest
  -- and anything more than a few moves old dissolves into the scatter.
  mapM_ (\tm -> do
    let alpha = max 0.12 (1.0 - fromIntegral (tmAge tm) * 0.18 :: Double)
        Color r g b a = dimText
        effA = round (fromIntegral a * alpha :: Double)
    renderText fc (tmGlyph tm) (Color r g b effA)
      ( spatialLeft   + fromIntegral (tmCol tm)
      , spatialTopRow + fromIntegral (tmRow tm)
      )
    ) (trailMarks you world hud)

-- | Look up how many times a character has arrived at a given location.
-- Zero for never-visited (or for characters absent from the visits map).
lookupVisits :: CharId -> Location -> GameWorld -> Int
lookupVisits cid loc world =
  case Map.lookup cid (worldLocationVisits world) of
    Nothing -> 0
    Just m  -> Map.findWithDefault 0 loc m

-- | Compute sparkle glyphs for movement cells that have a target location
-- and a non-zero sparkle level. Returns each cell paired with its level
-- and the glyph string to render.
cellSparkles :: (Location -> Int) -> [HUDCell] -> [(HUDCell, Int, String)]
cellSparkles sparkle = foldr step []
  where
    step cell acc = case hudTarget cell of
      Just loc ->
        let lvl = sparkle loc
            g   = sparkleGlyph lvl
        in if lvl > 0 && not (null g) then (cell, lvl, g) : acc else acc
      Nothing -> acc

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
      raw     = msgLines tension (neMessage entry)
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
-- Narrator and effect lines pick up a tension-keyed gutter glyph so
-- calm ambient prose, alert moments, and peak-tension beats all carry
-- a different visual signature at the left margin.
msgLines :: Int -> NarrativeMessage -> [String]
msgLines _ (MsgSay _ sName _ lNames text) =
  [sName <> fmtListeners lNames <> ": " <> text]
msgLines _ (MsgThink _ text)     = ["~ " <> text]
msgLines t (MsgNarrate text)     = [tensionGlyph t <> " " <> text]
msgLines t (MsgEffect text)      = [tensionGlyph t <> " " <> text]
msgLines _ (MsgDialogue dls)     = map fmtDLine dls
  where fmtDLine (_, sName, _, lNames, text) =
          sName <> fmtListeners lNames <> ": " <> text

-- | Gutter glyph for narrator/effect lines, keyed by tension (0-10).
-- Mirrors the narrator colour progression so the visual weight rises
-- with the tension.  Peak tension gets a bang to pop in peripheral
-- vision.
tensionGlyph :: Int -> String
tensionGlyph t
  | t <= 2    = "\x00b7"   -- middle dot, calm
  | t <= 5    = ">"        -- alert
  | t <= 8    = "\x00bb"   -- double-angle, tense
  | otherwise = "!"        -- peak — shot taken, spooked, etc.

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
