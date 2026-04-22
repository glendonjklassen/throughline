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
  , RevealFrame(..)
  , finalReveal
  , hiddenReveal
  , sweepFeatherCh
  , renderJournalOverlay
  , JournalTab(..)
  ) where

import           Control.Monad (unless, void, when)
import           Data.IORef
import           Data.List (intercalate, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Maybe (fromMaybe)
import           Foreign.C.Types (CInt)
import qualified SDL
import           SDL.FontContext
import           SDL.Palette
import           SDL.Text (stripAnsi, wrapWords)
import           SDL.Primitives (drawCellUnderline)
import           SDL.Sprites    (drawSprite, spritesForClass)
import           SDL.SpatialHUD (SpatialHUD(..), HUDCell(..),
                                 TrailMark(..), SpritePlacement(..),
                                 layoutHUD, terrainSpriteScatter,
                                 trailMarks)
import           Engine.Core.World (playerLocationName, engineTimeStatus, exitBearings)
import           Engine.Core.NarrativeMessage
import           SDL.Layout (LayoutConfig(..), ScenarioDisplay(..))
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

-- | Fixed clockwise order of compass rose labels.  Rendered as a full
-- rose with present directions highlighted so the player always sees
-- the same eight-point silhouette regardless of how many exits this
-- location actually has.
compassRoseOrder :: [String]
compassRoseOrder = ["N","NE","E","SE","S","SW","W","NW"]

-- | Draw the compass rose on @row@, centered horizontally in @cols@.
-- Directions present in @exits@ render bright; absent ones dim to the
-- separator so the rose reads as a stable chrome element rather than
-- a list of labels.  Consecutive labels are joined by a thin middle
-- dot rendered in the separator color regardless.
drawCompassRose :: FontContext -> Int -> CInt -> [String] -> IO ()
drawCompassRose fc cols row exits =
  let present  = Set.fromList exits
      sep      = "  ·  "
      sepLen   = fromIntegral (length sep)
      totalW   = sum (map length compassRoseOrder)
               + length sep * (length compassRoseOrder - 1)
      startCol = max marginLeft (fromIntegral (cols - totalW) `div` 2)
      drawLabel col lbl = do
        let color = if Set.member lbl present then greyText else separatorColor
        renderText fc lbl color (col, row)
        pure (col + fromIntegral (length lbl))
      draw _   []         = pure ()
      draw col [lbl]      = void (drawLabel col lbl)
      draw col (lbl:rest) = do
        col' <- drawLabel col lbl
        renderText fc sep separatorColor (col', row)
        draw (col' + sepLen) rest
  in draw startCol compassRoseOrder

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
  -> RevealFrame
  -> CharId
  -> GameWorld
  -> [AnyAction]
  -> IORef [NarrativeEntry]
  -> IORef DebugMode
  -> IORef [AxiomTrace]
  -> IO ()
renderWorldSDL ctx _layout statusLine sparkleFn zoneTintFn frame you world actions logRef debugRef traceRef = do
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
      compassDirs  = [ lbl | (_, lbl, _) <- exitBearings you world ]

  -- Center location name
  let locCol = max marginLeft (fromIntegral (cols - length locName) `div` 2)
  renderText fc locName defaultText (locCol, 0)
  -- Time right-aligned
  case timeStr of
    "" -> pure ()
    _  -> renderText fc timeStr dimText (timeCol, 0)
  -- Centered compass rose
  drawCompassRose fc cols 1 compassDirs
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
    drawSpatialHUD fc sparkleFn zoneTintFn frame
                   you world hud spatialLeft spatialTopRow

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
  -> RevealFrame
  -> CharId
  -> GameWorld
  -> [AnyAction]
  -> IORef [NarrativeEntry]
  -> IORef DebugMode
  -> IORef [AxiomTrace]
  -> IO ()
renderWorldFrame ctx _layout statusLine sparkleFn zoneTintFn frame you world actions logRef debugRef traceRef = do
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
      compassDirs  = [ lbl | (_, lbl, _) <- exitBearings you world ]
  let locCol = max marginLeft (fromIntegral (cols - length locName) `div` 2)
  renderText fc locName defaultText (locCol, 0)
  case timeStr of
    "" -> pure ()
    _  -> renderText fc timeStr dimText (timeCol, 0)
  drawCompassRose fc cols 1 compassDirs
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
    drawSpatialHUD fc sparkleFn zoneTintFn frame
                   you world hud spatialLeft spatialTopRow

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

-- | A rendering-time snapshot of the spatial HUD reveal animation.
-- Each neighbor cell has a 0-1 alpha; zero or more cells have an
-- active sensory fragment.  Each sensory is rendered character by
-- character at its own alpha, driven by a light-sweep across the
-- fragment: during fade-in a beam enters from the left and
-- illuminates rightward; during fade-out a shadow enters from the
-- left and darkens rightward.  Multiple cells animate on their own
-- independent timelines.
data RevealFrame = RevealFrame
  { rfCellAlpha   :: HUDCell -> Double
  , rfActiveSenses :: [(HUDCell, Double, Bool, String)]
    -- ^ (cell, sweepPos, darkening, fragment) — sweepPos is in
    -- character units along the fragment; @darkening@ flips the
    -- sweep from "lighting up behind it" to "going dark behind it".
  }

-- | The 'RevealFrame' representing a completed reveal: every cell at
-- full alpha, no active sensory fragment.
finalReveal :: RevealFrame
finalReveal = RevealFrame
  { rfCellAlpha    = const 1.0
  , rfActiveSenses = []
  }

-- | The 'RevealFrame' for the pre-reveal state (all cells hidden).
-- Used during the typewriter path so the HUD shows terrain + player
-- marker but no choices while new prose is still typing in.
hiddenReveal :: RevealFrame
hiddenReveal = RevealFrame
  { rfCellAlpha    = const 0.0
  , rfActiveSenses = []
  }

-- | Draw the spatial HUD box: terrain scatter, player marker, neighbor
-- labels (with optional halo + tint), sparkle hints, and trail marks.
-- Shared by the main and typewriter render paths.
--
-- The 'RevealFrame' controls per-cell visibility for the incremental
-- reveal animation.  Passing 'finalReveal' renders every cell at full
-- opacity (the interactive state); 'hiddenReveal' renders none of them
-- (the typewriter state, where prose is still arriving and labels
-- shouldn't be stealing attention).
drawSpatialHUD
  :: FontContext
  -> (Location -> Int)
  -> (Location -> Maybe Color)
  -> RevealFrame
  -> CharId
  -> GameWorld
  -> SpatialHUD
  -> CInt        -- ^ spatialLeft (col)
  -> CInt        -- ^ spatialTopRow (row)
  -> IO ()
drawSpatialHUD fc sparkleFn zoneTintFn frame you world hud spatialLeft spatialTopRow = do
  -- Precompute each neighbor label's pixel bounding box so the terrain
  -- scatter can avoid them.  Boxes are generous (padded ±6 px) so
  -- sprites never crowd the text.
  let sortedCells = sortOn hudDist (shSpatialCells hud)
      -- Reserve space for *every* cell regardless of current reveal
      -- alpha.  If we filtered by alpha here, the exclusion set would
      -- grow as cells fade in, re-pick different positions from the
      -- seeded stream, and the ground scatter would appear to reset
      -- between repaints.  Scatter must be a function of (location,
      -- panel geom, full cell set) only.
      labelBBoxes =
        [ labelBBoxPx fc spatialLeft spatialTopRow c | c <- sortedCells ]
      panelOriginX :: Int
      panelOriginX = fromIntegral (spatialLeft  * cellWidth  fc)
      panelOriginY :: Int
      panelOriginY = fromIntegral (spatialTopRow * cellHeight fc)
      panelPxW     = shBoxWidth  hud * fromIntegral (cellWidth fc)
      panelPxH     = shBoxHeight hud * fromIntegral (cellHeight fc)
      scatter = terrainSpriteScatter you world
                  (panelOriginX, panelOriginY)
                  (panelPxW, panelPxH)
                  labelBBoxes
  mapM_ (\sp -> do
    let pool = spritesForClass (spClass sp)
    case pool of
      [] -> pure ()
      xs -> drawSprite fc
              (fromIntegral (spX sp), fromIntegral (spY sp))
              1.0
              (xs !! (spIndex sp `mod` length xs))
    ) scatter
  -- Player marker.
  let (pcol, prow) = shPlayerMarker hud
      markerText = "You"
      markerCol  = max 0 (pcol - length markerText `div` 2)
  renderText fc markerText dimText
    (spatialLeft + fromIntegral markerCol, spatialTopRow + fromIntegral prow)
  -- Render every cell whose alpha is >0.  The alpha multiplies into
  -- both label and halo so a fading-in cell carries its full visual
  -- signature together.
  let enrich cell =
        let visits = case hudTarget cell of
              Just loc -> lookupVisits you loc world
              Nothing  -> 0
            tint   = Just (familiarityColor visits)
            halo   = hudTarget cell >>= zoneTintFn
        in cell { hudTint = tint, hudHalo = halo }
      visibleCells = filter (\c -> rfCellAlpha frame c > 0.01) (shSpatialCells hud)
  mapM_ (\cell0 -> do
    let cell  = enrich cell0
        alpha = rfCellAlpha frame cell0
        -- Base pixel position from the grid coord, then nudge off-grid
        -- by a seeded per-label offset so the HUD doesn't look like a
        -- tidy table.  Horizontal nudge is larger; vertical stays
        -- small so labels still read on their row.
        baseCol = spatialLeft   + fromIntegral (hudCol cell)
        baseRow = spatialTopRow + fromIntegral (hudRow cell)
        (dxPx, dyPx) = labelPixelOffset fc (hudLabel cell)
        px = baseCol * cellWidth fc  + dxPx
        py = baseRow * cellHeight fc + dyPx
        fg = applyAlpha alpha (fromMaybe greyText (hudTint cell))
    renderTextAtPixel fc (hudLabel cell) fg (px, py)
    case hudHalo cell of
      Just halo -> drawCellUnderline fc (applyAlpha alpha halo)
                     (length (hudLabel cell))
                     (fromIntegral baseCol, fromIntegral baseRow)
      Nothing   -> pure ()
    ) visibleCells
  -- Active sensory line (if any) one row under its parent cell.
  -- Each character's alpha is determined by its position relative to
  -- the sweep: during fade-in, chars the beam has already passed read
  -- bright, chars still ahead of it stay dark; during fade-out, the
  -- relationship inverts.  The soft feather around the sweep gives
  -- the effect of a light glow rather than a hard edge.
  mapM_ (\(cell, sweep, darkening, fragment) ->
    unless (null fragment) $ do
      let baseRow = spatialTopRow + fromIntegral (hudRow cell) + 1
          baseCol = spatialLeft   + fromIntegral (hudCol cell)
          py      = baseRow * cellHeight fc
          baseX   = baseCol * cellWidth  fc
          cw      = cellWidth fc
      mapM_ (\(i, ch) -> do
        let a = charSweepAlpha i sweep darkening
        when (a > 0.01) $
          renderTextAtPixel fc [ch] (applyAlpha a thoughtColor)
            (baseX + fromIntegral i * cw, py)
        ) (zip [0 :: Int ..] fragment)
    ) (rfActiveSenses frame)
  -- Sparkles.  Only rendered for cells that have been revealed.
  mapM_ (\(cell, level, glyph) -> do
    let row = spatialTopRow + fromIntegral (hudRow cell)
        col = spatialLeft   + fromIntegral (hudCol cell)
        gCol = max 0 (col - fromIntegral (length glyph) - 1)
    renderText fc glyph (sparkleColor level) (gCol, row)
    ) (cellSparkles sparkleFn visibleCells)
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
    ) (trailMarks you world (hud { shSpatialCells = visibleCells }))

-- | Multiply a color's alpha channel by a 0-1 factor, clamping.  Used
-- to fade text and halo primitives as cells enter/leave the reveal.
applyAlpha :: Double -> Color -> Color
applyAlpha factor (Color r g b a) =
  let f = max 0.0 (min 1.0 factor)
      a' = round (fromIntegral a * f :: Double)
  in Color r g b a'

-- | Alpha contribution of a single character under a sweep at position
-- @sweep@ (in character units along the fragment).  The sweep moves
-- left-to-right; chars the sweep has already crossed (i < sweep) read
-- bright during fade-in and dark during fade-out.  The
-- @sweepFeatherCh@ window gives a soft, glow-like edge.
charSweepAlpha :: Int -> Double -> Bool -> Double
charSweepAlpha i sweep darkening =
  let lit = clamp01 ((sweep - fromIntegral i) / sweepFeatherCh + 0.5)
  in if darkening then 1.0 - lit else lit
  where
    clamp01 x = max 0.0 (min 1.0 x)

-- | Soft edge of the sweep, in character units.  Wider = gentler glow.
sweepFeatherCh :: Double
sweepFeatherCh = 4.0

-- | Deterministic pixel offset for a label — hashed from the label
-- string so the same neighbour always sits in the same place across
-- frames, but different labels break off the invisible grid in
-- different directions.
--
-- Range is kept conservative (±cellWidth/4 horizontal, ±cellHeight/4
-- vertical) so the nudge can't push two neighbour labels into pixel
-- overlap with each other — 'resolveOverlaps' at grid-layout time
-- already guarantees a one-cell gap between same-row labels, and
-- ±quarter-cell nudges always stay inside that gap.
labelPixelOffset :: FontContext -> String -> (CInt, CInt)
labelPixelOffset fc label =
  let h    = foldl (\acc c -> acc * 131 + fromEnum c) 7 label
      cw   = cellWidth fc
      ch   = cellHeight fc
      qW   = cw `div` 4
      qH   = ch `div` 4
      dx   = fromIntegral ((h `mod` fromIntegral (2 * qW)) :: Int) - qW
      dy   = fromIntegral (((h `div` 7) `mod` fromIntegral (2 * qH)) :: Int) - qH
  in (dx, dy)

-- | Pixel bounding box for a label, padded so terrain scatter doesn't
-- crowd the text.  Accounts for the per-label pixel offset so the box
-- tracks where the text actually renders.  The result is (x, y, w, h)
-- in absolute pixel coordinates.
labelBBoxPx :: FontContext -> CInt -> CInt -> HUDCell -> (Int, Int, Int, Int)
labelBBoxPx fc spatialLeft spatialTopRow cell =
  let baseCol = spatialLeft   + fromIntegral (hudCol cell)
      baseRow = spatialTopRow + fromIntegral (hudRow cell)
      (dxPx, dyPx) = labelPixelOffset fc (hudLabel cell)
      px    = baseCol * cellWidth  fc + dxPx
      py    = baseRow * cellHeight fc + dyPx
      textW = fromIntegral (length (hudLabel cell)) * cellWidth fc
      -- Generous vertical: text row + the sensory row below.
      textH = 2 * cellHeight fc
      pad   = 6   -- px of breathing room on every side
  in ( fromIntegral px - pad
     , fromIntegral py - pad
     , fromIntegral textW + 2 * pad
     , fromIntegral textH + 2 * pad
     )

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

-- | Split a list into chunks of N.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Re-export for Runner.
takeLast :: Int -> [a] -> [a]
takeLast n xs = drop (max 0 (length xs - n)) xs

-- ---------------------------------------------------------------------------
-- Journal overlay
-- ---------------------------------------------------------------------------

-- | Which page the journal overlay is showing.
data JournalTab = TabToday | TabPast | TabCatalog
  deriving (Eq, Show)

-- | A day marker journal line has the shape "\x2014 Day <n> \x2014"
-- (em-dash space Day n space em-dash).  The rollover axiom inserts
-- one at the start of each new day, so pages are everything between
-- two markers.
isDayMarker :: String -> Bool
isDayMarker s = "\x2014 Day " `isPrefixOf'` s
  where
    isPrefixOf' p xs = p == take (length p) xs

-- | Split the journal into day pages, oldest-first.  Each page keeps
-- its leading day marker so the overlay can show the header with
-- the entries it introduces.  The final page is "today."
journalPages :: [String] -> [[String]]
journalPages = go []
  where
    go acc [] = [reverse acc | not (null acc)]
    go acc (e:rest)
      | isDayMarker e && not (null acc) = reverse acc : go [e] rest
      | otherwise                        = go (e : acc) rest

-- | Render the journal overlay on the given tab.  The caller cycles
-- tabs with 1/2/3 and dismisses with any other key.
renderJournalOverlay :: SDLContext -> ScenarioDisplay -> GameWorld -> JournalTab -> IO ()
renderJournalOverlay ctx display world tab = do
  clearSDL ctx
  let fc        = sdlFont ctx
      cols      = gridCols ctx
      rows      = gridRows ctx
      headerRow = 1
      firstRow  = headerRow + 3
      lastRow   = rows - 2
  drawJournalHeader fc cols headerRow tab
  case tab of
    TabToday   -> drawToday   fc cols rows firstRow lastRow world
    TabPast    -> drawPast    fc cols     firstRow lastRow world
    TabCatalog -> drawCatalog fc cols     firstRow lastRow display world
  renderText fc "1 today  2 past days  3 catalog   any other key: close" greyText
    (marginLeft, fromIntegral lastRow)
  presentSDL ctx

drawJournalHeader :: FontContext -> Int -> Int -> JournalTab -> IO ()
drawJournalHeader fc cols row tab = do
  let title = case tab of
        TabToday   -> "— journal · today —"
        TabPast    -> "— journal · past days —"
        TabCatalog -> "— journal · catalog —"
  renderText fc title defaultText (marginLeft, fromIntegral row)
  drawHLine fc cols (fromIntegral (row + 1))

-- | Today: entries since the last day marker, plus a small sketch map
-- tucked at the top.  If the journal has never seen a day boundary,
-- the whole thing is "today."
drawToday :: FontContext -> Int -> Int -> Int -> Int -> GameWorld -> IO ()
drawToday fc cols rows firstRow lastRow world = do
  let pages      = journalPages (worldJournal world)
      today      = if null pages then [] else last pages
      pageW      = cols - 2 * fromIntegral marginLeft
      mapHeight  = min 8 (max 0 (rows `div` 6))
      mapRows    = sketchMapRows world (cols - 2 * fromIntegral marginLeft - 4) mapHeight
      afterMap   = firstRow + length mapRows + 1
      budget     = max 1 (lastRow - afterMap)
      wrapped    = concatMap (wrapWords (max 10 pageW)) today
      visible    = takeLast budget wrapped
  mapM_ (\(i, line) ->
    renderText fc line dimText (marginLeft + 2, fromIntegral (firstRow + i))
    ) (zip [0 :: Int ..] mapRows)
  mapM_ (\(i, line) ->
    renderText fc line defaultText (marginLeft, fromIntegral (afterMap + i))
    ) (zip [0 :: Int ..] visible)

-- | Past days: every completed day's entries, paginated by the day
-- markers.  Today is deliberately excluded (it lives in its own tab).
-- The last N lines across all past pages are shown.
drawPast :: FontContext -> Int -> Int -> Int -> GameWorld -> IO ()
drawPast fc cols firstRow lastRow world = do
  let pages   = journalPages (worldJournal world)
      past    = if null pages then [] else init pages
      pageW   = cols - 2 * fromIntegral marginLeft
      flatten = concatMap (wrapWords (max 10 pageW)) (concat past)
      budget  = max 1 (lastRow - firstRow)
      visible = takeLast budget flatten
  case past of
    [] -> renderText fc "No past days yet." greyText (marginLeft, fromIntegral firstRow)
    _  -> mapM_ (\(i, line) ->
            renderText fc line defaultText (marginLeft, fromIntegral (firstRow + i))
            ) (zip [0 :: Int ..] visible)

-- | Catalog: every discovery the scenario exposes, grouped by
-- category.  Empty groups render as a quiet footer line naming what
-- you still haven't seen.  Scenarios without a catalog get a single
-- hint.
drawCatalog :: FontContext -> Int -> Int -> Int -> ScenarioDisplay -> GameWorld -> IO ()
drawCatalog fc cols firstRow lastRow display world = do
  let groups    = sdCatalog display world
      pageW     = cols - 2 * fromIntegral marginLeft
      budget    = max 1 (lastRow - firstRow)
      lines'    = concatMap renderGroup groups
      wrapped   = concatMap (wrapWords (max 10 pageW)) lines'
      visible   = take budget wrapped
  case groups of
    [] -> renderText fc "This scenario doesn't keep a catalog."
            greyText (marginLeft, fromIntegral firstRow)
    _  -> mapM_ (\(i, line) ->
            let color = if line `elem` lines' && null (dropWhile (/= ':') line)
                          then greyText else defaultText
            in renderText fc line color (marginLeft, fromIntegral (firstRow + i))
            ) (zip [0 :: Int ..] visible)
  where
    renderGroup (label, names) =
      let header = label <> "s:"
          body   = if null names
                     then ["  you haven't catalogued any yet."]
                     else map ("  " <>) names
      in header : body ++ [""]

-- ---------------------------------------------------------------------------
-- Sketch map
-- ---------------------------------------------------------------------------

-- | Render a small unicode sketch of where the player has been this
-- hunt, derived from the location graph's normalized coords plus the
-- player's location history.  Returns the map as a list of rows of
-- text so the overlay caller can position it freely.  Empty when the
-- graph has no coord data.
sketchMapRows :: GameWorld -> Int -> Int -> [String]
sketchMapRows _ _ 0 = []
sketchMapRows world width height =
  let coordMap = lgCoords (worldLocationGraph world)
  in if Map.null coordMap
       then []
       else
         let history    = concat (Map.elems (worldLocationHistory world))
             historySet = Set.fromList history
             playerLoc  = case Map.elems (worldLocations world) of
                            (l:_) -> Just l
                            []    -> Nothing
             w          = max 8 (min 40 width)
             h          = max 3 (min 10 height)
             cellAt x y =
               let matches =
                     [ loc
                     | (loc, (nx, ny)) <- Map.toList coordMap
                     , round (nx * fromIntegral (w - 1)) == (x :: Int)
                     , round (ny * fromIntegral (h - 1)) == (y :: Int)
                     ]
               in pickGlyph matches playerLoc historySet
         in [ [ cellAt x y | x <- [0 .. w - 1] ] | y <- [0 .. h - 1] ]

-- | Choose which glyph represents the cell: 'P' for the player, '*'
-- for a history cell, '.' for a mapped but unvisited cell, ' ' when
-- no location falls here.
pickGlyph :: [Location] -> Maybe Location -> Set.Set Location -> Char
pickGlyph locs playerLoc hist
  | any (\l -> Just l == playerLoc) locs = 'P'
  | any (`Set.member` hist) locs         = '*'
  | not (null locs)                      = '.'
  | otherwise                            = ' '
