{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- | SDL2 text-grid renderer. Full-width layout: top status bar, scrolling
-- message history in the middle, action HUD at the bottom.
module SDL.Renderer
  ( SDLContext(..)
  , initSDL
  , initSDLWith
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
  , isDayMarker
  , dayMarkerLabel
  , Layout(..)
  , computeLayout
  ) where

import           Control.Monad (unless, void, when)
import           Data.Char (toLower)
import           Data.IORef
import           Data.List (intercalate, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Foreign.C.Types (CInt)
import qualified SDL
import           SDL.FontContext
import           SDL.Palette
import           SDL.Text (stripAnsi, wrapWords)
import           SDL.Primitives (drawCellUnderline)
import           SDL.Sprites    (drawSprite, spritesForClass)
import           SDL.SpatialHUD (SpatialHUD(..), HUDCell(..),
                                 TrailMark(..), SpritePlacement(..),
                                 layoutHUD, hudGenRowCount,
                                 terrainSpriteScatter, trailMarks)
import           Engine.Core.World (playerLocationName, engineTimeStatus, exitBearings, getWeather)
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

-- | Shipping default viewport size — Steam Deck native.  Retained so
-- any caller of 'initSDL' that doesn't plumb settings through keeps
-- the previous behaviour.
windowWidth, windowHeight :: CInt
windowWidth  = 1280
windowHeight = 800

fontSize :: Int
fontSize = 16

-- | Init with default settings (Autumn palette, Deck viewport, 1.0
-- font scale, windowed) and a generic window title.  Callers with a
-- 'Settings' value and a bundle-specific title should use
-- 'initSDLWith'.
initSDL :: FilePath -> IO SDLContext
initSDL fontPath = initSDLWith fontPath "Throughline"
                                (fromIntegral windowWidth, fromIntegral windowHeight)
                                1.0 Autumn

-- | Init with explicit window title, viewport size, font-scale and
-- palette mode.  Font scale is clamped to a sensible range so a
-- bogus settings file can't render text unreadably tiny or clip
-- text off the window.  The window size is clamped in the same
-- spirit — a preset that somehow names a 0×0 size falls back to Deck.
initSDLWith :: FilePath -> String -> (Int, Int) -> Double -> PaletteMode
            -> IO SDLContext
initSDLWith fontPath title (wPx0, hPx0) scale mode = do
  let wPx = fromIntegral (max 640  wPx0)
      hPx = fromIntegral (max 400  hPx0)
  SDL.initializeAll
  window <- SDL.createWindow (T.pack title)
    SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 wPx hPx }
  renderer <- SDL.createRenderer window (-1)
    SDL.defaultRenderer { SDL.rendererType = SDL.AcceleratedVSyncRenderer }
  -- Cap the scale at 4.0 rather than 2.0 — 4K at the default font
  -- needs ~2.7× just to read like Deck does.
  let clamped = max 0.5 (min 4.0 scale)
      ptSize  = max 8 (round (fromIntegral fontSize * clamped))
  fc <- initFontWith renderer fontPath ptSize mode
  let cols = wPx `div` cellWidth fc
      rows = hPx `div` cellHeight fc
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
  let Color r g b a = remapBgColor (fcPalette (sdlFont ctx))
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
-- Layout math — vertical zones for the main gameplay screen
-- ---------------------------------------------------------------------------

-- | Where each zone of the gameplay screen starts / how many rows it
-- gets.  Top-down order: thin top bar, then the selection HUD (the
-- biggest chunk — prompt, general actions, spatial map), then a
-- separator, then the text history filling the rest at the bottom.
--
-- The spatial HUD absorbs whatever rows remain between the general
-- actions and the history separator, so the sprite scatter fills the
-- gap that would otherwise read as dead space.
data Layout = Layout
  { loHudStart     :: !Int  -- ^ row of HUD separator
  , loSpatialTop   :: !Int  -- ^ first row of the spatial-HUD box
  , loSpatialBoxH  :: !Int  -- ^ height of the spatial-HUD box in rows
  , loGenRowStride :: !Int  -- ^ rows allocated per general-action row (touch spacing)
  , loHistSep      :: !Int  -- ^ row of separator between HUD and history
  , loHistTop      :: !Int  -- ^ first row of history text
  , loHistRows     :: !Int  -- ^ visible history rows
  }

-- | Rows of blank space below each general-action row so tap targets
-- aren't cramped against the next row.  A stride of 2 means each
-- logical general row actually occupies 2 screen rows (text row +
-- padding), giving ~32-40 px of vertical hit area for touch.
generalRowStride :: Int
generalRowStride = 2

-- | Fixed budget for the history zone at the bottom of the screen.
historyRowBudget :: Int
historyRowBudget = 5

-- | Empty rows left below the history so the last log line doesn't
-- hug the bottom edge of the window.  Pure breathing room — nothing
-- renders here.
bottomMargin :: Int
bottomMargin = 2

-- | Compute the vertical layout.  Top-down: top bar, optional
-- learning-mode lines, HUD divider, prompt, general actions (one row
-- of labels per 'generalRowStride' screen rows), gap, spatial HUD
-- filling the remaining space, history separator, and a fixed
-- history zone at the bottom.  The spatial HUD stretches to fill
-- whatever's left between the general actions and the history
-- separator — on tall viewports that's a lot of room for sprites
-- and neighbour labels, on short viewports it collapses gracefully.
computeLayout
  :: Int   -- ^ total rows
  -> Int   -- ^ top-bar height
  -> Int   -- ^ learning-mode rows (under top bar)
  -> Int   -- ^ general-action rows
  -> Bool  -- ^ has spatial HUD?
  -> Layout
computeLayout rows topH learnH genRowCount hasSpatial =
  let hudStart   = topH + learnH
      histRows   = min historyRowBudget (max 0 (rows - hudStart - 4 - bottomMargin))
      histTop    = rows - histRows - bottomMargin
      histSep    = histTop - 1
      genAreaH   = genRowCount * generalRowStride
      spatialTop = hudStart + 2 + genAreaH + (if hasSpatial then 1 else 0)
      boxH       = if hasSpatial then max 0 (histSep - spatialTop) else 0
  in Layout
      { loHudStart     = hudStart
      , loSpatialTop   = spatialTop
      , loSpatialBoxH  = boxH
      , loGenRowStride = generalRowStride
      , loHistSep      = histSep
      , loHistTop      = histTop
      , loHistRows     = histRows
      }

-- | Draw the top bar: centred location name, right-aligned time,
-- compass rose on the next row, separator on the third.  Same three
-- rows for every frame type (full render, back-buffer render,
-- typewriter tick).
drawTopBar
  :: FontContext
  -> Int
  -> CharId
  -> GameWorld
  -> (GameWorld -> Maybe String)
  -> IO ()
drawTopBar fc cols you world statusLine = do
  let locName     = fromMaybe "" (playerLocationName you world)
      timeStr     = fromMaybe "" (engineTimeStatus world)
      timeCol     = max (marginLeft + 1)
                      (fromIntegral cols - fromIntegral (length timeStr) - marginLeft)
      compassDirs = [ lbl | (_, lbl, _) <- exitBearings you world ]
      locCol      = max marginLeft (fromIntegral (cols - length locName) `div` 2)
  renderText fc locName defaultText (locCol, 0)
  case timeStr of
    "" -> pure ()
    _  -> renderText fc timeStr dimText (timeCol, 0)
  drawCompassRose fc cols 1 compassDirs
  drawHLine fc cols 2
  case statusLine world of
    Just s  -> renderText fc s dimText (marginLeft, 1)
    Nothing -> pure ()

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
      (genRowCount, hasSpatial) = hudGenRowCount you world actions cols
      learnLines    = if debugMode == Learning
                        then learningModeLines traces you world
                        else []
      learnRowCount = length learnLines
      Layout{..}    = computeLayout rows topBarRows learnRowCount
                                     genRowCount hasSpatial
      hud           = layoutHUD you world actions cols loSpatialBoxH
      genLabels     = shGeneralLabels hud
      maxGenLen     = if null genLabels then 0 else maximum (map length genLabels)
      genColW       = maxGenLen + 3
      genNumCols    = max 1 ((cols - 4) `div` max 1 genColW)
      genRows       = chunksOf genNumCols genLabels

  -- Top bar (rows 0-2)
  drawTopBar fc cols you world statusLine

  -- Learning mode (optional, just below top bar)
  mapM_ (\(idx, line) ->
    renderText fc (stripAnsi line) dimText (marginLeft, fromIntegral (topBarRows + idx))
    ) (zip [0 :: Int ..] learnLines)

  -- Selection HUD (middle, largest) — prompt, general actions, spatial map
  drawHLine fc cols (fromIntegral loHudStart)
  renderText fc "What do you do?" defaultText (marginLeft, fromIntegral (loHudStart + 1))
  mapM_ (\(rowIdx, rowActions) -> do
    let y = fromIntegral (loHudStart + 2 + rowIdx * loGenRowStride)
    mapM_ (\(colIdx, label) -> do
      let x = marginLeft + 1 + fromIntegral colIdx * fromIntegral genColW
      renderText fc label greyText (x, y)
      ) (zip [0 :: Int ..] rowActions)
    ) (zip [0 :: Int ..] genRows)
  let spatialLeft = fromIntegral ((cols - shBoxWidth hud) `div` 2)
  when hasSpatial $
    drawSpatialHUD fc cols sparkleFn zoneTintFn frame
                   you world hud spatialLeft (fromIntegral loSpatialTop)

  -- History (bottom, 2nd largest)
  drawHLine fc cols (fromIntegral loHistSep)
  let allOrdered = reverse allMsgs
      labelW     = maximum (0 : map (length . neTimeLabel) allOrdered)
      dispLines  = concatMap (fmtEntry contentW labelW) allOrdered
      visible    = takeLast loHistRows dispLines
      faded      = ageFadeVisible visible
  mapM_ (\(idx, (color, line)) ->
    renderText fc (take (cols - 2) line) color (marginLeft, fromIntegral (loHistTop + idx))
    ) (zip [0 :: Int ..] faded)

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
      (genRowCount, hasSpatial) = hudGenRowCount you world actions cols
      learnLines    = if debugMode == Learning
                        then learningModeLines traces you world
                        else []
      learnRowCount = length learnLines
      Layout{..}    = computeLayout rows topBarRows learnRowCount
                                     genRowCount hasSpatial
      hud           = layoutHUD you world actions cols loSpatialBoxH
      genLabels     = shGeneralLabels hud
      maxGenLen     = if null genLabels then 0 else maximum (map length genLabels)
      genColW       = maxGenLen + 3
      genNumCols    = max 1 ((cols - 4) `div` max 1 genColW)
      genRows       = chunksOf genNumCols genLabels

  drawTopBar fc cols you world statusLine

  mapM_ (\(idx, line) ->
    renderText fc (stripAnsi line) dimText (marginLeft, fromIntegral (topBarRows + idx))
    ) (zip [0 :: Int ..] learnLines)

  drawHLine fc cols (fromIntegral loHudStart)
  renderText fc "What do you do?" defaultText (marginLeft, fromIntegral (loHudStart + 1))
  mapM_ (\(rowIdx, rowActions) -> do
    let y = fromIntegral (loHudStart + 2 + rowIdx * loGenRowStride)
    mapM_ (\(colIdx, label) -> do
      let x = marginLeft + 1 + fromIntegral colIdx * fromIntegral genColW
      renderText fc label greyText (x, y)
      ) (zip [0 :: Int ..] rowActions)
    ) (zip [0 :: Int ..] genRows)
  let spatialLeft = fromIntegral ((cols - shBoxWidth hud) `div` 2)
  when hasSpatial $
    drawSpatialHUD fc cols sparkleFn zoneTintFn frame
                   you world hud spatialLeft (fromIntegral loSpatialTop)

  drawHLine fc cols (fromIntegral loHistSep)
  let allOrdered = reverse allMsgs
      labelW     = maximum (0 : map (length . neTimeLabel) allOrdered)
      dispLines  = concatMap (fmtEntry contentW labelW) allOrdered
      visible    = takeLast loHistRows dispLines
      faded      = ageFadeVisible visible
  mapM_ (\(idx, (color, line)) ->
    renderText fc (take (cols - 2) line) color (marginLeft, fromIntegral (loHistTop + idx))
    ) (zip [0 :: Int ..] faded)

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
  -> Int                              -- ^ total cols (for edge clamping)
  -> (Location -> Int)
  -> (Location -> Maybe Color)
  -> RevealFrame
  -> CharId
  -> GameWorld
  -> SpatialHUD
  -> CInt        -- ^ spatialLeft (col)
  -> CInt        -- ^ spatialTopRow (row)
  -> IO ()
drawSpatialHUD fc totalCols sparkleFn zoneTintFn frame you world hud spatialLeft spatialTopRow = do
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
  --
  -- Right-edge clamp: if a fragment anchored to its cell's column
  -- would overflow the screen, nudge its starting column left just
  -- enough that the last character fits, leaving a small right
  -- margin.  Cell-to-fragment visual tie loosens slightly for these
  -- edge cases, but the alternative (clipping off the tail) is
  -- strictly worse to read.
  mapM_ (\(cell, sweep, darkening, fragment) ->
    unless (null fragment) $ do
      let baseRow    = spatialTopRow + fromIntegral (hudRow cell) + 1
          naturalCol = spatialLeft   + fromIntegral (hudCol cell)
          rightEdge  = fromIntegral totalCols - 1 :: CInt
          maxStart   = max 0 (rightEdge - fromIntegral (length fragment))
          startCol   = max 0 (min naturalCol maxStart)
          py         = baseRow * cellHeight fc
          baseX      = startCol * cellWidth fc
          cw         = cellWidth fc
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

-- | A day marker journal line has the shape "\x2014 <label> \x2014"
-- — em-dash, space, the scenario's day label (day number, date,
-- whatever it chooses), space, em-dash.  The rollover axiom inserts
-- one at the start of each new day, so pages are everything between
-- two markers.  Label content is deliberately not parsed here —
-- every scenario can use its own vocabulary for time.
isDayMarker :: String -> Bool
isDayMarker s =
  length s >= 4
  && take 2 s == "\x2014 "
  && drop (length s - 2) s == " \x2014"

-- | Extract the label between the em-dashes of a day marker, or
-- 'Nothing' for a non-marker line.  Renderers show this directly as
-- the day header, so the scenario has full control of the text.
dayMarkerLabel :: String -> Maybe String
dayMarkerLabel s
  | isDayMarker s = Just (drop 2 (take (length s - 2) s))
  | otherwise     = Nothing

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
    TabToday   -> drawToday   fc display cols rows firstRow lastRow world
    TabPast    -> drawPast    fc display cols      firstRow lastRow world
    TabCatalog -> drawCatalog fc cols              firstRow lastRow display world
  renderText fc "1 today  2 earlier  3 index   s share   any other: close" greyText
    (marginLeft, fromIntegral lastRow)
  presentSDL ctx

drawJournalHeader :: FontContext -> Int -> Int -> JournalTab -> IO ()
drawJournalHeader fc cols row tab = do
  let title = case tab of
        TabToday   -> "— field notes · today —"
        TabPast    -> "— field notes · earlier —"
        TabCatalog -> "— field notes · index —"
  renderText fc title defaultText (marginLeft, fromIntegral row)
  drawHLine fc cols (fromIntegral (row + 1))

-- ---------------------------------------------------------------------------
-- Field-notebook layout
-- ---------------------------------------------------------------------------

-- | A single drawable line of the notebook.  'Nothing' is a blank row
-- — the page breathes between entries and day blocks.  'Just (color,
-- leftCol, text)' draws @text@ at @leftCol@ in @color@.  A list of
-- these is easy to trim (for scrolling) and cheap to render.
type JournalLine = Maybe (Color, CInt, String)

-- | Columns for the notebook's left gutter.  "Day N" headers sit
-- snug against the margin so they read like a ribbon; entry text is
-- indented so the gutter reads as whitespace around the day label.
notebookHeaderCol, notebookEntryCol, notebookContCol :: CInt
notebookHeaderCol = marginLeft         -- "Day N"
notebookEntryCol  = marginLeft + 6     -- "·  entry..."
notebookContCol   = marginLeft + 9     -- wrapped continuation

-- | Useful text width for wrapping notebook entries.
notebookTextWidth :: Int -> Int
notebookTextWidth cols = max 10 (cols - fromIntegral notebookEntryCol - 4)

-- | Draw a vertical slice of notebook lines between @firstRow@ and
-- @lastRow@ (inclusive).  If @anchorBottom@ is True, the *last* N
-- lines that fit are shown (useful for live-feeling scroll); if
-- False, the *first* N are shown (useful for an index that grows
-- from the top).
drawJournalLines :: FontContext -> Int -> Int -> Bool -> [JournalLine] -> IO ()
drawJournalLines fc firstRow lastRow anchorBottom lines_ =
  let budget  = max 0 (lastRow - firstRow)
      visible = if anchorBottom
                  then takeLast budget lines_
                  else take budget lines_
  in mapM_ draw (zip [0 :: Int ..] visible)
  where
    draw (_, Nothing) = pure ()
    draw (i, Just (color, col, txt)) =
      renderText fc txt color (fromIntegral col, fromIntegral (firstRow + i))

-- | Format one day's journal page as notebook lines.  The day header
-- reads its scenario-provided label (a date, a day number — whatever
-- the scenario emits); if @msub@ is provided it renders dimmed
-- underneath (weather for today, nothing for past days).  Entries
-- flow below with a middle-dot gutter glyph, wrapped continuation
-- lines aligned under the first word, and a blank row between each
-- entry so the page has texture rather than a flat list.
dayBlockLines :: Int -> String -> Maybe String -> [String] -> [JournalLine]
dayBlockLines textW dayLabel msub entries =
  let header = Just (defaultText, notebookHeaderCol, dayLabel)
      subLine = case msub of
        Just s  -> [Just (dimText, notebookHeaderCol, s)]
        Nothing -> []
      entryRows entry = case wrapWords textW entry of
        []     -> []
        (l:ls) -> Just (defaultText, notebookEntryCol, "\x00b7  " <> l)
                : map (\r -> Just (defaultText, notebookContCol, r)) ls
      entriesSection = intercalate [Nothing] (map entryRows entries)
  in [header] <> subLine <> [Nothing] <> entriesSection <> [Nothing]

-- | Split a journal page into (dayLabel, entriesOnly).  The first
-- line is treated as a day marker when it parses as one; otherwise
-- we fall back to the caller-provided default (used for the initial
-- day, which has no leading marker).
pageLabelAndEntries :: String -> [String] -> (String, [String])
pageLabelAndEntries fallback page = case page of
  (m:rest) | Just lbl <- dayMarkerLabel m -> (lbl, rest)
  _                                       -> (fallback, page)

-- | Today: current day's notebook block.  Header shows the
-- scenario's label for the current day (a date in DeerHunt, "Day N"
-- for default scenarios) and, if we can read it off the world tags,
-- the weather as a dim subtitle.  Entries follow in paragraph form.
-- Scrolls from the bottom so the newest entry is always visible at
-- the edge of the page.
drawToday :: FontContext -> ScenarioDisplay -> Int -> Int -> Int -> Int -> GameWorld -> IO ()
drawToday fc display cols _rows firstRow lastRow world =
  let pages   = journalPages (worldJournal world)
      dayNum  = max 1 (length pages)
      fallback = sdDayLabel display dayNum
      (todayLabel, todayEntries) =
        pageLabelAndEntries fallback
                            (if null pages then [] else last pages)
      weather = fmap (map toLower . weatherName) (getWeather world)
      textW   = notebookTextWidth cols
      lines_  = dayBlockLines textW todayLabel weather todayEntries
  in case todayEntries of
       [] -> renderText fc "Nothing written yet today."
               greyText (notebookHeaderCol, fromIntegral firstRow)
       _  -> drawJournalLines fc firstRow lastRow True lines_

-- | Earlier days: every completed day's notebook block stacked in
-- chronological order.  Label is read from each page's leading
-- marker, falling back to the scenario's own day formatter for the
-- initial day (written without a marker).
drawPast :: FontContext -> ScenarioDisplay -> Int -> Int -> Int -> GameWorld -> IO ()
drawPast fc display cols firstRow lastRow world =
  let pages = journalPages (worldJournal world)
      past  = if null pages then [] else init pages
      textW = notebookTextWidth cols
      blocks = zipWith
        (\i p ->
          let fallback = sdDayLabel display (i + 1)
              (lbl, es) = pageLabelAndEntries fallback p
          in dayBlockLines textW lbl Nothing es)
        [0 :: Int ..]
        past
      lines_ = concat blocks
  in case past of
       [] -> renderText fc "No earlier days yet."
               greyText (notebookHeaderCol, fromIntegral firstRow)
       _  -> drawJournalLines fc firstRow lastRow True lines_

-- | Index: things noticed on this hunt, as diary paragraphs rather
-- than a ledger.  Each scenario emits pre-formatted prose (day,
-- sighting, short factoid), and the renderer just wraps and spaces
-- them out — no counts, no grouping, no checklist texture.  Empty
-- catalog reads as one dim line so the page doesn't go blank.
drawCatalog :: FontContext -> Int -> Int -> Int -> ScenarioDisplay -> GameWorld -> IO ()
drawCatalog fc cols firstRow lastRow display world =
  let entries = sdCatalog display world
      textW   = notebookTextWidth cols
      lines_  = intercalate [Nothing] (map (entryLines textW) entries)
  in case entries of
       [] -> renderText fc "Nothing kept in this ledger yet."
               greyText (notebookHeaderCol, fromIntegral firstRow)
       _  -> drawJournalLines fc firstRow lastRow False lines_
  where
    entryLines textW paragraph =
      case wrapWords textW paragraph of
        []     -> []
        (l:ls) ->
          Just (defaultText, notebookEntryCol, "\x00b7  " <> l)
          : map (\r -> Just (defaultText, notebookContCol, r)) ls

