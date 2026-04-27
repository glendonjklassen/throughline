{-# LANGUAGE CPP           #-}
{-# LANGUAGE TupleSections #-}
-- | SDL2 game runner: constructs a RuntimeUI that renders to an SDL2 window.
module SDL.Runner (sdlUI) where

-- The two 'SDL.Debug' imports below look redundant to hlint, but the
-- second one is CPP-guarded so 'cycleDebug' stays out of release
-- builds.  Suppress the hint so CI doesn't flag it.
{- HLINT ignore "Use fewer imports" -}

import           Control.Monad           (unless, when)
import           Control.Monad.IO.Class (liftIO)
import           Data.Char       (isAsciiUpper)
import           Data.Foldable           (for_)
import           Control.Monad.Reader   (asks)
import           Control.Monad.State    (get)
import           Data.IORef
import           Data.List       (find, partition, sortOn)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe)
import           Data.Word       (Word32)
import           Foreign.C.Types (CInt)
import qualified SDL

import           Engine.Author.Discovery (parseFirstFindLine)
import           Engine.Core.NarrativeMessage (NarrativeEntry(..), neTimeLabel,
                                               NarrativeMessage(..))
import           Engine.Core.Conditions (checkCondition)
import           Engine.Headless       (ActionSource, StepHook, coreLoop)
import           Engine.Runtime        (RuntimeUI(..))
import           GameTypes
import           MonadStack

import           SDL.Animation
import           SDL.ClickMap          (hitTest)
import           SDL.Settings          (Settings(..), loadSettings,
                                        viewportRecommendedFontScale, viewportSize)
import           SDL.SharedFolder      (SharedBroadcastResult(..), broadcastLog)
import           SDL.FindReveal        (findRevealOverlay)
import           SDL.FontContext
import           SDL.InputHandler
import           SDL.Palette
import           SDL.Renderer
import           SDL.SpatialHUD        (SpatialHUD(..), HUDCell(..), hudClickMap,
                                        layoutHUD, hudGenRowCount)
import           SDL.Text              (stripAnsi, wrapWords)
import           SDL.Debug             (learningModeLines)
#ifndef RELEASE_BUILD
import           SDL.Debug             (cycleDebug)
#endif
import           SDL.Layout            (ScenarioDisplay(..), LayoutConfig(..))

-- | Font asset path (relative to working directory).
fontPath :: FilePath
fontPath = "assets/JetBrainsMono-Regular.ttf"

-- | Build an SDLContext honoring the player's current settings —
-- viewport preset, font-scale, palette mode.  Used for every runner
-- surface (game loop, end screen, error/warn, merge prompt) so
-- settings apply to gameplay rendering, not just launcher menus.
initSDLFromSettings :: String -> IO SDLContext
initSDLFromSettings title = do
  settings <- loadSettings
  let vp    = sViewport settings
      scale = viewportRecommendedFontScale vp * sFontScale settings
      mode  = if sHighContrast settings then HighContrast else Autumn
  initSDLWith fontPath title (viewportSize vp) scale mode

-- | Construct an SDL2-based RuntimeUI from a ScenarioDisplay.  The
-- scenario name is threaded through so the journal's
-- "text your friends" action knows which scenario is being
-- broadcast (the shared folder is per-player, multi-scenario).
sdlUI :: String -> ScenarioDisplay -> RuntimeUI
sdlUI scenName display = RuntimeUI
  { uiSetup     = pure ()
  , uiTeardown  = pure ()
  , uiGameLoop  = \env world -> do
      ctx <- initSDLFromSettings scenName
      -- Gameplay is keyboard-first; hide the system cursor so it
      -- doesn't float over the prose.  Clicks still register —
      -- SDL dispatches mouse-button events whether or not the
      -- cursor is visible.
      SDL.cursorVisible SDL.$= False
      result <- runApp env world (sdlGameLoop ctx scenName display)
      SDL.cursorVisible SDL.$= True
      freeSDL ctx
      pure result
  , uiOnEnd     = \finalW -> do
      ctx <- initSDLFromSettings scenName
      clearSDL ctx
      let fc = sdlFont ctx
      let endLines = sdEndScreen display finalW
      mapM_ (\(row, line) ->
        renderText fc (stripAnsi line) textColor (2, fromIntegral row + 1)
        ) (zip [0 :: Int ..] endLines)
      renderText fc "Press any key to exit." chromeColor (2, fromIntegral (length endLines) + 2)
      presentSDL ctx
      _ <- awaitAnyKeySDL
      freeSDL ctx
  , uiOnError   = \msg -> do
      ctx <- initSDLFromSettings scenName
      clearSDL ctx
      renderText (sdlFont ctx) ("Fatal: " <> msg) errorColor (2, 2)
      presentSDL ctx
      _ <- awaitKeySDL
      freeSDL ctx
  , uiOnWarn    = \msg -> do
      ctx <- initSDLFromSettings scenName
      clearSDL ctx
      renderText (sdlFont ctx) msg warningColor (2, 2)
      presentSDL ctx
      _ <- awaitKeySDL
      freeSDL ctx
  , uiPromptMerge = \name count -> do
      ctx <- initSDLFromSettings scenName
      clearSDL ctx
      let fc = sdlFont ctx
      renderText fc ("Foreign log from " <> name <> ": " <> show count <> " new action(s).") chromeColor (2, 2)
      renderText fc "Merge? (y/n)" textColor (2, 4)
      presentSDL ctx
      mc <- awaitKeySDL
      freeSDL ctx
      pure (mc == Just 'y')
  }

-- ---------------------------------------------------------------------------
-- Game loop
-- ---------------------------------------------------------------------------

sdlGameLoop :: SDLContext -> String -> ScenarioDisplay -> App ()
sdlGameLoop ctx scenName display = do
  msgCountRef <- liftIO $ newIORef (0 :: Int)
  -- Stash last-rendered actions so the step hook can keep the HUD stable
  actionsRef  <- liftIO $ newIORef ([] :: [AnyAction])
  -- Remember the player's last-rendered location so the HUD reveal
  -- animation only fires when movement actually happened.  Non-moving
  -- actions (sit, look, wave) leave this unchanged and skip the
  -- animation — the HUD stays fully revealed.
  lastLocRef  <- liftIO $ newIORef (Nothing :: Maybe Location)
  coreLoop (sdlStepHook ctx display msgCountRef actionsRef)
           (sdlActionSource ctx scenName display msgCountRef actionsRef lastLocRef)

-- | Action source: render the world, animate the spatial-HUD reveal,
-- then await a keypress and map it to an action.  If the player hits
-- a key during the reveal, we skip to the fully-revealed HUD and
-- process that key as the selection input.
sdlActionSource :: SDLContext -> String -> ScenarioDisplay
                -> IORef Int -> IORef [AnyAction] -> IORef (Maybe Location)
                -> ActionSource
sdlActionSource ctx scenName display countRef actionsRef lastLocRef actions = do
  world    <- get
  you      <- asks envPlayerCharId
  playerId <- asks envPlayerId
  logRef   <- asks envMessageLog
  debugRef <- asks envDebug
  traceRef <- asks envAxiomTrace
  let layout     = sdLayout display
      statusLine = sdStatusLine display
      sparkleFn  = sdLocationSparkle display world you
      zoneTintFn = sdZoneTintFor display world
      sensoryFn  = sdSensoryFor display world
      render frame = renderWorldSDL ctx layout statusLine sparkleFn zoneTintFn frame
                                    you world actions logRef debugRef traceRef
      currentLoc = Map.lookup you (worldLocations world)
  liftIO $ writeIORef actionsRef actions
  -- Only animate the reveal when the player has moved since the last
  -- turn.  Non-moving actions (sit, look, wave) keep the HUD static.
  prevLoc <- liftIO $ readIORef lastLocRef
  -- First-ever render or a location change → animate.
  -- Same location → static HUD (non-movement action like sit / look).
  let playerMoved = case prevLoc of
        Nothing       -> True                  -- first render of the game
        Just prev     -> Just prev /= currentLoc
  -- Precompute the sorted cell list and per-cell sensory fragments so the
  -- animation loop doesn't re-derive them every frame.
  let totalCols = gridCols ctx
      totalRows = gridRows ctx
      (grc, hasSp) = hudGenRowCount you world actions totalCols
      hudLayout = computeLayout totalRows topBarRows 0 grc hasSp
      hud       = layoutHUD you world actions totalCols (loSpatialBoxH hudLayout)
      sortedCells = sortOn hudDist (shSpatialCells hud)
      tick        = lcTick (worldClock world)
      salt0       = tick + hashLoc currentLoc
      cellSensory i cell =
        case hudTarget cell of
          Just loc -> fromMaybe "" (sensoryFn loc (salt0 + i * 131))
          Nothing  -> ""
      withSensory = zipWith (\i c -> (c, cellSensory i c)) [0 :: Int ..] sortedCells
  liftIO $ writeIORef countRef . length =<< readIORef logRef
  liftIO drainSDLEvents
  skipChar <- liftIO $ if playerMoved
    then animateReveal render withSensory
    else do
      -- No animation: just draw the full-reveal frame and await input.
      render finalReveal
      pure Nothing
  -- Final render at full reveal to guarantee both back buffers are clean.
  liftIO $ render finalReveal
  liftIO $ render finalReveal
  liftIO $ writeIORef lastLocRef currentLoc
  liftIO (awaitKeyLoop playerId actions debugRef skipChar world render hud)
  where
    -- Split actions into the two pools the input handler expects:
    -- non-movement actions land on the home row, movement actions
    -- land on the top letter row.  Order within each pool matches
    -- the action-list order so keys stay stable across turns.
    partitionActions = partition (not . isMovement)
    isMovement a = any (isSetLocation . effectBody) (anyActionEffects a)
    isSetLocation (SetLocation _ _)           = True
    isSetLocation SetLocationRandom {}        = True
    isSetLocation (SetLocationAdjacent _ _)   = True
    isSetLocation SetLocationAdjacentPrefer {}= True
    isSetLocation _                           = False

    awaitKeyLoop :: PlayerId -> [AnyAction] -> IORef DebugMode -> Maybe Char
                 -> GameWorld -> (RevealFrame -> IO ()) -> SpatialHUD
                 -> IO (Maybe AnyAction)
    awaitKeyLoop pid acts debugRef' pending worldNow render' hudLayout = do
      mc <- case pending of
        Just c  -> pure (Just c)
        Nothing -> resolveInput ctx hudLayout acts
      let (generals, movements) = partitionActions acts
      case mc of
        Nothing -> pure Nothing
        Just c
          | c == quitKeyChar  -> do
              confirmed <- confirmQuit ctx
              drainSDLEvents
              render' finalReveal
              if confirmed
                then pure Nothing
                else awaitKeyLoop pid acts debugRef' Nothing worldNow render' hudLayout
#ifndef RELEASE_BUILD
          | c == debugKeyChar -> do
              modifyIORef' debugRef' cycleDebug
              awaitKeyLoop pid acts debugRef' Nothing worldNow render' hudLayout
#endif
          | c == '1' -> do
              journalOverlayLoop ctx scenName pid display worldNow TabToday
              drainSDLEvents
              render' finalReveal
              render' finalReveal
              awaitKeyLoop pid acts debugRef' Nothing worldNow render' hudLayout
          | Just a <- safeOptionIndexIn generalOptionKeys  c generals  -> pure (Just a)
          | Just a <- safeOptionIndexIn movementOptionKeys c movements -> pure (Just a)
          | otherwise -> awaitKeyLoop pid acts debugRef' Nothing worldNow render' hudLayout

-- | Resolve a single input event — key, click, or touch — to a char
-- the dispatch body understands.  Clicks are hit-tested against the
-- gameplay HUD's click map; anything outside the map is treated as
-- "no input, keep waiting".
resolveInput :: SDLContext -> SpatialHUD -> [AnyAction] -> IO (Maybe Char)
resolveInput ctx hudLayout _acts = go
  where
    go = do
      me <- awaitInputSDL (sdlWindow ctx)
      case me of
        Nothing              -> pure Nothing
        Just (KeyPress c)    -> pure (Just c)
        Just (ClickAt px py) -> case hitTest (buildHUDClicks ctx hudLayout) px py of
          Just c  -> pure (Just c)
          Nothing -> go

-- | Assemble the HUD's click regions in pixel space.  Mirrors the
-- renderer's layout math so clicks land where the labels do.  The HUD
-- now sits directly below the top bar, so click rows are
-- computed from 'computeLayout' rather than bottom-aligned.
buildHUDClicks :: SDLContext -> SpatialHUD -> [(Int, Int, Int, Int, Char)]
buildHUDClicks ctx hud =
  let fc   = sdlFont ctx
      cw   = fromIntegral (cellWidth fc)  :: Int
      chH  = fromIntegral (cellHeight fc) :: Int
      cols = gridCols ctx
      rws  = gridRows ctx
      ml   = fromIntegral marginLeft      :: Int
      genLabels    = shGeneralLabels hud
      maxGenLen    = if null genLabels then 0 else maximum (map length genLabels)
      genColW      = maxGenLen + 3
      genNumCols   = max 1 ((cols - 4) `div` max 1 genColW)
      genRowCount  = (length genLabels + genNumCols - 1) `div` max 1 genNumCols
      hasSpatial   = not (null (shSpatialCells hud))
      -- The renderer's click-layout cannot see the player's current
      -- learning-mode / trace state cheaply, so assume it's off here:
      -- learning mode is a dev tool, and if it's on the layout shifts
      -- down uniformly (clicks land a few rows above the labels — not
      -- ideal, but keyboard still works).
      Layout{ loHudStart     = hudStartRow
            , loSpatialTop   = spatialTopRow
            , loGenRowStride = genStride
            } =
        computeLayout rws topBarRows 0 genRowCount hasSpatial
      spatialLeft = (cols - shBoxWidth hud) `div` 2
      gridHits = hudClickMap ml genColW genNumCols genStride hudStartRow
                             spatialLeft spatialTopRow hud
  in [ (col * cw, row * chH, w * cw, h * chH, c)
     | (col, row, w, h, c) <- gridHits
     ]

-- | Ask the player to confirm a mid-hunt quit.  Every action autosaves
-- so leaving truly costs nothing, but a stray Escape keypress shouldn't
-- send them back to the launcher — confirmation protects against that.
-- Accepts key presses and clicks on either option row.
confirmQuit :: SDLContext -> IO Bool
confirmQuit ctx = do
  clearSDL ctx
  let fc = sdlFont ctx
  renderText fc "Quit hunt?"                              textColor (3, 2)
  renderText fc ""                                        dimTextColor     (3, 3)
  renderText fc "Your progress has been saved."           dimTextColor     (3, 4)
  renderText fc "You can pick up where you left off."     dimTextColor     (3, 5)
  renderText fc ""                                        dimTextColor     (3, 6)
  renderText fc "y) Yes, quit"                            textColor (4, 8)
  renderText fc "n) No, keep hunting"                     textColor (4, 9)
  presentSDL ctx
  let yesRect = confirmRowRect fc 8 'y'
      noRect  = confirmRowRect fc 9 'n'
      cm = [yesRect, noRect]
  loop cm
  where
    loop cm = do
      me <- awaitInputSDL (sdlWindow ctx)
      case me of
        Nothing              -> pure False
        Just (KeyPress c)    -> pure (c == 'y' || c == 'Y')
        Just (ClickAt px py) -> case hitTest cm px py of
          Just c  -> pure (c == 'y' || c == 'Y')
          Nothing -> loop cm

-- | Helper for the single-column confirmation dialogs: a full-width
-- row at 'row' that resolves to 'ch' when clicked.
confirmRowRect :: FontContext -> Int -> Char -> (Int, Int, Int, Int, Char)
confirmRowRect fc row ch =
  let cw = fromIntegral (cellWidth fc)  :: Int
      hh = fromIntegral (cellHeight fc) :: Int
  in (0, row * hh, 80 * cw, hh, ch)

-- | Modal shown between ticks when a day rolled over.  Gives the
-- player a moment to sit with the result — a kill, a miss, or
-- calling it early — before the new morning's HUD loads.  The
-- summary lines come straight from the journal entries the scenario
-- just wrote, so the voice stays the scenario's own.
dayEndOverlay :: SDLContext -> String -> [String] -> IO ()
dayEndOverlay ctx dayLabel summary = do
  clearSDL ctx
  let fc   = sdlFont ctx
      cols = gridCols ctx
      rows = gridRows ctx
      pageW = max 20 (cols - 2 * fromIntegral marginLeft - 4)
      wrapped = concatMap (wrapWords pageW) summary
      headerTxt = "\x2014  end of " <> dayLabel <> "  \x2014"
      hintTxt   = "any key to continue"
      -- Vertically center the header + prose block in the top two
      -- thirds of the screen; hint sits two rows above the bottom.
      blockH    = 3 + length wrapped  -- header + spacer + lines (approx)
      headerRow = max 2 ((rows - blockH) `div` 2)
      firstLine = headerRow + 3
      hintRow   = rows - 3
      centerCol s = fromIntegral (max 0 (cols `div` 2 - length s `div` 2))
  renderText fc headerTxt textColor (centerCol headerTxt, fromIntegral headerRow)
  mapM_ (\(i, line) ->
    renderText fc line textColor (centerCol line, fromIntegral (firstLine + i))
    ) (zip [0 :: Int ..] wrapped)
  renderText fc hintTxt chromeColor (centerCol hintTxt, fromIntegral hintRow)
  presentSDL ctx
  drainSDLEvents
  _ <- awaitInputSDL (sdlWindow ctx)
  pure ()

-- | Drive the journal overlay.  Pressing the number of a *different*
-- tab switches to it; pressing the number of the *current* tab
-- closes the overlay (so the key that opened the journal also
-- closes it).  Any other key dismisses.
--
-- Clicks and touches work too: each tab word in the footer is a hit
-- region that behaves exactly as pressing its digit would.  A click
-- outside the footer (i.e. on the body or anywhere else) dismisses
-- the overlay, same as pressing any non-tab key.
journalOverlayLoop
  :: SDLContext -> String -> PlayerId -> ScenarioDisplay
  -> GameWorld -> JournalTab -> IO ()
journalOverlayLoop ctx scenName playerId display world tab =
  loop tab 0
  where
    loop t scroll = do
      renderJournalOverlay ctx display world t scroll
      let fc = sdlFont ctx
          rows = gridRows ctx
          footerRow = rows - 2
          -- Layout of the footer text "1 today  2 past  3 catalog   s share …"
          -- Column positions are stable because marginLeft and the label
          -- strings are fixed.
          todayRect   = journalRect fc footerRow 0  7 '1'
          pastRect    = journalRect fc footerRow 9  8 '2'
          catalogRect = journalRect fc footerRow 19 11 '3'
          shareRect   = journalRect fc footerRow 33 9 's'
          cm = [todayRect, pastRect, catalogRect, shareRect]
          -- One page of scroll is half the visible notebook height.
          -- Keeps PgUp/PgDn responsive without flipping past the
          -- content in one step.
          pageStep = max 1 ((rows - 6) `div` 2)
      me <- awaitInputSDL (sdlWindow ctx)
      case me of
        Nothing -> pure ()
        Just (KeyPress c)    -> dispatch t scroll pageStep c
        Just (ClickAt px py) -> for_ (hitTest cm px py) (dispatch t scroll pageStep)

    dispatch t scroll pageStep c
      | c == '1'               = loop TabToday   0
      | c == '2'               = loop TabPast    0
      | c == '3'               = loop TabCatalog 0
      | c == 's' || c == 'S'   = do
          shareWithFriends ctx scenName playerId
          loop t scroll
      | c == scrollUpKeyChar   = loop t (scroll + 1)
      | c == scrollDownKeyChar = loop t (max 0 (scroll - 1))
      | c == pageUpKeyChar     = loop t (scroll + pageStep)
      | c == pageDownKeyChar   = loop t (max 0 (scroll - pageStep))
      | c == currentTabKey t   = pure ()
      | otherwise              = pure ()

    currentTabKey TabToday   = '1'
    currentTabKey TabPast    = '2'
    currentTabKey TabCatalog = '3'

-- | Handle the "text your friends" journal action.  Reads the
-- configured shared folder from settings, broadcasts the current
-- log to it, and shows a short status screen so the player knows
-- whether the message got through.
shareWithFriends :: SDLContext -> String -> PlayerId -> IO ()
shareWithFriends ctx scenName playerId = do
  settings <- loadSettings
  clearSDL ctx
  let fc = sdlFont ctx
  case sSharedFolder settings of
    Nothing -> do
      renderText fc "No shared folder configured."     warningColor (3, 2)
      renderText fc ""                                 dimTextColor      (3, 3)
      renderText fc "Pick a folder in Settings first"  dimTextColor      (3, 4)
      renderText fc "— a Dropbox / Drive / Syncthing"  dimTextColor      (3, 5)
      renderText fc "path that you and your friends"   dimTextColor      (3, 6)
      renderText fc "all point at.  Their hunts will"  dimTextColor      (3, 7)
      renderText fc "merge into yours automatically."  dimTextColor      (3, 8)
      renderText fc ""                                 dimTextColor      (3, 9)
      renderText fc "press any key or click to return" chromeColor     (3, 11)
      presentSDL ctx
      _ <- awaitInputSDL (sdlWindow ctx)
      pure ()
    Just dir -> do
      result <- broadcastLog dir scenName playerId
      renderShareResult fc result
      presentSDL ctx
      _ <- awaitInputSDL (sdlWindow ctx)
      pure ()
  where
    renderShareResult fc result = case result of
      Broadcast path -> do
        renderText fc "Sent."                              textColor  (3, 2)
        renderText fc ""                                   dimTextColor      (3, 3)
        renderText fc "Your log was copied to:"            dimTextColor      (3, 4)
        renderText fc path                                 dimTextColor      (3, 5)
        renderText fc ""                                   dimTextColor      (3, 6)
        renderText fc "Your friends' hunts will merge"     dimTextColor      (3, 7)
        renderText fc "into yours next time their logs"    dimTextColor      (3, 8)
        renderText fc "are visible in that folder."        dimTextColor      (3, 9)
        renderText fc ""                                   dimTextColor      (3, 10)
        renderText fc "press any key or click to return"   chromeColor     (3, 12)
      BroadcastNoLog -> do
        renderText fc "Nothing to send yet."               dimTextColor      (3, 2)
        renderText fc ""                                   dimTextColor      (3, 3)
        renderText fc "Take a few actions first."          dimTextColor      (3, 4)
        renderText fc ""                                   dimTextColor      (3, 5)
        renderText fc "press any key or click to return"   chromeColor     (3, 7)
      BroadcastFailed err -> do
        renderText fc "Share failed."                      warningColor (3, 2)
        renderText fc ""                                   dimTextColor      (3, 3)
        renderText fc (take 70 err)                        dimTextColor      (3, 4)
        renderText fc ""                                   dimTextColor      (3, 5)
        renderText fc "press any key or click to return"   chromeColor     (3, 7)

-- | Build a single clickable rectangle for a tab label on the
-- journal footer row.  Offset from 'marginLeft' matches the column
-- where the corresponding text starts in the footer string.
journalRect :: FontContext -> Int -> Int -> Int -> Char -> (Int, Int, Int, Int, Char)
journalRect fc row offset widthCells c =
  let cw = fromIntegral (cellWidth fc)  :: Int
      ch = fromIntegral (cellHeight fc) :: Int
      margin = fromIntegral marginLeft :: Int
      x = (margin + offset) * cw
      y = row * ch
  in (x, y, widthCells * cw, ch, c)

-- | Hash a location (or its absence) into an Int for seeded sensory
-- selection.  Co-arrivals at the same tick and location pick the same
-- fragment; different ticks or locations shift the pool.
hashLoc :: Maybe Location -> Int
hashLoc Nothing             = 0
hashLoc (Just (Location s)) = foldl (\acc c -> acc * 131 + fromEnum c) 7 s

-- ---------------------------------------------------------------------------
-- Reveal animation — time-based, continuous fades
-- ---------------------------------------------------------------------------

-- | Fade-in duration for each label (ms).  The label crosses from alpha
-- 0 to 1 over this window, starting when the cell's slot opens.
labelFadeInMs :: Double
labelFadeInMs = 700

-- | The sensory fragment's timeline within a single cell's slot.  A
-- beam of light sweeps left-to-right across the fragment, lighting it
-- as it passes; the fragment holds fully lit; then a shadow sweeps
-- left-to-right, darkening as it passes.  Durations in ms per phase.
sensorySweepInMs, sensoryHoldMs, sensorySweepOutMs :: Double
sensorySweepInMs  = 900
sensoryHoldMs     = 1600
sensorySweepOutMs = 900

-- | Total on-screen life of a single sensory fragment, from first
-- lit char to last dimmed char.  Drives sequential fragment pacing
-- so only one fragment animates at a time.
sensoryTotalMs :: Double
sensoryTotalMs = sensorySweepInMs + sensoryHoldMs + sensorySweepOutMs

-- | Animate the spatial-HUD reveal continuously at ~30fps, composing a
-- 'RevealFrame' per frame from elapsed time.  Each cell gets a per-slot
-- window of length @revealTotalMs / cellCount@ during which its label
-- fades in and its sensory fragment rises, holds, and fades back out.
-- Returns the char of any keypress that interrupted the animation;
-- 'Nothing' means the reveal completed naturally.
animateReveal
  :: (RevealFrame -> IO ())
  -> [(HUDCell, String)]        -- ^ nearest-first cells paired with sensory fragments
  -> IO (Maybe Char)
animateReveal render [] = do
  render finalReveal
  pure Nothing
animateReveal render cellsWithSense = do
  let n           = length cellsWithSense
      -- Labels still stagger at the quick per-cell cadence so the
      -- selection HUD reveals briskly; sensory fragments run
      -- sequentially so only one animates at a time.
      slotMs      = perCellOffsetMs
      cellMeta    = zip [0 :: Int ..] cellsWithSense
      -- Sense-order index: zero-based position among cells that
      -- actually have a non-empty fragment.  Drives sequential
      -- pacing so blank cells don't eat a slot.
      senseOrder  = Map.fromList
                      [ (hudLabel c, si)
                      | (si, (c, _)) <- zip [0 :: Int ..]
                          [ (c, f) | (_, (c, f)) <- cellMeta, not (null f) ]
                      ]
      numSenses   = Map.size senseOrder
      frameAt  :: Double -> RevealFrame
      frameAt elapsed = RevealFrame
        { rfCellAlpha = \cell ->
            case find (\(_, (c, _)) -> hudLabel c == hudLabel cell) cellMeta of
              Nothing     -> 0.0
              Just (i, _) -> labelAlphaAt elapsed (fromIntegral i * slotMs)
        , rfActiveSenses = activeSensesAt elapsed senseOrder cellMeta
        }
      -- Two ends: last label finishes fading in, and the last
      -- fragment finishes its sweep-out.  Take the later of the two
      -- so neither tail is clipped.
      labelFinish = fromIntegral (n - 1) * slotMs + labelFadeInMs
      senseFinish = labelFadeInMs + fromIntegral numSenses * sensoryTotalMs
      totalMs     = max labelFinish senseFinish
  startTick <- SDL.ticks
  loop startTick frameAt totalMs
  where
    loop :: Word32 -> (Double -> RevealFrame) -> Double -> IO (Maybe Char)
    loop start frameAt total = do
      now <- SDL.ticks
      let elapsed = fromIntegral (now - start) :: Double
      if elapsed >= total
        then do
          render finalReveal
          pure Nothing
        else do
          render (frameAt elapsed)
          ri <- waitOrRevealInput 33
          case ri of
            RevealKey c  -> pure (Just c)
            RevealSkip   -> do
              -- Pointer tap during reveal: finish the animation but
              -- don't dispatch a selection.  The player's next input
              -- gets hit-tested against the fully-revealed HUD.
              render finalReveal
              pure Nothing
            RevealTimeout -> loop start frameAt total

-- | Alpha for a label given elapsed ms and the label's slot start ms.
-- Before its slot opens: 0.  During fade-in window: ramps 0→1.  After
-- that: stays at 1 until the animation ends.
labelAlphaAt :: Double -> Double -> Double
labelAlphaAt elapsed slotStart
  | elapsed < slotStart = 0.0
  | otherwise =
      let t = (elapsed - slotStart) / labelFadeInMs
      in max 0.0 (min 1.0 t)

-- | Compute the currently active sensory fragments.  Fragments are
-- scheduled sequentially — fragment @k@ starts exactly when fragment
-- @k-1@ finishes its sweep-out — so a player who wants to soak in a
-- screen can read each one fully before the next begins.  The
-- sense-order index comes from @senseOrder@; cells with blank
-- fragments are skipped and don't eat a slot.
activeSensesAt
  :: Double                          -- ^ elapsed ms
  -> Map.Map String Int              -- ^ hudLabel -> sense-order index
  -> [(Int, (HUDCell, String))]      -- ^ (cell slot, (cell, fragment))
  -> [(HUDCell, Double, Bool, String)]
activeSensesAt elapsed senseOrder cellMeta =
  [ r
  | (_, (cell, frag)) <- cellMeta
  , not (null frag)
  , Just si <- [Map.lookup (hudLabel cell) senseOrder]
  , let sStart   = labelFadeInMs + fromIntegral si * sensoryTotalMs
        sweepEnd = sStart + sensorySweepInMs
        holdEnd  = sweepEnd + sensoryHoldMs
        totalEnd = holdEnd + sensorySweepOutMs
        n        = fromIntegral (length frag)
        -- Sweep travels left-to-right: starts just past the left edge
        -- (-feather/2) and ends just past the right edge (n +
        -- feather/2), so the leading and trailing chars get a full
        -- transit through the feather.
        startPos = negate (sweepFeatherCh / 2)
        endPos   = n + sweepFeatherCh / 2
        travel t = startPos + (endPos - startPos) * smooth t
  , elapsed >= sStart
  , elapsed <  totalEnd
  , let r | elapsed < sweepEnd =
            -- Light sweeping in from the left; leftmost chars light first.
            let t = (elapsed - sStart) / sensorySweepInMs
            in (cell, travel t, False, frag)
          | elapsed < holdEnd =
            -- Fully lit: sweep held past the right edge, no darkening.
            (cell, endPos, False, frag)
          | otherwise =
            -- Shadow sweeping in from the left; leftmost chars dim first.
            let t = (elapsed - holdEnd) / sensorySweepOutMs
            in (cell, travel t, True, frag)
  ]

-- | Smoothstep easing, 0-1 in → 0-1 out with zero slope at both ends.
-- Rounds the sweep so it eases in and out rather than moving at a
-- constant rate — reads more like a drifting beam than a slide.
smooth :: Double -> Double
smooth x =
  let c = max 0 (min 1 x)
  in c * c * (3 - 2 * c)

-- | Stagger between adjacent cells' sensory starts.  Keeps adjacent
-- fragments from all lighting on the same frame while still feeling
-- like one continuous sweep across the neighbours.
perCellOffsetMs :: Double
perCellOffsetMs = 400

-- ---------------------------------------------------------------------------
-- Step hook: typewriter new messages onto the existing frame
-- ---------------------------------------------------------------------------

sdlStepHook :: SDLContext -> ScenarioDisplay
            -> IORef Int -> IORef [AnyAction] -> StepHook
sdlStepHook ctx display countRef actionsRef before after _diff = do
  you      <- asks envPlayerCharId
  logRef   <- asks envMessageLog
  debugRef <- asks envDebug
  traceRef <- asks envAxiomTrace
  allMsgs  <- liftIO $ readIORef logRef
  prevCount <- liftIO $ readIORef countRef
  -- Compute fresh actions for the post-step world so the HUD matches
  -- what the next sdlActionSource call will show (no flicker/double-change).
  allActs  <- asks envActions
  let lastActions = filter (checkCondition after . anyActionCondition) allActs
  liftIO $ writeIORef actionsRef lastActions
  let newCount = length allMsgs
      newMsgs  = reverse (take (newCount - prevCount) allMsgs)
      tension  = getTension after
      newJournal = drop (length (worldJournal before)) (worldJournal after)
      crossedDay = any isDayMarker newJournal
      summary    = dedupe (takeWhile (not . isDayMarker) newJournal)
      endLabel   = sdDayLabel display (worldDayNumber before)
  if null newMsgs
    then pure ()
    else liftIO $ do
      -- Glitch flash at high tension
      when (tension >= 4) $
        glitchFrame (sdlFont ctx) (sdlRenderer ctx) tension
          (fromIntegral (gridCols ctx)) (fromIntegral (gridRows ctx))
      -- Typewrite: each tick re-renders the FULL frame from scratch so both
      -- back buffers always have consistent content.  No partial-buffer flicker.
      let cols     = gridCols ctx
          contentW = cols - 4 - 8
          allOrdered = reverse allMsgs
          labelW   = maximum (0 : map (length . neTimeLabel) allOrdered)
          fc       = sdlFont ctx
      -- Build the list of (entry, plain lines) for the new messages
      let newEntryLines = concatMap (\entry ->
            let ls  = fmtOneEntry contentW labelW entry
                clr = msgColorSDL (neMessage entry) (neTension entry)
                del = beatDelay (neMessage entry)
            in map (clr, del,) ls
            ) newMsgs
      -- Typewrite: reveal one character at a time, full re-render each tick.
      -- Pass old messages only so renderWorldFrame doesn't show new ones in history.
      let oldMsgs = drop (newCount - prevCount) allMsgs  -- allMsgs is newest-first
          layout     = sdLayout display
          statusLine = sdStatusLine display
          sparkleFn  = sdLocationSparkle display after you
          zoneTintFn = sdZoneTintFor display after
      oldLogRef <- newIORef oldMsgs
      -- On a movement action the HUD is hidden during typewriter
      -- ('hiddenReveal') so the reveal animation can take over
      -- cleanly afterwards.  On non-movement actions the choices
      -- should stay exactly where they were — hiding them would
      -- flicker them away and back for no reason.
      let movedHere = Map.lookup you (worldLocations before)
                   /= Map.lookup you (worldLocations after)
          hudFrame  = if movedHere then hiddenReveal else finalReveal
      typewriteFullFrame ctx layout statusLine sparkleFn zoneTintFn hudFrame you after lastActions
                         oldLogRef debugRef traceRef
                         fc labelW newEntryLines
      -- Drain lingering key events, brief pause, then done
      drainSDLEvents
      SDL.delay 400
      -- First-find reveal modals: punctuate any "First X: Y." entries
      -- the scenario wrote this tick with a centered visual of the
      -- find.  Modal is silent (no-op) when the find has no sprite,
      -- so trees and other unillustrated kinds slip through.
      mapM_ (\(kindLbl, name) -> findRevealOverlay ctx kindLbl name [])
            (revealableFinds newJournal)
      -- Day-end transition: if the tick crossed a day boundary (the
      -- scenario wrote a "— ... —" marker this tick), pause on a
      -- recap modal before the new day's HUD loads.  Pulls the
      -- scenario's pre-marker journal lines as the recap text, and
      -- uses the ending day's label from 'sdDayLabel'.
      when crossedDay $ do
        dayEndOverlay ctx endLabel summary
        -- New day, clean slate: the on-screen history starts fresh
        -- so the morning HUD isn't cluttered with yesterday's beats.
        -- 'worldJournal' is untouched, so the notebook tab still
        -- remembers every day.
        writeIORef logRef []
  liftIO $ writeIORef countRef (if crossedDay then 0 else newCount)

-- | Pull (kindLabel, name) pairs out of a journal diff for any
-- first-find lines whose kind warrants a reveal modal.  Lines that
-- don't match the "First Kind: Name." format are silently skipped.
-- Tree first-finds are excluded by intent — trees are catalogued in
-- prose without a visual moment.  Sign first-finds are also
-- excluded; sign reveals are already covered by the spatial HUD's
-- sparkle and ambient narration.
revealableFinds :: [String] -> [(String, String)]
revealableFinds journal =
  [ (kindLabel kind, name)
  | line <- journal
  , Just (kind, name) <- [parseFirstFindLine line]
  , kindShouldReveal kind
  ]
  where
    kindShouldReveal "Tree" = False
    kindShouldReveal "Sign" = False
    kindShouldReveal _      = True
    kindLabel "Find"      = "find"
    kindLabel "Signature" = "signature"
    kindLabel "Animal"    = "creature"
    kindLabel k           = map toLowerCh k
    toLowerCh c
      | isAsciiUpper c = toEnum (fromEnum c + 32)
      | otherwise      = c

-- | Drop adjacent-duplicate entries.  The day-rollover axiom's
-- "called it" path repeats the player's own journal entry; we'd
-- rather show it once on the recap screen.
dedupe :: [String] -> [String]
dedupe (x:y:rest) | x == y = dedupe (x:rest)
dedupe (x:rest)            = x : dedupe rest
dedupe []                  = []

-- ---------------------------------------------------------------------------
-- Full-frame typewriter: re-renders the entire screen each tick so both
-- back buffers always have consistent content.  Zero flicker.
-- ---------------------------------------------------------------------------

-- | Typewrite new messages by revealing one character at a time.
-- Each tick: renderWorldFrame (full clear+render) → overlay partial text → present.
-- A keypress skips to showing all text immediately.
typewriteFullFrame
  :: SDLContext -> LayoutConfig -> (GameWorld -> CharacterId -> Maybe String)
  -> (Location -> Int)
  -> (Location -> Maybe Color)
  -> RevealFrame           -- ^ HUD frame to render under the typewriter
  -> CharacterId -> GameWorld -> [AnyAction]
  -> IORef [NarrativeEntry] -> IORef DebugMode -> IORef [AxiomTrace]
  -> FontContext -> Int
  -> [(Color, Int, String)]   -- ^ (color, delayMs, plainLine) for each new line
  -> IO ()
typewriteFullFrame _   _      _          _         _          _     _   _     _           _      _        _        _  _      []    = pure ()
typewriteFullFrame ctx layout statusLine sparkleFn zoneTintFn frame you world actions logRef debugRef traceRef fc labelW newLines = do
  let cols     = gridCols ctx
      rows     = gridRows ctx
      contentW = cols - fromIntegral marginLeft * 2 - 8
  debugMode <- readIORef debugRef
  traces    <- readIORef traceRef
  allMsgs   <- readIORef logRef
  let learnRowCount = if debugMode == Learning
                        then length (learningModeLines traces you world)
                        else 0
      (genRowCount, hasSpatial) = hudGenRowCount you world actions cols
      Layout{loHistTop = histTop, loHistRows = histAvail} =
        computeLayout rows topBarRows learnRowCount genRowCount hasSpatial
      -- Reserve the bottom of the history area for the typewritten
      -- new text; only show as many old lines as fit above it.
      -- Without this, a burst of new narration (e.g. a kill plus the
      -- rollover axiom's drive-home beats) would typewrite past the
      -- last row of history and clip off-screen.
      newLineCount = length newLines
      availForOld  = max 0 (histAvail - newLineCount)
      -- 'allMsgs' is newest-first; keep newest entries until their
      -- formatted line count exhausts the budget, then stop.
      oldLabelW    = maximum (0 : map (length . neTimeLabel) allMsgs)
      useLabelW    = max labelW oldLabelW
      trimNewest _ []     = []
      trimNewest b (e:es) =
        let lns = length (fmtOneEntry contentW useLabelW e)
        in if lns <= b then e : trimNewest (b - lns) es else []
      trimmedOld   = trimNewest availForOld allMsgs
      trimmedLines = concatMap (fmtOneEntry contentW useLabelW) (reverse trimmedOld)
      oldVisCount  = length trimmedLines
      twStartRow   = fromIntegral (histTop + oldVisCount)
  -- Swap the logRef to the trimmed set so 'renderWorldFrame' renders
  -- exactly what we leave room for — no overlapping old lines under
  -- the typewritten text.
  writeIORef logRef trimmedOld
  -- Flatten new lines into individual characters with their screen row
  let charSteps = buildCharSteps newLines twStartRow
  go charSteps
  where
    -- Render one frame: full world + all revealed characters so far
    renderTick :: [(Color, String, CInt)] -> IO ()
    renderTick revealed = do
      renderWorldFrame ctx layout statusLine sparkleFn zoneTintFn frame you world actions logRef debugRef traceRef
      mapM_ (\(clr, txt, row) ->
        renderText fc txt clr (marginLeft, row)
        ) revealed
      presentSDL ctx

    go :: [(Color, Int, Char, CInt, String)] -> IO ()
    go [] = pure ()
    go steps = goInner [] steps

    -- Process one character at a time, accumulating revealed text
    goInner :: [(Color, String, CInt)] -> [(Color, Int, Char, CInt, String)] -> IO ()
    goInner revealed [] = renderTick revealed  -- final frame with all text
    goInner revealed ((clr, delay, _ch, row, soFar) : rest) = do
      let revealed' = updateRevealed revealed clr row soFar
      renderTick revealed'
      skip <- waitOrKey delay
      if skip
        then do
          -- Dump everything: build final revealed state
          let final = foldl (\acc (c, _, _, r, s) -> updateRevealed acc c r s) revealed' rest
          renderTick final
        else goInner revealed' rest

    -- Update the revealed lines: replace or add the line for this row
    updateRevealed :: [(Color, String, CInt)] -> Color -> CInt -> String -> [(Color, String, CInt)]
    updateRevealed [] clr row txt = [(clr, txt, row)]
    updateRevealed ((_c, _, r):xs) clr row txt
      | r == row  = (clr, txt, row) : xs
    updateRevealed (x:xs) clr row txt = x : updateRevealed xs clr row txt

-- | Expand lines into per-character steps: (color, delay, char, row, prefixSoFar)
buildCharSteps :: [(Color, Int, String)] -> CInt -> [(Color, Int, Char, CInt, String)]
buildCharSteps [] _ = []
buildCharSteps ((_, _, []):rest) row = buildCharSteps rest (row + 1)
buildCharSteps ((clr, delay, line):rest) row =
  let steps = [ (clr, delay, c, row, take i line) | (i, c) <- zip [1..] line ]
  in steps ++ buildCharSteps rest (row + 1)

-- | Drain all pending SDL events so stale keypresses don't leak.
drainSDLEvents :: IO ()
drainSDLEvents = do
  events <- SDL.pollEvents
  unless (null events) drainSDLEvents

-- | Format a single entry into plain strings (no ANSI).
fmtOneEntry :: Int -> Int -> NarrativeEntry -> [String]
fmtOneEntry contentW labelW entry =
  let label   = neTimeLabel entry
      raw     = msgLinesPlain (neMessage entry)
      wrapped = concatMap (wrapWords (max 10 (contentW - labelW - 2))) raw
      pad     = replicate (labelW + 2) ' '
      labelPad = padToN (labelW + 2) label
  in case wrapped of
       []     -> []
       (l:ls) -> (labelPad <> l) : map (pad <>) ls

-- | Extract raw text lines from a message.
msgLinesPlain :: NarrativeMessage -> [String]
msgLinesPlain (MsgSay _ sName _ lNames text) =
  [sName <> fmtLis lNames <> ": " <> text]
msgLinesPlain (MsgThink _ text)     = ["~ " <> text]
msgLinesPlain (MsgNarrate text)     = ["> " <> text]
msgLinesPlain (MsgEffect text)      = ["  " <> text]
msgLinesPlain (MsgDialogue dls)     = map fmtDL dls
  where fmtDL (_, sName, _, lNames, text) =
          sName <> fmtLis lNames <> ": " <> text

fmtLis :: [String] -> String
fmtLis [] = ""
fmtLis ns = " (to " <> unwords ns <> ")"

-- | Color for a message type.
msgColorSDL :: NarrativeMessage -> Int -> Color
msgColorSDL MsgSay {}       _ = dialogueColor
msgColorSDL (MsgThink _ _)  _ = thoughtColor
msgColorSDL (MsgNarrate _)  t = narratorColor t
msgColorSDL (MsgEffect _)   t = narratorColor t
msgColorSDL (MsgDialogue _) _ = dialogueColor

-- | Typewriter delay per message type (milliseconds).
beatDelay :: NarrativeMessage -> Int
beatDelay (MsgThink _ _)    = 16
beatDelay MsgSay {}         = 10
beatDelay (MsgNarrate _)    = 8
beatDelay (MsgEffect _)     = 8
beatDelay (MsgDialogue _)   = 10

-- | Pad a string to N chars.
padToN :: Int -> String -> String
padToN n s
  | length s >= n = s
  | otherwise     = s <> replicate (n - length s) ' '
