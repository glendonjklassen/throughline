{-# LANGUAGE TupleSections #-}
-- | SDL2 game runner: constructs a RuntimeUI that renders to an SDL2 window.
module SDL.Runner (sdlUI) where

import           Control.Monad           (unless, when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (asks)
import           Control.Monad.State    (get)
import           Data.IORef
import           Data.List       (find, partition, sortOn)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe)
import           Data.Word       (Word32)
import           Foreign.C.Types (CInt)
import qualified SDL

import           Engine.Core.NarrativeMessage (NarrativeEntry(..), neTimeLabel,
                                               NarrativeMessage(..))
import           Engine.Core.Conditions (checkCondition)
import           Engine.Headless       (ActionSource, StepHook, coreLoop)
import           Engine.Runtime        (RuntimeUI(..))
import           GameTypes
import           MonadStack

import           SDL.Animation
import           SDL.FontContext
import           SDL.InputHandler
import           SDL.Palette
import           SDL.Renderer
import           SDL.SpatialHUD        (SpatialHUD(..), HUDCell(..), layoutHUD)
import           SDL.Text              (stripAnsi, wrapWords)
import           SDL.Debug             (cycleDebug, learningModeLines)
import           SDL.Layout            (ScenarioDisplay(..), LayoutConfig(..))

-- | Font asset path (relative to working directory).
fontPath :: FilePath
fontPath = "assets/JetBrainsMono-Regular.ttf"

-- | Construct an SDL2-based RuntimeUI from a ScenarioDisplay.
sdlUI :: ScenarioDisplay -> RuntimeUI
sdlUI display = RuntimeUI
  { uiSetup     = pure ()
  , uiTeardown  = pure ()
  , uiGameLoop  = \env world -> do
      ctx <- initSDL fontPath
      result <- runApp env world (sdlGameLoop ctx display)
      freeSDL ctx
      pure result
  , uiOnEnd     = \finalW -> do
      ctx <- initSDL fontPath
      clearSDL ctx
      let fc = sdlFont ctx
      let endLines = sdEndScreen display finalW
      mapM_ (\(row, line) ->
        renderText fc (stripAnsi line) defaultText (2, fromIntegral row + 1)
        ) (zip [0 :: Int ..] endLines)
      renderText fc "Press any key to exit." greyText (2, fromIntegral (length endLines) + 2)
      presentSDL ctx
      _ <- awaitAnyKeySDL
      freeSDL ctx
  , uiOnError   = \msg -> do
      ctx <- initSDL fontPath
      clearSDL ctx
      renderText (sdlFont ctx) ("Fatal: " <> msg) errorColor (2, 2)
      presentSDL ctx
      _ <- awaitKeySDL
      freeSDL ctx
  , uiOnWarn    = \msg -> do
      ctx <- initSDL fontPath
      clearSDL ctx
      renderText (sdlFont ctx) msg warningColor (2, 2)
      presentSDL ctx
      _ <- awaitKeySDL
      freeSDL ctx
  , uiPromptMerge = \name count -> do
      ctx <- initSDL fontPath
      clearSDL ctx
      let fc = sdlFont ctx
      renderText fc ("Foreign log from " <> name <> ": " <> show count <> " new action(s).") greyText (2, 2)
      renderText fc "Merge? (y/n)" defaultText (2, 4)
      presentSDL ctx
      mc <- awaitKeySDL
      freeSDL ctx
      pure (mc == Just 'y')
  }

-- ---------------------------------------------------------------------------
-- Game loop
-- ---------------------------------------------------------------------------

sdlGameLoop :: SDLContext -> ScenarioDisplay -> App ()
sdlGameLoop ctx display = do
  msgCountRef <- liftIO $ newIORef (0 :: Int)
  -- Stash last-rendered actions so the step hook can keep the HUD stable
  actionsRef  <- liftIO $ newIORef ([] :: [AnyAction])
  -- Remember the player's last-rendered location so the HUD reveal
  -- animation only fires when movement actually happened.  Non-moving
  -- actions (sit, look, wave) leave this unchanged and skip the
  -- animation — the HUD stays fully revealed.
  lastLocRef  <- liftIO $ newIORef (Nothing :: Maybe Location)
  coreLoop (sdlStepHook ctx display msgCountRef actionsRef)
           (sdlActionSource ctx display msgCountRef actionsRef lastLocRef)

-- | Action source: render the world, animate the spatial-HUD reveal,
-- then await a keypress and map it to an action.  If the player hits
-- a key during the reveal, we skip to the fully-revealed HUD and
-- process that key as the selection input.
sdlActionSource :: SDLContext -> ScenarioDisplay
                -> IORef Int -> IORef [AnyAction] -> IORef (Maybe Location)
                -> ActionSource
sdlActionSource ctx display countRef actionsRef lastLocRef actions = do
  world    <- get
  you      <- asks envPlayerCharId
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
      hud       = layoutHUD you world actions totalCols
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
  liftIO (awaitKeyLoop actions debugRef skipChar world render)
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

    awaitKeyLoop :: [AnyAction] -> IORef DebugMode -> Maybe Char -> GameWorld
                 -> (RevealFrame -> IO ()) -> IO (Maybe AnyAction)
    awaitKeyLoop acts debugRef' pending worldNow render' = do
      mc <- case pending of
        Just c  -> pure (Just c)
        Nothing -> awaitKeySDL
      let (generals, movements) = partitionActions acts
      case mc of
        Nothing -> pure Nothing
        Just c
          | c == quitKeyChar  -> pure Nothing
          | c == debugKeyChar -> do
              modifyIORef' debugRef' cycleDebug
              awaitKeyLoop acts debugRef' Nothing worldNow render'
          | c == '1' -> do
              journalOverlayLoop ctx display worldNow TabToday
              drainSDLEvents
              render' finalReveal
              render' finalReveal
              awaitKeyLoop acts debugRef' Nothing worldNow render'
          | Just a <- safeOptionIndexIn generalOptionKeys  c generals  -> pure (Just a)
          | Just a <- safeOptionIndexIn movementOptionKeys c movements -> pure (Just a)
          | otherwise -> awaitKeyLoop acts debugRef' Nothing worldNow render'

-- | Drive the journal overlay.  Pressing the number of a *different*
-- tab switches to it; pressing the number of the *current* tab
-- closes the overlay (so the key that opened the journal also
-- closes it).  Any other key dismisses.
journalOverlayLoop :: SDLContext -> ScenarioDisplay -> GameWorld -> JournalTab -> IO ()
journalOverlayLoop ctx display world tab = do
  renderJournalOverlay ctx display world tab
  mc <- awaitKeySDL
  case mc of
    Nothing -> pure ()
    Just c
      | c == currentTabKey tab -> pure ()
      | c == '1'               -> journalOverlayLoop ctx display world TabToday
      | c == '2'               -> journalOverlayLoop ctx display world TabPast
      | c == '3'               -> journalOverlayLoop ctx display world TabCatalog
      | otherwise              -> pure ()
  where
    currentTabKey TabToday   = '1'
    currentTabKey TabPast    = '2'
    currentTabKey TabCatalog = '3'

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
          mc <- waitOrKeyChar 33
          case mc of
            Just c  -> pure (Just c)
            Nothing -> loop start frameAt total

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
  liftIO $ writeIORef countRef newCount

-- ---------------------------------------------------------------------------
-- Full-frame typewriter: re-renders the entire screen each tick so both
-- back buffers always have consistent content.  Zero flicker.
-- ---------------------------------------------------------------------------

-- | Typewrite new messages by revealing one character at a time.
-- Each tick: renderWorldFrame (full clear+render) → overlay partial text → present.
-- A keypress skips to showing all text immediately.
typewriteFullFrame
  :: SDLContext -> LayoutConfig -> (GameWorld -> Maybe String)
  -> (Location -> Int)
  -> (Location -> Maybe Color)
  -> RevealFrame           -- ^ HUD frame to render under the typewriter
  -> CharId -> GameWorld -> [AnyAction]
  -> IORef [NarrativeEntry] -> IORef DebugMode -> IORef [AxiomTrace]
  -> FontContext -> Int
  -> [(Color, Int, String)]   -- ^ (color, delayMs, plainLine) for each new line
  -> IO ()
typewriteFullFrame _   _      _          _         _          _     _   _     _           _      _        _        _  _      []    = pure ()
typewriteFullFrame ctx layout statusLine sparkleFn zoneTintFn frame you world actions logRef debugRef traceRef fc labelW newLines = do
  let cols     = gridCols ctx
      -- Compute where the history area starts (mirrors renderWorldFrame)
      rows     = gridRows ctx
      contentW = cols - fromIntegral marginLeft * 2 - 8
  debugMode <- readIORef debugRef
  traces    <- readIORef traceRef
  allMsgs   <- readIORef logRef
  let learnRowCount = if debugMode == Learning
                        then length (learningModeLines traces you world)
                        else 0
      hud          = layoutHUD you world actions cols
      hasSpatial   = not (null (shSpatialCells hud))
      genLabels    = shGeneralLabels hud
      maxGenLen    = if null genLabels then 0 else maximum (map length genLabels)
      genColW      = maxGenLen + 3
      genNumCols   = max 1 ((cols - 4) `div` max 1 genColW)
      genRowCount  = length (chunksOf genNumCols genLabels)
      spatialH     = if hasSpatial then shBoxHeight hud else 0
      hudRows'     = 1 + 1 + genRowCount
                       + (if hasSpatial then 1 + spatialH else 0) + 1
      hudStartRow  = rows - hudRows'
      histTop      = topBarRows + learnRowCount
      histAvail    = hudStartRow - histTop - 1
      -- How many old history lines are visible
      allOrdered   = reverse allMsgs
      oldLabelW    = maximum (0 : map (length . neTimeLabel) allOrdered)
      useLabelW    = max labelW oldLabelW
      oldDispLines = concatMap (fmtOneEntry contentW useLabelW) allOrdered
      oldVisCount  = min (length oldDispLines) (max 0 histAvail)
      -- Row where new typewriter text starts
      twStartRow   = fromIntegral (histTop + oldVisCount)
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
msgColorSDL (MsgNarrate _)  t = tensionColor t
msgColorSDL (MsgEffect _)   t = tensionColor t
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
