{-# LANGUAGE TupleSections #-}
-- | SDL2 game runner: constructs a RuntimeUI that renders to an SDL2 window.
module SDL.Runner (sdlUI) where

import           Control.Monad           (unless, when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (asks)
import           Control.Monad.State    (get)
import           Data.IORef
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
import           SDL.SpatialHUD        (SpatialHUD(..), layoutHUD)
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
  coreLoop (sdlStepHook ctx display msgCountRef actionsRef)
           (sdlActionSource ctx display msgCountRef actionsRef)

-- | Action source: render the world, await a keypress, map to an action.
sdlActionSource :: SDLContext -> ScenarioDisplay
                -> IORef Int -> IORef [AnyAction] -> ActionSource
sdlActionSource ctx display countRef actionsRef actions = do
  world    <- get
  you      <- asks envPlayerCharId
  logRef   <- asks envMessageLog
  debugRef <- asks envDebug
  traceRef <- asks envAxiomTrace
  let layout     = sdLayout display
      statusLine = sdStatusLine display
      sparkleFn  = sdLocationSparkle display world you
      zoneTintFn = sdZoneTintFor display world
  -- Stash current actions for the step hook
  liftIO $ writeIORef actionsRef actions
  -- Render the world with actions (twice to populate both back buffers)
  liftIO $ renderWorldSDL ctx layout statusLine sparkleFn zoneTintFn you world actions logRef debugRef traceRef
  liftIO $ renderWorldSDL ctx layout statusLine sparkleFn zoneTintFn you world actions logRef debugRef traceRef
  -- Update message count
  msgs <- liftIO $ readIORef logRef
  liftIO $ writeIORef countRef (length msgs)
  -- Drain stale events (e.g. from typewriter skip) before awaiting fresh input
  liftIO drainSDLEvents
  -- Await input
  liftIO (awaitKeyLoop actions debugRef)
  where
    awaitKeyLoop :: [AnyAction] -> IORef DebugMode -> IO (Maybe AnyAction)
    awaitKeyLoop acts debugRef' = do
      mc <- awaitKeySDL
      case mc of
        Nothing   -> pure Nothing
        Just 'q'  -> pure Nothing
        Just 'd'  -> do
          modifyIORef' debugRef' cycleDebug
          awaitKeyLoop acts debugRef'
        Just 'm'  ->
          awaitKeyLoop acts debugRef'
        Just c    -> case safeIndex c acts of
          Just a  -> pure (Just a)
          Nothing -> awaitKeyLoop acts debugRef'

-- ---------------------------------------------------------------------------
-- Step hook: typewriter new messages onto the existing frame
-- ---------------------------------------------------------------------------

sdlStepHook :: SDLContext -> ScenarioDisplay
            -> IORef Int -> IORef [AnyAction] -> StepHook
sdlStepHook ctx display countRef actionsRef _before after _diff = do
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
      typewriteFullFrame ctx layout statusLine sparkleFn zoneTintFn you after lastActions
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
  -> CharId -> GameWorld -> [AnyAction]
  -> IORef [NarrativeEntry] -> IORef DebugMode -> IORef [AxiomTrace]
  -> FontContext -> Int
  -> [(Color, Int, String)]   -- ^ (color, delayMs, plainLine) for each new line
  -> IO ()
typewriteFullFrame _   _      _          _         _          _   _     _           _      _        _        _  _      []    = pure ()
typewriteFullFrame ctx layout statusLine sparkleFn zoneTintFn you world actions logRef debugRef traceRef fc labelW newLines = do
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
      renderWorldFrame ctx layout statusLine sparkleFn you world actions logRef debugRef traceRef
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
