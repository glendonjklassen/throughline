{-# OPTIONS_GHC -fno-hpc #-}
-- | Terminal rendering: breathing pulse animation thread and prompt display.
module Terminal.Render where

import           Control.Concurrent      (forkIO, killThread, threadDelay)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.IORef
import           System.IO
import           System.Random           (randomRIO)

import           Engine.Core.NarrativeMessage
import           Engine.Core.World      (engineStatusLine, exitBearings, narrate)
import           Engine.Headless        (ActionSource, StepHook, coreLoop)
import           Terminal.Layout
import           Terminal.ANSI
import           Terminal.Debug
import           Terminal.Display
import           GameTypes
import           MonadStack

-- ---------------------------------------------------------------------------
-- Interactive game loop
-- ---------------------------------------------------------------------------

gameLoop :: LayoutConfig -> (GameWorld -> Maybe String) -> App ()
gameLoop layout statusLine = do
  msgCountRef <- liftIO $ newIORef (0 :: Int)
  coreLoop (typewriterHook layout msgCountRef) (typewriterSource layout statusLine msgCountRef)

typewriterSource :: LayoutConfig -> (GameWorld -> Maybe String) -> IORef Int -> ActionSource
typewriterSource layout statusLine countRef actions = do
  renderWorld layout statusLine actions
  logRef <- asks envMessageLog
  msgs <- liftIO $ readIORef logRef
  liftIO $ writeIORef countRef (length msgs)
  -- Pre-compute values needed by both the pulse thread and the dim-on-keypress
  world        <- get
  you          <- asks envPlayerCharId
  (_, termW)   <- liftIO getTerminalSize
  let (leftW, _) = computePanelWidths layout termW
      engineS    = engineStatusLine you world
      scenarioS  = statusLine world
      compass    = buildCompassString [ (lbl, b) | (_, lbl, b) <- exitBearings you world ]
      promptRow  = length (buildStatusPart engineS scenarioS compass) + 1
      suffix     = dim " (q quit, d debug, m merge)"
  -- Spawn breathing pulse on the prompt line
  stepRef <- liftIO $ newIORef (0 :: Int)
  pulseThread <- liftIO $ forkIO $ breathingPulse stepRef promptRow leftW suffix
  result <- awaitKey actions
  liftIO $ killThread pulseThread
  case result of
    Just _ -> liftIO $ do
      let promptLine = "What do you do?"
          actionStrs = zipWith (\n a -> "  " <> show n <> ") " <> stripAnsi (anyActionLabel a))
                         [1 :: Int ..] actions
          dimLines   = promptLine : actionStrs
      mapM_ (\(r, s) -> moveCursor r 1 >> putStr (fitToWidth leftW (dim s)))
            (zip [promptRow ..] dimLines)
      hFlush stdout
    Nothing -> pure ()
  pure result

-- | Background thread that smoothly breathes the prompt brightness using a
-- sine wave over ANSI 256-color grayscale (codes 232–255, 24 shades).
-- One full cycle takes ~3.2s (40 steps × 80ms).
breathingPulse :: IORef Int -> Int -> Int -> String -> IO ()
breathingPulse stepRef row leftW suffix = go
  where
    totalSteps = 40
    stepDelay  = 80000  -- 80ms per step
    -- Grayscale range: 240 (bright) down to 244 (dim-ish). Subtle.
    lo = 242 :: Int
    hi = 255 :: Int
    go = do
      threadDelay stepDelay
      step <- readIORef stepRef
      let t     = fromIntegral step / fromIntegral totalSteps :: Double
          phase = (1 + sin (2 * pi * t)) / 2  -- 0..1 sine wave
          shade = lo + round (phase * fromIntegral (hi - lo))
          color s = "\ESC[38;5;" <> show shade <> "m" <> s <> "\ESC[0m"
      moveCursor row 1
      putStr (fitToWidth leftW (color "What do you do?" <> suffix))
      hFlush stdout
      writeIORef stepRef ((step + 1) `mod` totalSteps)
      go

typewriterHook :: LayoutConfig -> IORef Int -> StepHook
typewriterHook layout countRef before after diff = do
  debugBefore before >> debugAfter after >> debugWorldDiff diff
  logRef    <- asks envMessageLog
  allMsgs   <- liftIO $ readIORef logRef
  prevCount <- liftIO $ readIORef countRef
  let newCount = length allMsgs
      newMsgs  = reverse (take (newCount - prevCount) allMsgs)
  if null newMsgs
    then pure ()
    else do
      (termH, termW) <- liftIO getTerminalSize
      let (leftW, rightW) = computePanelWidths layout termW
          col             = leftW + 4   -- 1-based column where right-pane content starts
          maxRows         = termH - layoutBottomMargin layout
          allOrdered      = reverse allMsgs
          oldMsgs         = take prevCount allOrdered
          -- Compute label width from ALL messages so typewriter alignment matches
          labelW          = maximum (0 : map (length . neTimeLabel) allOrdered)
          oldLineCount    = length (buildHistoryLinesWith rightW labelW oldMsgs)
          startRow        = min maxRows oldLineCount + 1
      liftIO $ do
        histRef <- newIORef (buildHistoryLinesWith rightW labelW oldMsgs)
        let go _ [] = pure ()
            go row (entry:rest) = do
              let delay    = beatDelayMsg (neMessage entry)
                  fmtLines = buildHistoryLinesWith rightW labelW [entry]
              nextRow <- foldlM' row fmtLines $ \r line -> do
                modifyIORef' histRef (<> [line])
                if r <= maxRows
                  then do
                    moveCursor r col >> clearToEOL >> typewriteLine delay line
                    pure (r + 1)
                  else do
                    -- Scroll: redraw old lines instantly, typewrite new one at bottom
                    allHist <- readIORef histRef
                    let visible = takeLast maxRows allHist
                        old     = init visible
                        new     = last visible
                    mapM_ (\(ri, ln) -> do
                      moveCursor ri col
                      clearToEOL
                      putStr (fitToWidth rightW ln)
                      ) (zip [1..] old)
                    moveCursor maxRows col
                    clearToEOL
                    typewriteLine delay new
                    hFlush stdout
                    pure (maxRows + 1)
              go nextRow rest
        go startRow newMsgs
        threadDelay 400000
        drainInput
  liftIO $ writeIORef countRef newCount
  where
    foldlM' z xs f = go' z xs where
      go' acc []     = pure acc
      go' acc (x:rest) = f acc x >>= \acc' -> go' acc' rest

-- | Per-character typewriter delay based on the narrative message type.
beatDelayMsg :: NarrativeMessage -> Int
beatDelayMsg (MsgThink _ _)    = 35000
beatDelayMsg MsgSay {}         = 22000
beatDelayMsg (MsgNarrate _)    = 18000
beatDelayMsg (MsgEffect _)     = 18000
beatDelayMsg (MsgDialogue _)   = 22000

-- | Glitch intensity range based on tension level.
-- Returns (lo, hi) for randomRIO, or Nothing if below threshold.
glitchIntensity :: Int -> Maybe (Int, Int)
glitchIntensity t
  | t >= 8    = Just (5, 10)   -- buck fever / shot taken
  | t >= 6    = Just (3, 6)    -- closing in
  | t >= 4    = Just (1, 3)    -- fresh sign, same zone
  | otherwise = Nothing

-- | Corrupt random character positions in a list of screen lines.
glitchLines :: Int -> Int -> [String] -> IO [String]
glitchLines lo hi ls = do
  numGlitches <- randomRIO (lo, hi)
  go numGlitches ls
  where
    glitchChars = "░▒▓█▌▐╳╱╲┼┤├" :: String
    go 0 acc = pure acc
    go n acc = do
      row <- randomRIO (0, max 0 (length acc - 1))
      let line = stripAnsi (acc !! row)
      if null line then go (n - 1) acc
      else do
        col <- randomRIO (0, length line - 1)
        gc  <- (glitchChars !!) <$> randomRIO (0, length glitchChars - 1)
        let (before, after) = splitAt col line
            glitched = before ++ [gc] ++ drop 1 after
        go (n - 1) (take row acc ++ [glitched] ++ drop (row + 1) acc)

renderWorld :: LayoutConfig -> (GameWorld -> Maybe String) -> [AnyAction] -> App ()
renderWorld layout statusLine actions = do
  liftIO $ hideCursor >> clearScreen
  world          <- get
  you            <- asks envPlayerCharId
  logRef         <- asks envMessageLog
  traceRef       <- asks envAxiomTrace
  debugMode      <- liftIO . readIORef =<< asks envDebug
  allMsgs        <- liftIO (readIORef logRef)
  traces         <- liftIO (readIORef traceRef)
  (termH, termW) <- liftIO getTerminalSize
  let (leftW, rightW) = computePanelWidths layout termW
  let engineStatus   = engineStatusLine you world
      scenarioStatus = statusLine world
      compass        = buildCompassString [ (lbl, b) | (_, lbl, b) <- exitBearings you world ]
      statusPart     = buildStatusPart engineStatus scenarioStatus compass
  let learnLines = if debugMode == Learning
                     then learningModeLines traces you world
                     else []
  let promptLine  = bold "What do you do?" <> dim " (q quit, d debug, m merge)"
  let actionLines = zipWith (\n a -> grey ("  " <> show n <> ")") <> " " <> anyActionLabel a)
                      [1 :: Int ..] actions
  let leftLines   = statusPart <> learnLines <> [promptLine] <> actionLines
  let tension     = getTension world
  let histLines   = buildHistoryLines rightW (reverse allMsgs)
  let maxRows     = termH - layoutBottomMargin layout
  let rightLines  = takeLast maxRows histLines
  let pairs       = zip (leftLines <> repeat "") (rightLines <> repeat "")
  let sepChar     = separatorFor (lcTick (worldClock world))
  let screenLines = map (renderSplitRow leftW sepChar) (take maxRows pairs)
  liftIO $ do
    case glitchIntensity tension of
      Just (lo, hi) -> do
        glitched <- glitchLines lo hi screenLines
        mapM_ putStrLn glitched
        hFlush stdout
        threadDelay 80000
        hideCursor >> clearScreen
      Nothing -> pure ()
    mapM_ putStrLn screenLines
    putStrLn ""

awaitKey :: [AnyAction] -> App (Maybe AnyAction)
awaitKey actions = do
  liftIO showCursor
  log'  <- asks envLog
  input <- liftIO getChar
  case input of
    'q' -> liftIO (putStrLn "" >> log' (grey "Goodbye.")) >> pure Nothing
    'd' -> do
      ref <- asks envDebug
      liftIO $ do
        putStrLn ""
        cur <- readIORef ref
        let next = cycleDebug cur
        writeIORef ref next
        log' (grey ("Debug: " <> describeDebug next))
      awaitKey actions
    'm' -> do
      liveMerge <- asks envLiveMerge
      world <- get
      (merged, mergedPlayers) <- liftIO (liveMerge world)
      put merged
      case mergedPlayers of
        [] -> liftIO $ log' (grey "No new actions to merge.")
        _  -> mapM_ (\(name, count) ->
                narrate (MsgNarrate (name <> "'s actions arrived. (" <> show count <> " merged)")))
              mergedPlayers
      awaitKey actions
    _   -> case safeIndex input actions of
      Just a  -> pure (Just a)
      Nothing -> awaitKey actions

rawInputMode :: IO ()
rawInputMode = hSetBuffering stdin NoBuffering >> hSetEcho stdin False

cookedInputMode :: IO ()
cookedInputMode = showCursor >> hSetBuffering stdin LineBuffering >> hSetEcho stdin True
