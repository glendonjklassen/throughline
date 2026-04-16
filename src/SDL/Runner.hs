-- | SDL2 game runner: constructs a RuntimeUI that renders to an SDL2 window.
module SDL.Runner (sdlUI) where

import           Control.Monad           (when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (asks)
import           Control.Monad.State    (get)
import           Data.IORef
import           Foreign.C.Types (CInt)
import qualified SDL

import           Engine.Core.NarrativeMessage (NarrativeEntry(..), neTimeLabel,
                                               NarrativeMessage(..))
import           Engine.Headless       (ActionSource, StepHook, coreLoop)
import           Engine.Runtime        (RuntimeUI(..))
import           GameTypes
import           MonadStack

import           SDL.Animation
import           SDL.FontContext
import           SDL.InputHandler
import           SDL.Palette
import           SDL.Renderer

import           Terminal.ANSI         (stripAnsi, wrapWords)
import           Terminal.Display      (safeIndex)
import           Terminal.Debug        (cycleDebug, learningModeLines)
import           Terminal.Layout       (ScenarioDisplay(..), LayoutConfig(..))

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
      result <- runApp env world (sdlGameLoop ctx (sdLayout display) (sdStatusLine display))
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

sdlGameLoop :: SDLContext -> LayoutConfig -> (GameWorld -> Maybe String) -> App ()
sdlGameLoop ctx layout statusLine = do
  msgCountRef <- liftIO $ newIORef (0 :: Int)
  -- Stash last-rendered actions so the step hook can keep the HUD stable
  actionsRef  <- liftIO $ newIORef ([] :: [AnyAction])
  coreLoop (sdlStepHook ctx layout statusLine msgCountRef actionsRef)
           (sdlActionSource ctx layout statusLine msgCountRef actionsRef)

-- | Action source: render the world, await a keypress, map to an action.
sdlActionSource :: SDLContext -> LayoutConfig -> (GameWorld -> Maybe String)
                -> IORef Int -> IORef [AnyAction] -> ActionSource
sdlActionSource ctx layout statusLine countRef actionsRef actions = do
  world    <- get
  you      <- asks envPlayerCharId
  logRef   <- asks envMessageLog
  debugRef <- asks envDebug
  traceRef <- asks envAxiomTrace
  -- Stash current actions for the step hook
  liftIO $ writeIORef actionsRef actions
  -- Render the world with actions
  liftIO $ renderWorldSDL ctx layout statusLine you world actions logRef debugRef traceRef
  -- Update message count
  msgs <- liftIO $ readIORef logRef
  liftIO $ writeIORef countRef (length msgs)
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

sdlStepHook :: SDLContext -> LayoutConfig -> (GameWorld -> Maybe String)
            -> IORef Int -> IORef [AnyAction] -> StepHook
sdlStepHook ctx layout statusLine countRef actionsRef _before after _diff = do
  you      <- asks envPlayerCharId
  logRef   <- asks envMessageLog
  debugRef <- asks envDebug
  traceRef <- asks envAxiomTrace
  allMsgs  <- liftIO $ readIORef logRef
  prevCount <- liftIO $ readIORef countRef
  lastActions <- liftIO $ readIORef actionsRef
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
      -- Render the frame with OLD messages only (so we can typewrite new ones)
      -- Use the stashed actions to keep the HUD stable
      let oldMsgs = drop (newCount - prevCount) allMsgs  -- allMsgs is newest-first
      oldLogRef <- newIORef oldMsgs
      renderWorldSDL ctx layout statusLine you after lastActions oldLogRef debugRef traceRef
      -- Second render: populates both SDL2 back buffers with the static UI.
      -- With double buffering, one render only populates one buffer. The typewriter
      -- then alternates between a buffer with the compass and one without.
      renderWorldSDL ctx layout statusLine you after lastActions oldLogRef debugRef traceRef
      -- Typewrite new messages into the history area
      debugMode     <- readIORef debugRef
      traces        <- readIORef traceRef
      let learnRowCount = if debugMode == Learning
                            then length (learningModeLines traces you after)
                            else 0
      let cols     = gridCols ctx
          rows     = gridRows ctx
          contentW = cols - 4 - 8
          allOrdered = reverse allMsgs
          labelW   = maximum (0 : map (length . neTimeLabel) allOrdered)
          -- Replicate the HUD start row calculation from renderWorldSDL
          actionLabels  = zipWith (\n a -> show n <> ") " <> stripAnsi (anyActionLabel a))
                            [1 :: Int ..] lastActions
          maxLabelLen   = if null actionLabels then 0 else maximum (map length actionLabels)
          colWidth      = maxLabelLen + 3
          numCols       = max 1 ((cols - 4) `div` max 1 colWidth)
          actionRowCount = length (chunksOf numCols actionLabels)
          hudRows       = 2 + actionRowCount
          hudStartRow   = rows - hudRows
          -- histTop mirrors what renderWorldSDL computes (topBarRows + debug lines)
          histTop    = topBarRows + learnRowCount
          histAvail  = hudStartRow - histTop - 1
          -- Old history lines, capped to what is actually visible
          oldOrdered = reverse oldMsgs
          oldLines   = concatMap (fmtEntryPlain contentW labelW) oldOrdered
          visibleOldCount = min (length oldLines) (max 0 histAvail)
          startRow   = fromIntegral (histTop + visibleOldCount)
          fc         = sdlFont ctx
          rend       = sdlRenderer ctx
      -- Typewrite each new message
      typewriteNewMessages fc rend newMsgs contentW labelW startRow
      SDL.delay 400
  liftIO $ writeIORef countRef newCount

-- | Typewrite new messages character by character onto the rendered frame.
typewriteNewMessages :: FontContext -> SDL.Renderer -> [NarrativeEntry]
                     -> Int -> Int -> CInt -> IO ()
typewriteNewMessages _  _    []         _  _  _   = pure ()
typewriteNewMessages fc rend (msg:rest) cw lw row = do
  let color   = msgColorSDL (neMessage msg) (neTension msg)
      lines'  = fmtOneEntry cw lw msg
      delay   = beatDelay (neMessage msg)
  nextRow <- typewriteEntryLines fc rend lines' color delay 2 row
  typewriteNewMessages fc rend rest cw lw nextRow

-- | Typewrite formatted lines for one entry, return the next row.
typewriteEntryLines :: FontContext -> SDL.Renderer -> [String] -> Color
                    -> Int -> CInt -> CInt -> IO CInt
typewriteEntryLines _  _    []     _     _     _   row = pure row
typewriteEntryLines fc rend (l:ls) color delay col row = do
  typewriteString fc rend l color col row delay
  typewriteEntryLines fc rend ls color delay col (row + 1)

-- | Typewrite a single string character by character.
-- Accumulates typed characters and re-renders the full prefix each frame
-- so that double-buffered back-buffer swaps never lose prior characters.
typewriteString :: FontContext -> SDL.Renderer -> String -> Color
                -> CInt -> CInt -> Int -> IO ()
typewriteString _  _    []  _     _   _   _     = pure ()
typewriteString fc rend str color col row delay = go [] str
  where
    go typed [] = do
      renderText fc (reverse typed) color (col, row)
      SDL.present rend
    go typed (c:cs) = do
      let typed' = c : typed  -- prepend for O(1), reverse on render
      renderText fc (reverse typed') color (col, row)
      SDL.present rend
      quit <- pollQuit
      if quit
        then do
          -- Finish the rest instantly
          renderText fc (reverse typed' ++ cs) color (col, row)
          SDL.present rend
        else do
          SDL.delay (fromIntegral delay)
          go typed' cs

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

-- | Same as fmtOneEntry but returns (Color, String) pairs.
fmtEntryPlain :: Int -> Int -> NarrativeEntry -> [String]
fmtEntryPlain = fmtOneEntry

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
