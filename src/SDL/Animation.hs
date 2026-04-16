-- | SDL2 animation effects: typewriter, breathing pulse, glitch.
{-# LANGUAGE TupleSections #-}
module SDL.Animation
  ( typewriteLines
  , glitchFrame
  ) where

import           Foreign.C.Types (CInt)
import           System.Random   (randomRIO)
import qualified SDL
import           SDL.FontContext
import           SDL.Palette
import           SDL.InputHandler (pollQuit)
import           Engine.Core.NarrativeMessage
import           Terminal.ANSI (stripAnsi)
import           Terminal.Display (buildHistoryLinesWith)

-- ---------------------------------------------------------------------------
-- Typewriter
-- ---------------------------------------------------------------------------

-- | Type out new narrative entries one character at a time in the right pane.
-- Returns True if the user pressed quit during animation.
typewriteLines
  :: FontContext
  -> SDL.Renderer
  -> [NarrativeEntry]      -- ^ new messages to type out (oldest first)
  -> [NarrativeEntry]      -- ^ all messages (for label width calculation)
  -> CInt                  -- ^ right pane width in columns
  -> CInt                  -- ^ right pane start column
  -> CInt                  -- ^ max rows
  -> CInt                  -- ^ start row for new messages
  -> IO Bool
typewriteLines _  _    []   _  _  _  _    _        = pure False
typewriteLines fc rend msgs allMsgs rightW startCol maxRows startRow = do
  let labelW = maximum (0 : map (length . neTimeLabel) allMsgs)
      newLines = concatMap (\entry ->
        map (entry,) (buildHistoryLinesWith (fromIntegral rightW) labelW [entry])
        ) msgs
  go startRow newLines
  where
    go _ [] = pure False
    go row ((entry, line):rest)
      | row >= maxRows = go maxRows rest  -- scroll not implemented yet, just cap
      | otherwise = do
          let delay   = beatDelay (neMessage entry)
              plain   = stripAnsi line
              tension = neTension entry
              color   = messageColor (neMessage entry) tension
          typewriteOneLine fc rend plain color startCol row delay
          go (row + 1) rest

    beatDelay :: NarrativeMessage -> Int
    beatDelay (MsgThink _ _)    = 16
    beatDelay MsgSay {}         = 10
    beatDelay (MsgNarrate _)    = 8
    beatDelay (MsgEffect _)     = 8
    beatDelay (MsgDialogue _)   = 10

    messageColor :: NarrativeMessage -> Int -> Color
    messageColor MsgSay {}       _ = dialogueColor
    messageColor (MsgThink _ _)  _ = thoughtColor
    messageColor (MsgNarrate _)  t = tensionColor t
    messageColor (MsgEffect _)   t = tensionColor t
    messageColor (MsgDialogue _) _ = dialogueColor

-- | Type a single line character by character.
-- Re-renders the full accumulated prefix each frame so that
-- double-buffered back-buffer swaps never lose prior characters.
typewriteOneLine :: FontContext -> SDL.Renderer -> String -> Color -> CInt -> CInt -> Int -> IO ()
typewriteOneLine fc rend str color startCol row delayMs = go [] str
  where
    go typed [] = do
      renderText fc (reverse typed) color (startCol, row)
      SDL.present rend
    go typed (c:cs) = do
      let typed' = c : typed  -- prepend for O(1), reverse on render
      renderText fc (reverse typed') color (startCol, row)
      SDL.present rend
      quit <- pollQuit
      if quit
        then do
          renderText fc (reverse typed' ++ cs) color (startCol, row)
          SDL.present rend
        else do
          SDL.delay (fromIntegral delayMs)
          go typed' cs

-- ---------------------------------------------------------------------------
-- Glitch
-- ---------------------------------------------------------------------------

-- | Corrupt random cells on screen for a brief flash, then restore.
-- Only fires when tension >= 4.
glitchFrame :: FontContext -> SDL.Renderer -> Int -> CInt -> CInt -> IO ()
glitchFrame fc rend tension cols rows = do
  let (lo, hi) = glitchIntensity tension
  numGlitches <- randomRIO (lo, hi)
  -- Render glitch characters at random positions
  mapM_ (\_ -> do
    col <- randomRIO (0, cols - 1)
    row <- randomRIO (0, rows - 1)
    gc  <- (glitchChars !!) <$> randomRIO (0, length glitchChars - 1)
    renderChar fc gc (glitchColor tension) (col, row)
    ) [1..numGlitches :: Int]
  SDL.present rend
  SDL.delay 80  -- 80ms flash
  where
    glitchChars :: [Char]
    glitchChars = "░▒▓█▌▐╳╱╲┼┤├"

    glitchIntensity :: Int -> (Int, Int)
    glitchIntensity t
      | t >= 8    = (5, 10)
      | t >= 6    = (3, 6)
      | t >= 4    = (1, 3)
      | otherwise = (0, 0)
