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
import           SDL.InputHandler (waitOrKey)
import           SDL.Text (stripAnsi, wrapWords)
import           Engine.Core.NarrativeMessage

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
        map (entry,) (fmtEntryPlain (fromIntegral rightW) labelW entry)
        ) msgs
  go startRow newLines
  where
    go _ [] = pure False
    go row ((entry, line):rest)
      | row >= maxRows = go maxRows rest  -- scroll not implemented yet, just cap
      | otherwise = do
          let delay   = beatDelay (neMessage entry)
              tension = neTension entry
              color   = messageColor (neMessage entry) tension
          typewriteOneLine fc rend line color startCol row delay
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
    messageColor (MsgNarrate _)  t = narratorColor t
    messageColor (MsgEffect _)   t = narratorColor t
    messageColor (MsgDialogue _) _ = dialogueColor

-- | Format a single entry into plain-text lines (no ANSI codes).
fmtEntryPlain :: Int -> Int -> NarrativeEntry -> [String]
fmtEntryPlain contentW labelW entry =
  let label   = neTimeLabel entry
      raw     = msgLinesPlain (neMessage entry)
      wrapped = concatMap (wrapWords (max 10 (contentW - labelW - 2))) raw
      pad     = replicate (labelW + 2) ' '
      labelPad = padToN (labelW + 2) label
  in case wrapped of
       []     -> []
       (l:ls) -> (labelPad <> l) : map (pad <>) ls

msgLinesPlain :: NarrativeMessage -> [String]
msgLinesPlain (MsgSay _ sName _ lNames text) =
  [sName <> fmtLis lNames <> ": " <> text]
msgLinesPlain (MsgThink _ text)     = ["~ " <> text]
msgLinesPlain (MsgNarrate text)     = ["> " <> stripAnsi text]
msgLinesPlain (MsgEffect text)      = ["  " <> stripAnsi text]
msgLinesPlain (MsgDialogue dls)     = map fmtDL dls
  where fmtDL (_, sName, _, lNames, text) =
          sName <> fmtLis lNames <> ": " <> text

fmtLis :: [String] -> String
fmtLis [] = ""
fmtLis ns = " (to " <> unwords ns <> ")"

padToN :: Int -> String -> String
padToN n s
  | length s >= n = s
  | otherwise     = s <> replicate (n - length s) ' '

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
      skip <- waitOrKey delayMs
      if skip
        then do
          renderText fc (reverse typed' ++ cs) color (startCol, row)
          SDL.present rend
        else go typed' cs

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
