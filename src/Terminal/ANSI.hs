-- | ANSI terminal helpers: color wrappers, text measurement, word wrapping, and typewriter effect.
module Terminal.ANSI where

import           Control.Concurrent              (threadDelay)
import           Control.Monad                   (when)
import           Data.Char                       (isAlpha)
import qualified System.Console.Terminal.Size    as TS
import           System.IO                       (hFlush, hReady, stdin, stdout)

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H" >> hFlush stdout

-- | Move the cursor to a 1-based (row, col) position.
moveCursor :: Int -> Int -> IO ()
moveCursor row col = putStr ("\ESC[" <> show row <> ";" <> show col <> "H") >> hFlush stdout

-- | Erase from the cursor position to the end of the line.
clearToEOL :: IO ()
clearToEOL = putStr "\ESC[K" >> hFlush stdout

hideCursor :: IO ()
hideCursor = putStr "\ESC[?25l" >> hFlush stdout

showCursor :: IO ()
showCursor = putStr "\ESC[?25h" >> hFlush stdout

-- | Consume any buffered keypresses so they don't leak into the next prompt.
drainInput :: IO ()
drainInput = do
  ready <- hReady stdin
  when ready $ getChar >> drainInput

grey :: String -> String
grey s = "\ESC[90m" <> s <> "\ESC[0m"

green :: String -> String
green s = "\ESC[32m" <> s <> "\ESC[0m"

yellow :: String -> String
yellow s = "\ESC[33m" <> s <> "\ESC[0m"

red :: String -> String
red s = "\ESC[31m" <> s <> "\ESC[0m"

cyan :: String -> String
cyan s = "\ESC[36m" <> s <> "\ESC[0m"

bold :: String -> String
bold s = "\ESC[1m" <> s <> "\ESC[0m"

dim :: String -> String
dim s = "\ESC[2m" <> s <> "\ESC[0m"

-- | Strip ANSI escape sequences, leaving only visible characters.
stripAnsi :: String -> String
stripAnsi []              = []
stripAnsi ('\ESC':'[':cs) =
  case dropWhile (not . isAlpha) cs of
    []     -> []
    (_:xs) -> stripAnsi xs
stripAnsi (c:cs) = c : stripAnsi cs

-- | Number of visible (non-escape) characters in a string.
visibleLength :: String -> Int
visibleLength = length . stripAnsi

-- | Pad a string to n visible characters by appending spaces.
padRight :: Int -> String -> String
padRight n s = s <> replicate (max 0 (n - visibleLength s)) ' '

-- | Fit a string to exactly n visible characters: truncate (dropping ANSI) or pad.
fitToWidth :: Int -> String -> String
fitToWidth n s
  | visibleLength s <= n = padRight n s
  | otherwise            = take n (stripAnsi s)

-- | Word-wrap plain text to a given visible width. Returns a list of lines.
wrapWords :: Int -> String -> [String]
wrapWords w s = go (words s) [] 0
  where
    go [] []  _   = []
    go [] cur _   = [unwords (reverse cur)]
    go (word:rest) cur len
      | null cur                        = go rest [word] (length word)
      | len + 1 + length word <= w      = go rest (word : cur) (len + 1 + length word)
      | otherwise                       = unwords (reverse cur) : go (word : rest) [] 0

-- | Print a string with a per-visible-character delay.  ANSI escape sequences
-- are emitted instantly so colours render correctly.  If a key is pressed
-- during the animation the remaining text prints immediately and the
-- triggering keypress is consumed.
typewriteLine :: Int -> String -> IO ()
typewriteLine delayUs = go
  where
    go []              = pure ()
    go ('\ESC':'[':cs) =
      let (params, rest) = break isAlpha cs
      in case rest of
           []     -> putStr ("\ESC[" <> params) >> hFlush stdout
           (x:xs) -> putStr ("\ESC[" <> params <> [x]) >> go xs
    go (c:cs) = do
      putChar c
      hFlush stdout
      ready <- hReady stdin
      if ready
        then do _ <- getChar
                putStr cs >> hFlush stdout
        else threadDelay delayUs >> go cs

-- | Query the terminal dimensions. Returns (height, width); falls back to (24, 80).
getTerminalSize :: IO (Int, Int)
getTerminalSize = do
  s <- TS.size
  return $ case s of
    Just (TS.Window h w) -> (h, w)
    Nothing              -> (24, 80)
