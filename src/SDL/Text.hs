-- | Pure text utilities: ANSI stripping, word wrapping, colour helpers.
-- Extracted from Terminal.ANSI for SDL-only builds.
module SDL.Text
  ( -- * ANSI stripping and measurement
    stripAnsi
  , visibleLength
  , padRight
  , fitToWidth
    -- * Word wrapping
  , wrapWords
    -- * ANSI colour wrappers (for scenario end screens).  Prefixed
    -- @ansi@ so scenarios can use plain-English colour identifiers
    -- locally without shadowing the engine's wrappers.
  , ansiGrey, ansiGreen, ansiYellow, ansiRed, ansiCyan, ansiBold, ansiDim
  ) where

import Data.Char (isAlpha)

-- ---------------------------------------------------------------------------
-- ANSI stripping
-- ---------------------------------------------------------------------------

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

-- ---------------------------------------------------------------------------
-- Word wrapping
-- ---------------------------------------------------------------------------

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

-- ---------------------------------------------------------------------------
-- ANSI colour wrappers
-- ---------------------------------------------------------------------------

ansiGrey :: String -> String
ansiGrey s = "\ESC[90m" <> s <> "\ESC[0m"

ansiGreen :: String -> String
ansiGreen s = "\ESC[32m" <> s <> "\ESC[0m"

ansiYellow :: String -> String
ansiYellow s = "\ESC[33m" <> s <> "\ESC[0m"

ansiRed :: String -> String
ansiRed s = "\ESC[31m" <> s <> "\ESC[0m"

ansiCyan :: String -> String
ansiCyan s = "\ESC[36m" <> s <> "\ESC[0m"

ansiBold :: String -> String
ansiBold s = "\ESC[1m" <> s <> "\ESC[0m"

ansiDim :: String -> String
ansiDim s = "\ESC[2m" <> s <> "\ESC[0m"
