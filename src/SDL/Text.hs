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
    -- * ANSI colour wrappers (for scenario end screens)
  , grey, green, yellow, red, cyan, bold, dim
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
