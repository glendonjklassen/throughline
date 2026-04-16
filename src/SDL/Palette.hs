-- | Color palette for the SDL2 frontend.
-- Late-autumn prairie: desaturated, warm-shifting.
module SDL.Palette
  ( Color(..)
  , bgColor
  , defaultText, dimText, greyText
  , narratorColor, dialogueColor, thoughtColor
  , warningColor, errorColor
  , separatorColor
  , glitchColor
  , tensionColor
  , breathePulseColor
  ) where

import           Data.Word (Word8)

-- | RGBA color.
data Color = Color !Word8 !Word8 !Word8 !Word8
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Base tones
-- ---------------------------------------------------------------------------

-- | Background: soil dark, slightly warm.
bgColor :: Color
bgColor = Color 20 20 17 255

-- | Default text: parchment. Warm white, never blue.
defaultText :: Color
defaultText = Color 212 204 186 255

-- | Dim text: dried mud. Readable but receding.
dimText :: Color
dimText = Color 92 88 73 255

-- | Grey (UI chrome): weathered fence post.
greyText :: Color
greyText = Color 138 132 117 255

-- ---------------------------------------------------------------------------
-- Semantic colors
-- ---------------------------------------------------------------------------

-- | Dialogue: lichen on poplar bark. Slightly blue-green.
dialogueColor :: Color
dialogueColor = Color 143 175 167 255

-- | Internal thought: quieter than grey.
thoughtColor :: Color
thoughtColor = Color 107 101 88 255

-- | Warning: amber.
warningColor :: Color
warningColor = Color 196 148 58 255

-- | Error: dried blood on snow.
errorColor :: Color
errorColor = Color 166 93 93 255

-- | Separator: barely visible.
separatorColor :: Color
separatorColor = Color 61 58 51 255

-- ---------------------------------------------------------------------------
-- Tension gradient
-- ---------------------------------------------------------------------------

-- | Narrator color based on tension level (0-10).
narratorColor :: Int -> Color
narratorColor t
  | t <= 2    = Color 125 140 106 255  -- sage
  | t <= 4    = Color 154 154  90 255  -- yellow-green
  | t <= 6    = Color 181 164  78 255  -- dry grass
  | t <= 8    = Color 196 122  58 255  -- rust
  | otherwise = Color  92  88  73 255  -- dim (tunnel vision)

-- | Tension color for narrator text (alias for narratorColor).
tensionColor :: Int -> Color
tensionColor = narratorColor

-- | Glitch character color based on tension.
glitchColor :: Int -> Color
glitchColor t
  | t < 7     = Color 196 122  58 255  -- rust
  | otherwise = Color 166  93  93 255  -- dried blood

-- | Breathing pulse color: interpolate between dim and parchment.
-- phase is 0.0 to 1.0 (sine wave position).
breathePulseColor :: Double -> Color
breathePulseColor phase =
  let lo = 92 :: Double
      hi = 212 :: Double
      v  = round (lo + phase * (hi - lo)) :: Int
      -- Keep the warm tint: green channel slightly lower, blue lower still
      r  = fromIntegral (min 255 v)
      g  = fromIntegral (min 255 (v - 8))
      b  = fromIntegral (min 255 (v - 26))
  in Color r g b 255
