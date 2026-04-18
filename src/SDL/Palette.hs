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
  , sparkleColor
  , sparkleGlyph
  , ageFadeColor
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

-- ---------------------------------------------------------------------------
-- Shiny sense + history fade
-- ---------------------------------------------------------------------------

-- | Color for a sparkle glyph on the spatial HUD, keyed by intensity (0-3).
-- Level 0 should not render; levels 1-3 pick pale-gold shades rising toward
-- a near-white peak.  Use alongside 'sparkleGlyph' to pick the glyph itself.
sparkleColor :: Int -> Color
sparkleColor n
  | n <= 0    = dimText
  | n == 1    = Color 140 128  78 255   -- faint wheat
  | n == 2    = Color 196 172  92 255   -- honey
  | otherwise = Color 232 210 140 255   -- pale gold

-- | Glyph to draw for a given sparkle intensity (0-3). Empty string for 0.
sparkleGlyph :: Int -> String
sparkleGlyph n
  | n <= 0    = ""
  | n == 1    = "."
  | n == 2    = "*"
  | otherwise = "\x2726"                  -- sparkle bullet

-- | Fade a base color by a 0-1 factor toward the background.
-- 1.0 keeps the color intact; 0.0 sinks it into the background.
-- Used to age out older history lines so the most recent text pops.
ageFadeColor :: Color -> Double -> Color
ageFadeColor (Color r g b a) factor =
  let f = max 0.0 (min 1.0 factor)
      Color br bg bb _ = bgColor
      blend :: Word8 -> Word8 -> Word8
      blend base fg =
        let bf = fromIntegral base :: Double
            ff = fromIntegral fg   :: Double
        in round (bf + (ff - bf) * f)
  in Color (blend br r) (blend bg g) (blend bb b) a

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
