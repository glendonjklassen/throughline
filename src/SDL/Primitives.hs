-- | Pixel-level drawing primitives on top of SDL2.  The rest of the
-- frontend thinks in character cells; these helpers bridge to pixels for
-- effects the text grid cannot express — alpha fades, colored halos
-- behind labels, subpixel markers.
--
-- All drawing goes through the 'SDL.Renderer' exposed on 'FontContext'
-- so primitives and text end up on the same back buffer.
module SDL.Primitives
  ( fillCell
  , fillCellAlpha
  , fillCellsAlpha
  , fillRectPx
  , drawLinePx
  ) where

import           Data.Word       (Word8)
import           Foreign.C.Types (CInt)
import qualified SDL

import           SDL.FontContext (FontContext (..))
import           SDL.Palette     (Color (..))

-- | Fill a single character cell with a solid color.
fillCell :: FontContext -> Color -> (Int, Int) -> IO ()
fillCell fc color (col, row) =
  fillCellAlpha fc color 1.0 (col, row)

-- | Fill a single character cell with the given color at a 0-1 alpha.
fillCellAlpha :: FontContext -> Color -> Double -> (Int, Int) -> IO ()
fillCellAlpha fc color alpha (col, row) =
  fillCellsAlpha fc color alpha 1 1 (col, row)

-- | Fill a rectangular block of cells (width and height in cells) at a
-- given alpha.  Useful for a halo that spans a whole label.
fillCellsAlpha :: FontContext -> Color -> Double -> Int -> Int -> (Int, Int) -> IO ()
fillCellsAlpha fc (Color r g b _) alpha widthCells heightCells (col, row) = do
  let x   = fromIntegral col * fcCellW fc
      y   = fromIntegral row * fcCellH fc
      w   = fromIntegral widthCells  * fcCellW fc
      h   = fromIntegral heightCells * fcCellH fc
      a   = clampAlphaByte alpha
      ren = fcRenderer fc
  SDL.rendererDrawBlendMode ren SDL.$= SDL.BlendAlphaBlend
  SDL.rendererDrawColor ren SDL.$= SDL.V4 r g b a
  let rect = SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 w h)
  SDL.fillRect ren (Just rect)

-- | Fill an arbitrary pixel rectangle (x, y, w, h).  Alpha is taken from
-- the color itself; callers that want to modulate alpha should pass a
-- 'Color' whose fourth channel reflects that.
fillRectPx :: FontContext -> Color -> (CInt, CInt, CInt, CInt) -> IO ()
fillRectPx fc (Color r g b a) (x, y, w, h) = do
  let ren = fcRenderer fc
  SDL.rendererDrawBlendMode ren SDL.$= SDL.BlendAlphaBlend
  SDL.rendererDrawColor ren SDL.$= SDL.V4 r g b a
  let rect = SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 w h)
  SDL.fillRect ren (Just rect)

-- | Draw a straight line between two pixel points.
drawLinePx :: FontContext -> Color -> (CInt, CInt) -> (CInt, CInt) -> IO ()
drawLinePx fc (Color r g b a) (x1, y1) (x2, y2) = do
  let ren = fcRenderer fc
  SDL.rendererDrawBlendMode ren SDL.$= SDL.BlendAlphaBlend
  SDL.rendererDrawColor ren SDL.$= SDL.V4 r g b a
  SDL.drawLine ren (SDL.P (SDL.V2 x1 y1)) (SDL.P (SDL.V2 x2 y2))

-- | Convert a 0-1 alpha double to an 0-255 byte, clamped.
clampAlphaByte :: Double -> Word8
clampAlphaByte a
  | a <= 0    = 0
  | a >= 1    = 255
  | otherwise = round (a * 255)
