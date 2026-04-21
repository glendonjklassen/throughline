{-# LANGUAGE OverloadedStrings #-}
-- | Font loading and text rendering for the SDL2 frontend.
module SDL.FontContext
  ( FontContext(..)
  , initFont
  , freeFont
  , cellWidth
  , cellHeight
  , renderText
  , renderTextAtPixel
  , renderTextTinted
  , renderChar
  ) where

import           Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.Text       as T
import           Data.Word (Word8)
import           Foreign.C.Types (CInt)
import qualified SDL
import qualified SDL.Font as SDLTTF
import           SDL.Palette (Color(..))

-- | Holds the loaded font and renderer reference.
data FontContext = FontContext
  { fcFont     :: SDLTTF.Font
  , fcRenderer :: SDL.Renderer
  , fcCellW    :: !CInt       -- ^ character cell width in pixels
  , fcCellH    :: !CInt       -- ^ character cell height in pixels
  , fcCache    :: IORef (Map.Map (Char, Word8, Word8, Word8) SDL.Texture)
  }

cellWidth :: FontContext -> CInt
cellWidth = fcCellW

cellHeight :: FontContext -> CInt
cellHeight = fcCellH

-- | Load the embedded font at the given point size.
initFont :: SDL.Renderer -> FilePath -> Int -> IO FontContext
initFont renderer fontPath ptSize = do
  SDLTTF.initialize
  font <- SDLTTF.load fontPath ptSize
  -- Measure a reference character to get cell dimensions
  (w, h) <- SDLTTF.size font "M"
  cache <- newIORef Map.empty
  pure FontContext
    { fcFont     = font
    , fcRenderer = renderer
    , fcCellW    = fromIntegral w
    , fcCellH    = fromIntegral h
    , fcCache    = cache
    }

-- | Clean up font resources.
freeFont :: FontContext -> IO ()
freeFont fc = do
  cache <- readIORef (fcCache fc)
  mapM_ SDL.destroyTexture (Map.elems cache)
  SDLTTF.free (fcFont fc)
  SDLTTF.quit

-- | Render a string at a grid position (column, row) in the given color.
renderText :: FontContext -> String -> Color -> (CInt, CInt) -> IO ()
renderText _ [] _ _ = pure ()
renderText fc str color (col, row) = go str col
  where
    go [] _ = pure ()
    go (c:cs) x = do
      renderChar fc c color (x, row)
      go cs (x + 1)

-- | Render a string starting at an absolute pixel position.  Breaks
-- out of the character-cell grid so callers can nudge labels by small
-- pixel offsets — useful for making the HUD feel like a sketched
-- scene rather than a terminal table.
renderTextAtPixel :: FontContext -> String -> Color -> (CInt, CInt) -> IO ()
renderTextAtPixel _ [] _ _ = pure ()
renderTextAtPixel fc str color (x0, y0) = go str x0
  where
    go [] _ = pure ()
    go (c:cs) x = do
      renderCharAtPixel fc c color (x, y0)
      go cs (x + fcCellW fc)

-- | Render a single character at an absolute pixel position.
renderCharAtPixel :: FontContext -> Char -> Color -> (CInt, CInt) -> IO ()
renderCharAtPixel fc c (Color r g b a) (x, y) = do
  let key = (c, r, g, b)
  cache <- readIORef (fcCache fc)
  tex <- case Map.lookup key cache of
    Just t  -> pure t
    Nothing -> do
      let sdlColor = SDL.V4 r g b 255
      surface <- SDLTTF.blended (fcFont fc) sdlColor (T.singleton c)
      t <- SDL.createTextureFromSurface (fcRenderer fc) surface
      SDL.freeSurface surface
      modifyIORef' (fcCache fc) (Map.insert key t)
      pure t
  SDL.textureAlphaMod tex SDL.$= a
  SDL.textureBlendMode tex SDL.$= SDL.BlendAlphaBlend
  let dst = SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 (fcCellW fc) (fcCellH fc))
  SDL.copy (fcRenderer fc) tex Nothing (Just dst)

-- | Render a string at a grid position with a filled background rectangle
-- spanning the text's cells.  The background alpha is taken from the
-- 'Color' value, so callers can pre-attenuate to get a soft halo.
renderTextTinted
  :: FontContext
  -> String
  -> Color          -- ^ background color (alpha respected)
  -> Color          -- ^ foreground text color
  -> (CInt, CInt)   -- ^ grid (col, row)
  -> IO ()
renderTextTinted _ [] _ _ _ = pure ()
renderTextTinted fc str (Color br bg bb ba) fg (col, row) = do
  let ren = fcRenderer fc
      x   = col * fcCellW fc
      y   = row * fcCellH fc
      w   = fromIntegral (length str) * fcCellW fc
      h   = fcCellH fc
      rect = SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 w h)
  SDL.rendererDrawBlendMode ren SDL.$= SDL.BlendAlphaBlend
  SDL.rendererDrawColor ren SDL.$= SDL.V4 br bg bb ba
  SDL.fillRect ren (Just rect)
  renderText fc str fg (col, row)

-- | Render a single character at a grid position (column, row).  The
-- color's alpha channel modulates the glyph's opacity via the texture's
-- alphaMod, so callers can fade glyphs without needing a new cache entry.
renderChar :: FontContext -> Char -> Color -> (CInt, CInt) -> IO ()
renderChar fc c (Color r g b a) (col, row) = do
  let key = (c, r, g, b)
  cache <- readIORef (fcCache fc)
  tex <- case Map.lookup key cache of
    Just t  -> pure t
    Nothing -> do
      let sdlColor = SDL.V4 r g b 255
      surface <- SDLTTF.blended (fcFont fc) sdlColor (T.singleton c)
      t <- SDL.createTextureFromSurface (fcRenderer fc) surface
      SDL.freeSurface surface
      modifyIORef' (fcCache fc) (Map.insert key t)
      pure t
  SDL.textureAlphaMod tex SDL.$= a
  SDL.textureBlendMode tex SDL.$= SDL.BlendAlphaBlend
  let x = col * fcCellW fc
      y = row * fcCellH fc
      dst = SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 (fcCellW fc) (fcCellH fc))
  SDL.copy (fcRenderer fc) tex Nothing (Just dst)
