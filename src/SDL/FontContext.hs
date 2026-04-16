{-# LANGUAGE OverloadedStrings #-}
-- | Font loading and text rendering for the SDL2 frontend.
module SDL.FontContext
  ( FontContext(..)
  , initFont
  , freeFont
  , cellWidth
  , cellHeight
  , renderText
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

-- | Render a single character at a grid position (column, row).
renderChar :: FontContext -> Char -> Color -> (CInt, CInt) -> IO ()
renderChar fc c (Color r g b _a) (col, row) = do
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
  let x = col * fcCellW fc
      y = row * fcCellH fc
      dst = SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 (fcCellW fc) (fcCellH fc))
  SDL.copy (fcRenderer fc) tex Nothing (Just dst)
