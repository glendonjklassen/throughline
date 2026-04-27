-- | Reveal modal for first-time finds.  When the player turns up a
-- skull, an antler, the rusty car — anything with a sprite in
-- 'SDL.Sprites' — this modal pauses the game on a centered, scaled
-- rendering of the find.  Chunky 8-bit pixels enlarged to fill the
-- upper screen, ringed with sparkle particles, with the find's
-- prose flowing in below.
--
-- Visual treatment leans dream-like.  Two ghosts of the sprite are
-- drawn at low alpha and a few pixels offset (a soft chromatic
-- shimmer), the sparkle field drifts via 'drawSparkleParticles', and
-- the sprite itself breathes between ~85% and 100% alpha.  A brief
-- scan-fade entrance over ~700 ms lands on this static-but-living
-- frame.  Any key dismisses.
--
-- No-op (silent) when no sprite is registered for the name.  Trees
-- and ambient species fall through to their normal one-line
-- narration unchanged.
module SDL.FindReveal
  ( findRevealOverlay
  ) where

import           Control.Monad   (when, unless)
import           Data.Foldable   (for_)
import           Data.Word       (Word32)
import           Foreign.C.Types (CInt)
import qualified SDL

import           SDL.FontContext  (FontContext, cellWidth, cellHeight,
                                   renderText)
import           SDL.InputHandler (waitOrKey)
import           SDL.Palette
import           SDL.Primitives   (fillRectPx)
import           SDL.Renderer     (SDLContext (..), gridCols, gridRows,
                                   clearSDL, presentSDL)
import           SDL.Sprites      (Sprite, spriteByName, spriteBounds,
                                   drawSpriteScaled, drawSparkleParticles)
import           SDL.Text         (wrapWords)


-- | Show a reveal modal for the named find, if a sprite exists.
-- Blocks until the player presses a key.  Silent (no-op) if the name
-- has no sprite — those finds keep their existing prose-only
-- treatment.
findRevealOverlay :: SDLContext -> String -> String -> [String] -> IO ()
findRevealOverlay ctx kindLabel name proseLines =
  for_ (spriteByName name) (playReveal ctx kindLabel proseLines)


-- ---------------------------------------------------------------------------
-- Animation parameters
-- ---------------------------------------------------------------------------

-- | Pixel scale for the reveal sprite.  14 makes a 7×4-pixel sprite
-- about 100×56 px on screen — chunky and dominant in the upper half
-- of the modal without crowding the prose row.
revealScale :: CInt
revealScale = 14

-- | Wall-clock duration of the scan-fade entrance, in ms.  Short:
-- the player is here to *see* the find, not wait through a curtain.
revealMs :: Word32
revealMs = 350

-- | Frame budget for the breathe loop.  ~30 FPS is plenty for an
-- alpha pulse and the existing sparkle drift, and keeps the modal
-- responsive to a key press within one frame.
frameMs :: Int
frameMs = 33

-- | Chromatic-ghost offset in pixels.  Two ghosts of the sprite are
-- drawn at low alpha, one shifted left/up and one shifted right/down,
-- giving the reveal a soft "remembered" feel.  Wide enough that the
-- ghosts read as separate echoes rather than fringing.
ghostOffset :: CInt
ghostOffset = 10


-- ---------------------------------------------------------------------------
-- Modal body
-- ---------------------------------------------------------------------------

playReveal :: SDLContext -> String -> [String] -> Sprite -> IO ()
playReveal ctx kindLabel proseLines sprite = do
  let fc       = sdlFont ctx
      (sw, sh) = spriteBounds sprite
      cols     = gridCols ctx
      rows     = gridRows ctx
      cellW    = fromIntegral (cellWidth  fc) :: Int
      cellH    = fromIntegral (cellHeight fc) :: Int
      pxW      = sw * fromIntegral revealScale
      pxH      = sh * fromIntegral revealScale
      winPxW   = cols * cellW
      winPxH   = rows * cellH
      spriteOx = fromIntegral ((winPxW - pxW) `div` 2) :: CInt
      spriteOy = fromIntegral (winPxH `div` 5)         :: CInt
      headerTxt     = "\x2014  " <> kindLabel <> "  \x2014"
      hintTxt       = "any key to continue"
      pageW         = max 24 (cols - 8)
      wrapped       = concatMap (wrapWords pageW) proseLines
      headerRow     = max 1 (rows `div` 8)
      spriteEndRow  = (fromIntegral spriteOy + pxH) `div` cellH
      proseStartRow = spriteEndRow + 2
      hintRow       = rows - 3
      centerCol s   = fromIntegral (max 0 (cols `div` 2 - length s `div` 2))
  startMs <- SDL.ticks
  let loop = do
        nowMs <- SDL.ticks
        let elapsed = nowMs - startMs
            t :: Double
            t = if elapsed >= revealMs
                  then 1.0
                  else fromIntegral elapsed / fromIntegral revealMs
        clearSDL ctx
        renderText fc headerTxt chromeColor
          (centerCol headerTxt, fromIntegral headerRow)
        drawDreamSprite fc (spriteOx, spriteOy) sprite t
        when (t >= 1.0) $ do
          mapM_ (\(i, l) ->
            renderText fc l textColor
              (centerCol l, fromIntegral (proseStartRow + i)))
            (zip [0 :: Int ..] wrapped)
          renderText fc hintTxt dimTextColor
            (centerCol hintTxt, fromIntegral hintRow)
        presentSDL ctx
        pressed <- waitOrKey frameMs
        unless pressed loop
  loop


-- ---------------------------------------------------------------------------
-- Visual layers
-- ---------------------------------------------------------------------------

-- | Draw the sprite + chromatic ghosts + sparkle field at the given
-- entrance progress.  @t@ ramps the master alpha from 0 to 1 over the
-- entrance window; once @t >= 1@ a slow alpha breathe takes over so
-- the static frame still feels alive.
drawDreamSprite :: FontContext -> (CInt, CInt) -> Sprite -> Double -> IO ()
drawDreamSprite fc (ox, oy) sprite t = do
  let (sw, sh) = spriteBounds sprite
      pxW      = fromIntegral sw * revealScale :: CInt
      pxH      = fromIntegral sh * revealScale :: CInt
      cx       = ox + pxW `div` 2
      cy       = oy + pxH `div` 2
      seed     = sw * 17 + sh * 31
  ms <- SDL.ticks
  let secs    = fromIntegral ms / 1000.0 :: Double
      breathe = if t < 1.0
                  then t
                  else 0.85 + 0.15 * (0.5 + 0.5 * sin (secs * 2 * pi * 0.5))
      ghostA  = max 0 (min 0.30 (t * 0.30))
  -- Chromatic ghosts: two sprite copies offset by a few pixels at low
  -- alpha.  They land before the main sprite so the sprite's solid
  -- pixels overdraw them where they overlap.
  drawSpriteScaled fc (ox - ghostOffset, oy - ghostOffset `div` 2)
                   revealScale ghostA sprite
  drawSpriteScaled fc (ox + ghostOffset, oy + ghostOffset `div` 2)
                   revealScale ghostA sprite
  -- Sparkle field around the bounding box.  Rendered at level 3
  -- (~10 particles) and a 0.7 alpha scalar so it reads as ambient
  -- shimmer rather than confetti.
  when (t >= 0.4) $
    drawSparkleParticles fc (cx, cy) 3 seed (min 0.7 (t * 0.7))
                         (Color 232 210 140 255)
  -- The sprite itself, on top of its ghosts.
  drawSpriteScaled fc (ox, oy) revealScale breathe sprite
  -- During the entrance, mask the not-yet-revealed rows with the
  -- background color.  Reads as a slow scanline wipe top-to-bottom,
  -- which is what sells the "this is appearing for you" beat.
  when (t < 1.0) $ do
    let cutoff = round (t * fromIntegral pxH) :: CInt
        Color br bg bb _ = bgColor
    fillRectPx fc (Color br bg bb 255)
      (ox - ghostOffset - 2, oy + cutoff,
       pxW + 2 * ghostOffset + 4, pxH - cutoff + ghostOffset)
