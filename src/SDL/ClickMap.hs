-- | Tiny hit-test table for the launcher and overlays.
--
-- A menu builds one while rendering: for each clickable row it records
-- a rectangle and the 'Char' that clicking there should dispatch as.
-- The pick loop then treats 'ClickAt x y' identically to a keyboard
-- press by resolving the click through 'hitTest'.
--
-- Keeping the table flat (a list of rects) avoids polluting the
-- renderer with a widget framework — the overlays are short enough
-- that enumerating a handful of regions is simpler than anything
-- fancier.  Each region is described in *pixel* space; the builder
-- helpers here convert the grid coordinates the renderer uses.
module SDL.ClickMap
  ( ClickRect
  , ClickMap
  , emptyClickMap
  , hitTest
  , gridRowRect
  , gridRect
  ) where

import           SDL.FontContext (FontContext, cellHeight, cellWidth)

-- | A single clickable region.  @(x, y, w, h)@ in pixels, with the
-- character the click resolves to when a pointer lands inside it.
type ClickRect = (Int, Int, Int, Int, Char)

-- | An ordered list of 'ClickRect' — order matters only when regions
-- overlap (first match wins).  Menus render and build this together,
-- then hand it to the pick loop.
type ClickMap = [ClickRect]

emptyClickMap :: ClickMap
emptyClickMap = []

-- | Return the character associated with the region containing the
-- pixel, or 'Nothing' if the point hits no rect.  Linear scan; the
-- maps are always tiny (a menu has <20 rows).
hitTest :: ClickMap -> Int -> Int -> Maybe Char
hitTest cm px py = go cm
  where
    go [] = Nothing
    go ((x, y, w, h, c) : rest)
      | px >= x && px < x + w && py >= y && py < y + h = Just c
      | otherwise = go rest

-- | Build a full-width clickable row from a grid row index and a
-- dispatch char.  The rect spans every column so a click anywhere on
-- the row selects the option, not just on the visible label.
gridRowRect :: FontContext -> Int -> Int -> Int -> Char -> ClickRect
gridRowRect fc gridCols row widthCells c =
  let cw = fromIntegral (cellWidth fc)  :: Int
      ch = fromIntegral (cellHeight fc) :: Int
      _  = gridCols       -- kept as a parameter for future anchoring
  in (0, row * ch, widthCells * cw, ch, c)

-- | Build a rectangle at a specific (col, row) with a given cell
-- width and height.  Useful for buttons that don't span full rows,
-- e.g. left/right arrow hit zones in the settings menu.
gridRect :: FontContext -> Int -> Int -> Int -> Int -> Char -> ClickRect
gridRect fc col row wCells hCells c =
  let cw = fromIntegral (cellWidth fc)  :: Int
      ch = fromIntegral (cellHeight fc) :: Int
  in (col * cw, row * ch, wCells * cw, hCells * ch, c)
