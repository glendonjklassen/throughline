-- | Spatial action layout for the bottom HUD.  When the scenario has a
-- LocationGraph with coordinates, movement actions are positioned on screen
-- based on their compass direction from the player.  Non-movement actions
-- are listed linearly at the top.  Falls back to a flat grid when there
-- are no coordinates.
module SDL.SpatialHUD
  ( HUDCell(..)
  , SpatialHUD(..)
  , layoutHUD
  , terrainSprites
  ) where

import qualified Data.Map.Strict as Map

import           SDL.Text (stripAnsi)
import           GameTypes

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | A single rendered cell in the spatial HUD.
data HUDCell = HUDCell
  { hudLabel  :: String        -- ^ e.g. "4) North Road Ditch"
  , hudCol    :: Int           -- ^ column offset within the spatial box
  , hudRow    :: Int           -- ^ row offset within the spatial box
  } deriving (Show)

-- | Full layout of the bottom HUD.
data SpatialHUD = SpatialHUD
  { shGeneralLabels :: [String]   -- ^ non-movement actions, rendered linearly
  , shSpatialCells  :: [HUDCell]  -- ^ movement actions, positioned spatially
  , shPlayerMarker  :: (Int, Int) -- ^ (col, row) for the "me" marker
  , shBoxWidth      :: Int        -- ^ width of the spatial area in columns
  , shBoxHeight     :: Int        -- ^ height of the spatial area in rows
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- Classification
-- ---------------------------------------------------------------------------

-- | Extract the target Location from a movement action, if it has one.
movementTarget :: AnyAction -> Maybe Location
movementTarget act = go (anyActionEffects act)
  where
    go [] = Nothing
    go (eff:rest) = case effectBody eff of
      SetLocation _ loc -> Just loc
      _                 -> go rest

-- ---------------------------------------------------------------------------
-- Layout
-- ---------------------------------------------------------------------------

-- | Build the spatial HUD layout.  If the world has no location graph with
-- coordinates, or the player has no known location, returns a flat layout
-- (all actions in shGeneralLabels, no spatial cells).
layoutHUD :: CharId -> GameWorld -> [AnyAction] -> Int -> SpatialHUD
layoutHUD you world actions totalCols =
  case Map.lookup you (worldLocations world) of
    Nothing       -> flatLayout actions
    Just playerLoc ->
      let lg = worldLocationGraph world
      in case Map.lookup playerLoc (lgCoords lg) of
           Nothing     -> flatLayout actions
           Just (px, py) -> spatialLayout actions playerLoc (px, py) lg totalCols

-- | Fallback: all actions listed linearly.
flatLayout :: [AnyAction] -> SpatialHUD
flatLayout actions = SpatialHUD
  { shGeneralLabels = zipWith mkLabel [1 :: Int ..] actions
  , shSpatialCells  = []
  , shPlayerMarker  = (0, 0)
  , shBoxWidth      = 0
  , shBoxHeight     = 0
  }
  where
    mkLabel n act = show n <> ") " <> stripAnsi (anyActionLabel act)

-- | Spatial layout: split actions into movement/non-movement, position movement
-- actions by relative direction, scaled by actual graph distance.
spatialLayout :: [AnyAction] -> Location -> (Double, Double)
              -> LocationGraph -> Int -> SpatialHUD
spatialLayout actions _playerLoc (px, py) lg totalCols =
  let -- Number all actions sequentially
      numbered = zip [1 :: Int ..] actions

      -- Classify: movement actions with known target coords vs general
      classify (n, act) = case movementTarget act of
        Just targetLoc
          | Just (tx, ty) <- Map.lookup targetLoc (lgCoords lg)
          -> Right (n, act, tx - px, ty - py)
        _ -> Left (n, act)

      (generals, movements) = partitionEither (map classify numbered)

      -- General labels
      genLabels = map (\(n, act) -> show n <> ") " <> stripAnsi (anyActionLabel act)) generals

      -- Spatial box dimensions — use most of the screen width
      boxW = totalCols - 8
      boxH = 9                        -- enough vertical spread

      -- Center of the box (player position)
      centerCol = boxW `div` 2
      centerRow = boxH `div` 2

      -- Find the max distance among movements for relative scaling
      dists = [ sqrt (dx * dx + dy * dy) | (_, _, dx, dy) <- movements ]
      maxDist = if null dists then 1.0 else max 0.01 (maximum dists)

      -- Map each movement action to a grid cell, scaled relative to max distance
      cells = map (placeMovement boxW boxH centerCol centerRow maxDist) movements

      -- Resolve overlaps: nudge cells that collide with each other or the center
      resolved = resolveOverlaps centerCol centerRow boxW boxH cells

  in SpatialHUD
    { shGeneralLabels = genLabels
    , shSpatialCells  = resolved
    , shPlayerMarker  = (centerCol, centerRow)
    , shBoxWidth      = boxW
    , shBoxHeight     = boxH
    }

-- | Place a movement action in the spatial grid based on its relative direction.
-- Distance is scaled relative to the farthest neighbor so closer spots are
-- visibly nearer and farther spots reach the edges — irregular, like a map.
placeMovement :: Int -> Int -> Int -> Int -> Double
              -> (Int, AnyAction, Double, Double) -> HUDCell
placeMovement boxW boxH centerCol centerRow maxDist (n, act, dx, dy) =
  let label = show n <> ") " <> stripAnsi (anyActionLabel act)
      labelLen = length label
      -- Reserve space for the label itself
      maxColDisp = (boxW - labelLen) `div` 2 - 1
      maxRowDisp = centerRow - 1
      -- Proportional distance: 0.0 (here) to 1.0 (farthest neighbor)
      dist = sqrt (dx * dx + dy * dy)
      -- Scale with a minimum of 35% so nothing sits right on center,
      -- but close vs far is clearly different
      proportion = if dist < 0.001 then 0 else dist / maxDist
      scale = 0.35 + 0.65 * proportion
      angle = atan2 dx dy  -- angle from north, clockwise
      -- Convert to grid displacement
      colDisp = round (scale * fromIntegral maxColDisp * sin angle)
      rowDisp = round (scale * fromIntegral maxRowDisp * cos angle)
      rawCol  = centerCol + colDisp
      rawRow  = centerRow - rowDisp  -- screen y inverted
      -- Clamp to stay in bounds (account for label width)
      col = max 0 (min (boxW - labelLen) rawCol)
      row = max 0 (min (boxH - 1) rawRow)
  in HUDCell { hudLabel = label, hudCol = col, hudRow = row }

-- | Push cells apart when they'd overlap each other or the player marker.
resolveOverlaps :: Int -> Int -> Int -> Int -> [HUDCell] -> [HUDCell]
resolveOverlaps _cx _cy _bw _bh [] = []
resolveOverlaps cx cy bw bh cells = go [] cells
  where
    go placed [] = placed
    go placed (cell:rest) =
      let cell' = nudge placed cell
      in go (placed ++ [cell']) rest

    -- Check if two cells overlap (their labels would collide on the same row)
    overlaps :: HUDCell -> HUDCell -> Bool
    overlaps a b =
      hudRow a == hudRow b &&
      let aEnd = hudCol a + length (hudLabel a) + 1
          bEnd = hudCol b + length (hudLabel b) + 1
      in not (aEnd <= hudCol b || bEnd <= hudCol a)

    -- Check if a cell overlaps the player marker "@ You" at center
    overlapsCenter :: HUDCell -> Bool
    overlapsCenter cell =
      hudRow cell == cy &&
      let cEnd = hudCol cell + length (hudLabel cell) + 1
          markerLen = 5  -- "@ You"
          markerStart = cx - markerLen `div` 2 - 1
          markerEnd   = cx + markerLen `div` 2 + 2
      in not (cEnd <= markerStart || hudCol cell >= markerEnd)

    -- Try to nudge a cell to avoid overlapping placed cells and center
    nudge :: [HUDCell] -> HUDCell -> HUDCell
    nudge placed cell
      | not (any (overlaps cell) placed) && not (overlapsCenter cell) = cell
      | otherwise = tryRows [1..bh] cell
      where
        tryRows [] c = c  -- give up, use original position
        tryRows (offset:offsets) c =
          let candidates = [ c { hudRow = hudRow cell + offset }
                           , c { hudRow = hudRow cell - offset }
                           , c { hudRow = hudRow cell + offset
                               , hudCol = max 0 (hudCol cell + 3) }
                           , c { hudRow = hudRow cell - offset
                               , hudCol = max 0 (hudCol cell - 3) }
                           ]
              valid cand = hudRow cand >= 0 && hudRow cand < bh
                        && hudCol cand >= 0
                        && hudCol cand + length (hudLabel cand) <= bw
                        && not (any (overlaps cand) placed)
                        && not (overlapsCenter cand)
          in case filter valid candidates of
               (good:_) -> good
               []       -> tryRows offsets c

-- ---------------------------------------------------------------------------
-- Terrain art sprites — seeded scatter
-- ---------------------------------------------------------------------------

-- | Return ASCII art sprites for the current terrain.  Positions are
-- deterministically scattered using the location name as seed so each
-- spot looks different but is stable across frames.
terrainSprites :: CharId -> GameWorld -> Int -> Int -> [(Int, Int, String)]
terrainSprites you world boxW boxH =
  case Map.lookup you (worldLocations world) of
    Nothing  -> []
    Just loc ->
      let lg     = worldLocationGraph world
          region = Map.lookup loc (lgRegions lg)
          seed   = locHash loc
      in case region of
           Nothing -> []
           Just r  -> scatter seed r boxW boxH

-- | Simple hash of a location name for deterministic placement.
locHash :: Location -> Int
locHash (Location name) = foldl (\acc c -> acc * 37 + fromEnum c) 7 name

-- | Scatter sprites across the box using the seed to pick positions.
-- The region determines the sprite vocabulary; the seed makes each
-- location look different.
scatter :: Int -> Region -> Int -> Int -> [(Int, Int, String)]
scatter seed (Region name) bw bh
  | name `elem` ["NorthRoad", "SouthRoad", "WestRoad"] =
      roadScatter seed bw bh
  | name `elem` ["NorthField", "SouthField"] =
      fieldScatter seed bw bh
  | name == "OakRidge" =
      treeScatter seed [" _^_ ", " ||| ", "\\|/"] ["~:~", "o", ".:."] bw bh
  | name == "WillowBottom" =
      treeScatter seed [" )( ", " || ", "~~~"] ["!", "...", "~"] bw bh
  | name == "PoplarStand" =
      treeScatter seed ["/\\", "||", "||"] ["'", "_ _", "."] bw bh
  | name == "BushEdge" =
      treeScatter seed ["/\\", "||", "{::}"] [",,", ":.:", "o"] bw bh
  | otherwise = []

-- | Place road features using the seed for variation.
roadScatter :: Int -> Int -> Int -> [(Int, Int, String)]
roadScatter seed bw bh =
  let cx = bw `div` 2
      -- Road surface — always centered
      road = [ (cx - 3, r, "= = =") | r <- [0..bh-1], even r ]
          ++ [ (cx - 1, r, ". .") | r <- [1, 3 .. bh-1] ]
      -- Ditches — offset by seed
      dOff = (seed `mod` 3)
      ditchL = [ (cx - 9 - dOff, r, "~~") | r <- [0, 2 .. bh-1] ]
      ditchR = [ (cx + 6 + dOff, r, "~~") | r <- [1, 3 .. bh-1] ]
      -- Scattered gravel and grass using seed
      extras = seededSprites seed
                 [",", ".", ",,", ".", ","]
                 bw bh 10
      -- Fence posts at seed-dependent positions
      pCol1 = 2 + (seed `mod` 5)
      pCol2 = bw - 4 - (seed `mod` 6)
      posts = [ (pCol1, 0, "|"), (pCol2, bh-1, "|")
              , (pCol1 + 8, bh-1, "|"), (pCol2 - 6, 0, "|") ]
  in bounds bw bh (road ++ ditchL ++ ditchR ++ extras ++ posts)

-- | Place field features using the seed for variation.
fieldScatter :: Int -> Int -> Int -> [(Int, Int, String)]
fieldScatter seed bw bh =
  let -- Horizon
      horizon = [ (0, 0, replicate (min bw 50) '.') ]
      -- Stubble and grass scattered by seed
      stubble = seededSprites seed
                  [",", ",", ".", ",", ". ."]
                  bw bh 16
      -- Fence posts at seed-dependent positions
      pCol = 6 + (seed `mod` 10)
      pRow = 1 + (seed `mod` max 1 (bh - 3))
      posts = [ (pCol, pRow, "=|=---")
              , (bw - pCol - 4, bh - pRow - 1, "---|=") ]
  in bounds bw bh (horizon ++ stubble ++ posts)

-- | Place tree/forest features.  @treeGlyphs@ are 2-row tree sprites
-- (canopy + trunk + base), @floorGlyphs@ are ground scatter.
treeScatter :: Int -> [String] -> [String] -> Int -> Int -> [(Int, Int, String)]
treeScatter seed treeGlyphs floorGlyphs bw bh =
  let -- Place 4-6 trees at seed-scattered positions, avoiding center
      numTrees = 4 + (seed `mod` 3)
      treePlaces = take numTrees (seededPositions seed bw bh)
      trees = concatMap (\(i, (tc, tr)) ->
        let glyph r = treeGlyphs !! (r `mod` length treeGlyphs)
            -- Each tree takes 2-3 rows
            rows = filter (< bh) [tr, tr + 1, tr + 2]
        in [ (tc + (i `mod` 3), r, glyph (i + r)) | r <- rows ]
        ) (zip [0 :: Int ..] treePlaces)
      -- Ground scatter
      ground = seededSprites (seed + 17) floorGlyphs bw bh 12
  in bounds bw bh (trees ++ ground)

-- | Generate scattered sprite positions using a seed, avoiding the center.
seededSprites :: Int -> [String] -> Int -> Int -> Int -> [(Int, Int, String)]
seededSprites seed glyphs bw bh count =
  let positions = seededPositions seed bw bh
      centerCol = bw `div` 2
      centerRow = bh `div` 2
      -- Filter out positions too close to center (within 6 cols, 1 row)
      ok (c, r) = abs (c - centerCol) > 6 || abs (r - centerRow) > 1
      usable = filter ok (take (count * 2) positions)
  in take count
       [ (c, r, glyphs !! (i `mod` length glyphs))
       | (i, (c, r)) <- zip [0 :: Int ..] usable ]

-- | Deterministic pseudo-random positions across the box from a seed.
seededPositions :: Int -> Int -> Int -> [(Int, Int)]
seededPositions seed bw bh = go (abs seed + 1)
  where
    go s =
      let s1 = s * 1103515245 + 12345
          s2 = s1 * 1103515245 + 12345
          c  = abs (s1 `div` 65536) `mod` max 1 bw
          r  = abs (s2 `div` 65536) `mod` max 1 bh
      in (c, r) : go s2

-- | Filter sprites to those within bounds.
bounds :: Int -> Int -> [(Int, Int, String)] -> [(Int, Int, String)]
bounds bw bh = filter (\(c, r, s) -> c >= 0 && c + length s <= bw && r >= 0 && r < bh)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

partitionEither :: [Either a b] -> ([a], [b])
partitionEither = foldr step ([], [])
  where
    step (Left  a) (ls, rs) = (a : ls, rs)
    step (Right b) (ls, rs) = (ls, b : rs)
