-- | Spatial action layout for the bottom HUD.  When the scenario has a
-- LocationGraph with coordinates, movement actions are positioned on screen
-- based on their compass direction from the player.  Non-movement actions
-- are listed linearly at the top.  Falls back to a flat grid when there
-- are no coordinates.
module SDL.SpatialHUD
  ( HUDCell(..)
  , SpatialHUD(..)
  , TerrainSprite(..)
  , TrailMark(..)
  , layoutHUD
  , terrainSprites
  , trailMarks
  ) where

import           Data.List       (find)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (catMaybes)

import           SDL.Palette (Color, separatorColor)
import           SDL.Text    (stripAnsi)
import           GameTypes

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | A single rendered cell in the spatial HUD.
--
-- 'hudTint' overrides the default foreground color (used for familiarity
-- shading); 'hudHalo' requests a soft filled background behind the label
-- (used for per-zone biome cues).  Both are optional — 'Nothing' preserves
-- today's look.
data HUDCell = HUDCell
  { hudLabel  :: String          -- ^ e.g. "4) North Road Ditch"
  , hudCol    :: Int             -- ^ column offset within the spatial box
  , hudRow    :: Int             -- ^ row offset within the spatial box
  , hudTarget :: Maybe Location  -- ^ target location for movement cells, Nothing for other actions
  , hudTint   :: Maybe Color     -- ^ optional foreground color override
  , hudHalo   :: Maybe Color     -- ^ optional background tint (alpha-respected)
  } deriving (Show)

-- | A breadcrumb dropped at a neighbor HUD cell, marking a location the
-- player recently departed.  Alpha decays with age so the trail fades
-- behind the player.
data TrailMark = TrailMark
  { tmCol   :: Int
  , tmRow   :: Int
  , tmGlyph :: String
  , tmAge   :: Int      -- ^ 0 = most recent (just left), larger = older
  } deriving (Show)

-- | A single terrain glyph on the spatial HUD, with its own color and
-- alpha so scatter density, trail fades, and other ambient cues can
-- modulate without touching the layout pipeline.
data TerrainSprite = TerrainSprite
  { tsCol   :: Int
  , tsRow   :: Int
  , tsGlyph :: String
  , tsColor :: Color
  , tsAlpha :: Double   -- ^ 0-1
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
  let target = movementTarget act
      label = show n <> ") " <> stripAnsi (anyActionLabel act)
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
  in HUDCell
       { hudLabel  = label
       , hudCol    = col
       , hudRow    = row
       , hudTarget = target
       , hudTint   = Nothing
       , hudHalo   = Nothing
       }

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
-- spot looks different but is stable across frames.  Density scales with
-- how deep in its zone the player's location is: edge locations feel
-- sparse, interior ones feel dense.
terrainSprites :: CharId -> GameWorld -> Int -> Int -> [TerrainSprite]
terrainSprites you world boxW boxH =
  case Map.lookup you (worldLocations world) of
    Nothing  -> []
    Just loc ->
      let lg         = worldLocationGraph world
          region     = Map.lookup loc (lgRegions lg)
          seed       = locHash loc
          interior   = zoneInteriorness lg loc region
          raw        = case region of
            Nothing -> []
            Just r  -> scatter seed r interior boxW boxH
      in map (uncolor separatorColor 1.0) raw

-- | How deep inside its zone a location sits.  1.0 = at the zone
-- centroid, 0.0 = at the far edge.  Returns 1.0 for zones with only a
-- single location (nowhere else to be) and 0.5 when anything is
-- undefined (fall back to "middle of the zone").
zoneInteriorness :: LocationGraph -> Location -> Maybe Region -> Double
zoneInteriorness _  _   Nothing   = 0.5
zoneInteriorness lg loc (Just r)  =
  let sameZone = [ (l, c) | (l, c) <- Map.toList (lgCoords lg)
                          , Map.lookup l (lgRegions lg) == Just r ]
  in case (Map.lookup loc (lgCoords lg), sameZone) of
       (Just (px, py), _:_:_) ->
         let coords      = map snd sameZone
             n           = fromIntegral (length coords) :: Double
             (cx, cy)    = ( sum (map fst coords) / n
                           , sum (map snd coords) / n )
             distFrom (qx, qy) = sqrt ((qx - cx) ** 2 + (qy - cy) ** 2)
             myDist      = distFrom (px, py)
             maxDist     = max 0.001 (maximum (map distFrom coords))
             edgeness    = min 1.0 (myDist / maxDist)
         in 1.0 - edgeness
       _ -> 1.0   -- solitary zone or no coords — call it "interior"

-- | Wrap a raw (col,row,glyph) triple into a 'TerrainSprite' with a
-- uniform color and alpha.  The scatter helpers stay in their simple
-- triple form; color modulation lives at this boundary.
uncolor :: Color -> Double -> (Int, Int, String) -> TerrainSprite
uncolor color alpha (c, r, g) = TerrainSprite
  { tsCol = c, tsRow = r, tsGlyph = g, tsColor = color, tsAlpha = alpha }

-- | Simple hash of a location name for deterministic placement.
locHash :: Location -> Int
locHash (Location name) = foldl (\acc c -> acc * 37 + fromEnum c) 7 name

-- ---------------------------------------------------------------------------
-- Trail marks — breadcrumbs on recently departed locations
-- ---------------------------------------------------------------------------

-- | For each entry in the player's location history that happens to be a
-- neighbor of the current position (i.e. shown as a movement cell in the
-- HUD), emit a 'TrailMark' co-located with that cell.  Entries that don't
-- land on a visible neighbor are silently dropped — they still live in
-- the history deque, just off-screen from this viewpoint.
trailMarks :: CharId -> GameWorld -> SpatialHUD -> [TrailMark]
trailMarks cid world hud =
  let history = Map.findWithDefault [] cid (worldLocationHistory world)
      cells   = shSpatialCells hud
      lookupCell loc = find (\c -> hudTarget c == Just loc) cells
      mk age loc = do
        cell <- lookupCell loc
        let glyph = trailGlyphFor world loc
            -- Place the glyph just past the end of the label so it sits
            -- between the neighbor name and the board edge without
            -- overlapping either the label or the sparkle (which is on
            -- the player-side of the label).
            colEnd = hudCol cell + length (hudLabel cell) + 1
        pure TrailMark
          { tmCol   = colEnd
          , tmRow   = hudRow cell
          , tmGlyph = glyph
          , tmAge   = age
          }
  in catMaybes (zipWith mk [0 ..] history)

-- | Pick a glyph for a trail mark based on the location's region.  Field
-- locations leave boot-in-stubble dots; bush and ridge locations leave
-- broken-twig marks; water-adjacent locations leave wet prints.  Falls
-- back to a neutral breadcrumb if region info is missing.
trailGlyphFor :: GameWorld -> Location -> String
trailGlyphFor world loc =
  case Map.lookup loc (lgRegions (worldLocationGraph world)) of
    Just (Region r)
      | r `elem` ["NorthField", "SouthField", "FieldBreak"] -> "\x2024"  -- one-dot leader
      | r `elem` ["NorthRoad", "SouthRoad", "WestRoad"]     -> "\x00b7"  -- middle dot
      | r == "WillowBottom" || r == "CreekBed"              -> "\x2234"  -- therefore (wet prints)
      | otherwise                                           -> ","
    Nothing                                                 -> ","

-- | Scatter sprites across the box using the seed to pick positions.
-- The region determines the sprite vocabulary; the seed makes each
-- location look different; @interior@ (0-1, 0 = edge of zone, 1 =
-- deepest) scales density.
scatter :: Int -> Region -> Double -> Int -> Int -> [(Int, Int, String)]
scatter seed (Region name) interior bw bh
  | name `elem` ["NorthRoad", "SouthRoad", "WestRoad"] =
      roadScatter seed interior bw bh
  | name `elem` ["NorthField", "SouthField"] =
      fieldScatter seed interior bw bh
  | name == "OakRidge" =
      treeScatter seed interior [" _^_ ", " ||| ", "\\|/"] ["~:~", "o", ".:."] bw bh
  | name == "WillowBottom" =
      treeScatter seed interior [" )( ", " || ", "~~~"] ["!", "...", "~"] bw bh
  | name == "PoplarStand" =
      treeScatter seed interior ["/\\", "||", "||"] ["'", "_ _", "."] bw bh
  | name == "BushEdge" =
      treeScatter seed interior ["/\\", "||", "{::}"] [",,", ":.:", "o"] bw bh
  | otherwise = []

-- | Scale an integer count by a density factor derived from zone
-- interiorness.  Edge locations get ~50% of base; interior locations get
-- ~120%.  Clamped at 1 so a very-edge location still has *something* on
-- screen.
scaleCount :: Double -> Int -> Int
scaleCount interior base =
  let factor = 0.5 + 0.7 * interior
  in max 1 (round (fromIntegral base * factor :: Double))

-- | Place road features using the seed for variation.  The road surface
-- and ditches are structural and ignore the density factor; only the
-- scattered gravel/grass shrinks at zone edges.
roadScatter :: Int -> Double -> Int -> Int -> [(Int, Int, String)]
roadScatter seed interior bw bh =
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
                 bw bh (scaleCount interior 10)
      -- Fence posts at seed-dependent positions
      pCol1 = 2 + (seed `mod` 5)
      pCol2 = bw - 4 - (seed `mod` 6)
      posts = [ (pCol1, 0, "|"), (pCol2, bh-1, "|")
              , (pCol1 + 8, bh-1, "|"), (pCol2 - 6, 0, "|") ]
  in bounds bw bh (road ++ ditchL ++ ditchR ++ extras ++ posts)

-- | Place field features using the seed for variation.  Stubble density
-- follows zone interiorness so standing at a field edge reads more open
-- than standing in the middle of one.
fieldScatter :: Int -> Double -> Int -> Int -> [(Int, Int, String)]
fieldScatter seed interior bw bh =
  let -- Horizon
      horizon = [ (0, 0, replicate (min bw 50) '.') ]
      -- Stubble and grass scattered by seed
      stubble = seededSprites seed
                  [",", ",", ".", ",", ". ."]
                  bw bh (scaleCount interior 16)
      -- Fence posts at seed-dependent positions
      pCol = 6 + (seed `mod` 10)
      pRow = 1 + (seed `mod` max 1 (bh - 3))
      posts = [ (pCol, pRow, "=|=---")
              , (bw - pCol - 4, bh - pRow - 1, "---|=") ]
  in bounds bw bh (horizon ++ stubble ++ posts)

-- | Place tree/forest features.  @treeGlyphs@ are 2-row tree sprites
-- (canopy + trunk + base), @floorGlyphs@ are ground scatter.  Both tree
-- and ground density scale with zone interiorness so walking to the
-- edge of the bush visibly thins the canopy.
treeScatter :: Int -> Double -> [String] -> [String] -> Int -> Int -> [(Int, Int, String)]
treeScatter seed interior treeGlyphs floorGlyphs bw bh =
  let -- Place 4-6 trees at base density, scaled by interiorness.
      numTrees = scaleCount interior (4 + (seed `mod` 3))
      treePlaces = take numTrees (seededPositions seed bw bh)
      trees = concatMap (\(i, (tc, tr)) ->
        let glyph r = treeGlyphs !! (r `mod` length treeGlyphs)
            -- Each tree takes 2-3 rows
            rows = filter (< bh) [tr, tr + 1, tr + 2]
        in [ (tc + (i `mod` 3), r, glyph (i + r)) | r <- rows ]
        ) (zip [0 :: Int ..] treePlaces)
      -- Ground scatter
      ground = seededSprites (seed + 17) floorGlyphs bw bh
                 (scaleCount interior 12)
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
