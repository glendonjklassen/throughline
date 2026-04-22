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
  , terrainSpriteScatter
  , SpritePlacement(..)
  , trailMarks
  ) where

import           Data.List       (find, sortOn)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (catMaybes)

import           SDL.InputHandler (generalOptionKeys, movementOptionKeys, poolKeyFor)
import           SDL.Palette      (Color, separatorColor)
import           SDL.Text         (stripAnsi)
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
  , hudDist   :: Double          -- ^ graph distance from player (for reveal order)
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
  let prevLoc = case Map.lookup you (worldLocationHistory world) of
        Just (p:_) -> Just p
        _          -> Nothing
      base = case Map.lookup you (worldLocations world) of
        Nothing       -> flatLayout actions
        Just playerLoc ->
          let lg = worldLocationGraph world
          in case Map.lookup playerLoc (lgCoords lg) of
               Nothing     -> flatLayout actions
               Just (px, py) -> spatialLayout actions playerLoc (px, py) lg totalCols
  in markPreviousLocation prevLoc base

-- | Prepend a back-arrow to the label of the cell whose movement target
-- is the player's most recent previous location.  Gives the player an
-- at-a-glance "this is the way you came" cue on the selection HUD —
-- complementing (not replacing) the subtler trail-mark breadcrumbs.
markPreviousLocation :: Maybe Location -> SpatialHUD -> SpatialHUD
markPreviousLocation Nothing  hud = hud
markPreviousLocation (Just p) hud =
  hud { shSpatialCells = map mark (shSpatialCells hud) }
  where
    mark c
      | hudTarget c == Just p = c { hudLabel = "\x2190 " <> hudLabel c }
      | otherwise             = c

-- | Prefix for the action with 1-based index @n@ drawn from the given
-- key pool — "q) ", "a) ", etc.  Matches the key the player actually
-- presses to pick that action.  Pool is typically 'generalOptionKeys'
-- or 'movementOptionKeys'.
optionLabel :: String -> Int -> AnyAction -> String
optionLabel pool n act = poolKeyFor pool n : ") " <> stripAnsi (anyActionLabel act)

-- | Fallback: all actions listed linearly, drawn from the general pool.
flatLayout :: [AnyAction] -> SpatialHUD
flatLayout actions = SpatialHUD
  { shGeneralLabels = zipWith (optionLabel generalOptionKeys) [1 :: Int ..] actions
  , shSpatialCells  = []
  , shPlayerMarker  = (0, 0)
  , shBoxWidth      = 0
  , shBoxHeight     = 0
  }

-- | Spatial layout: split actions into movement/non-movement, position movement
-- actions by relative direction, scaled by actual graph distance.
spatialLayout :: [AnyAction] -> Location -> (Double, Double)
              -> LocationGraph -> Int -> SpatialHUD
spatialLayout actions _playerLoc (px, py) lg totalCols =
  let -- Classify by shape first: movement actions with known target
      -- coords vs. general (non-movement) actions.  Order within each
      -- bucket matches the order in @actions@ so keys stay stable
      -- across turns when the action set is unchanged.
      classify act = case movementTarget act of
        Just targetLoc
          | Just (tx, ty) <- Map.lookup targetLoc (lgCoords lg)
          -> Right (act, tx - px, ty - py)
        _ -> Left act

      (generals, movementsRaw) = partitionEither (map classify actions)

      -- Each pool is numbered independently from 1.  Generals land on
      -- the home row (asdfghjkl); movements land on the top letter
      -- row (qwertyuiop) through 'placeMovement'.
      genLabels = zipWith (optionLabel generalOptionKeys) [1 :: Int ..] generals
      movements = zipWith (\i (act, dx, dy) -> (i, act, dx, dy))
                          [1 :: Int ..] movementsRaw

      -- Spatial box dimensions — use most of the screen width
      boxW = totalCols - 8
      boxH = 11                       -- enough vertical spread

      -- Center of the box (player position)
      centerCol = boxW `div` 2
      centerRow = boxH `div` 2

      -- Find the max distance among movements for relative scaling
      dists = [ sqrt (dx * dx + dy * dy) | (_, _, dx, dy) <- movements ]
      maxDist = if null dists then 1.0 else max 0.01 (maximum dists)

      -- If the generator dropped several neighbours in roughly the
      -- same compass direction they'd pile on top of each other.
      -- Redistribute angles so every cell sits at least
      -- @minAngularGap@ radians from its neighbour on the angle
      -- circle — preserves rough direction while guaranteeing
      -- readable spread.
      spreadMovements = angularSpread movements

      -- Map each movement action to a grid cell, scaled relative to max distance
      cells = map (placeMovement boxW boxH centerCol centerRow maxDist) spreadMovements

      -- Resolve overlaps: nudge cells that collide with each other or the center
      resolved = resolveOverlaps centerCol centerRow boxW boxH cells

  in SpatialHUD
    { shGeneralLabels = genLabels
    , shSpatialCells  = resolved
    , shPlayerMarker  = (centerCol, centerRow)
    , shBoxWidth      = boxW
    , shBoxHeight     = boxH
    }

-- | Redistribute movement angles so no two cells land in the same
-- compass wedge.  The generator can easily drop several neighbours
-- roughly north of the player, which would pile their labels on top
-- of each other if we used the graph angle unmodified.
--
-- Strategy: sort cells by current angle, walk them once, and if any
-- pair is closer than @minAngularGap@ radians, rotate the later one
-- forward by the remaining gap.  We preserve ordering (so "roughly
-- north" neighbours still cluster northward) while guaranteeing
-- visual separation.  The radius (distance) is left intact so near/
-- far still reads correctly after the fan-out.
angularSpread :: [(Int, AnyAction, Double, Double)]
              -> [(Int, AnyAction, Double, Double)]
angularSpread [] = []
angularSpread movements =
  let -- We need at least 8 distinct angular buckets for up to 8
      -- cells.  Anything tighter than this pushes labels into
      -- re-collision under the ~9-row grid.
      minGap = 2 * pi / 8    -- 45°
      -- Tag each movement with (angle, dist) for convenience
      tagged = [ (atan2 dx dy, sqrt (dx*dx + dy*dy), n, act)
               | (n, act, dx, dy) <- movements ]
      -- Sort by angle so we can walk it and enforce the gap
      sorted = sortOn (\(a, _, _, _) -> a) tagged
      spread []     = []
      spread (x:xs) = x : go x xs
      go _prev [] = []
      go (pa, _, _, _) ((a, d, n, act) : rest) =
        let diff = a - pa
            adjusted
              | diff < minGap = pa + minGap
              | otherwise     = a
            entry = (adjusted, d, n, act)
        in entry : go entry rest
      spread'  = spread sorted
      -- Rebuild into (n, act, dx, dy) using the adjusted angles but
      -- the original distances.  dx = d*sin(a), dy = d*cos(a) so
      -- atan2(dx, dy) = a — matches placeMovement's angle convention.
  in [ (n, act, d * sin a, d * cos a) | (a, d, n, act) <- spread' ]

-- | Place a movement action in the spatial grid based on its relative direction.
-- Distance is scaled relative to the farthest neighbor so closer spots are
-- visibly nearer and farther spots reach the edges — irregular, like a map.
placeMovement :: Int -> Int -> Int -> Int -> Double
              -> (Int, AnyAction, Double, Double) -> HUDCell
placeMovement boxW boxH centerCol centerRow maxDist (n, act, dx, dy) =
  let target = movementTarget act
      label = optionLabel movementOptionKeys n act
      labelLen = length label
      -- Reserve space for the label itself
      maxColDisp = (boxW - labelLen) `div` 2 - 1
      maxRowDisp = centerRow - 1
      -- Proportional distance: 0.0 (here) to 1.0 (farthest neighbor)
      dist = sqrt (dx * dx + dy * dy)
      -- Scale so even the nearest neighbour sits well clear of the
      -- player marker: 0.55 floor pushes every cell out at least a
      -- little over half the available displacement from center.
      -- The 1.0 ceiling (0.55 + 0.45) means the farthest still
      -- reaches the edge.
      proportion = if dist < 0.001 then 0 else dist / maxDist
      scale = 0.55 + 0.45 * proportion
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
       , hudDist   = dist
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

    -- Check if two cells overlap.  Labels on the same row obviously
    -- collide if their column ranges overlap; labels on adjacent rows
    -- can also visually collide once the pixel nudge ±cellHeight/4 is
    -- applied (the text from row N leaks into row N+1's upper
    -- descenders space).  Treating adjacent rows as collisions keeps
    -- cells at least two rows apart when their columns would overlap.
    overlaps :: HUDCell -> HUDCell -> Bool
    overlaps a b =
      let rowsClose = abs (hudRow a - hudRow b) <= 1
          pad      = 2   -- extra columns of breathing room
          aEnd = hudCol a + length (hudLabel a) + pad
          bEnd = hudCol b + length (hudLabel b) + pad
          colsOverlap = not (aEnd <= hudCol b || bEnd <= hudCol a)
      in rowsClose && colsOverlap

    -- Check if a cell overlaps the player marker "You" at center,
    -- with extra padding on either side so pixel nudges can't push a
    -- label into the marker cell either.  Also clears the rows just
    -- above and below so labels on the center row's neighbours don't
    -- graze the marker either.
    overlapsCenter :: HUDCell -> Bool
    overlapsCenter cell =
      let cEnd        = hudCol cell + length (hudLabel cell) + 1
          markerLen   = 3  -- "You"
          markerPad   = 3  -- breathing room on each side (in cells)
          markerStart = cx - markerLen `div` 2 - markerPad
          markerEnd   = cx + markerLen `div` 2 + markerPad
          horizontalHit = not (cEnd <= markerStart || hudCol cell >= markerEnd)
          rowHit       = abs (hudRow cell - cy) <= 0
      in rowHit && horizontalHit

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

-- | A sprite placement in absolute pixel coordinates.  The sprite's
-- identity is kept as a @String@ (its class name) so the renderer can
-- look up the actual pixel layout — keeping this module free of
-- 'SDL.Sprites' and the SDL import chain.
data SpritePlacement = SpritePlacement
  { spX      :: !Int      -- ^ pixel x (top-left)
  , spY      :: !Int      -- ^ pixel y (top-left)
  , spClass  :: !String   -- ^ sprite-class key (@"Field"@, @"Bush"@, …)
  , spIndex  :: !Int      -- ^ index into the class's sprite vocab
  } deriving (Show)

-- | Seeded pixel-coord scatter for the player's current location.
-- Returns sprite placements at absolute pixel positions within the
-- spatial panel.  @exclusions@ is a list of pixel rects (x, y, w, h)
-- to avoid — used to keep scatter from overlapping labels.
--
-- The sprite pool is keyed on the last word of the region's name
-- ("Field", "Road", etc.) — same convention the narration pool uses.
terrainSpriteScatter
  :: CharId
  -> GameWorld
  -> (Int, Int)            -- ^ panel pixel origin (left, top)
  -> (Int, Int)            -- ^ panel pixel size (w, h)
  -> [(Int, Int, Int, Int)] -- ^ exclusion rects in pixel space (x, y, w, h)
  -> [SpritePlacement]
terrainSpriteScatter you world (panelX, panelY) (panelW, panelH) exclusions =
  case Map.lookup you (worldLocations world) of
    Nothing  -> []
    Just loc ->
      let lg       = worldLocationGraph world
          region   = Map.lookup loc (lgRegions lg)
          seed     = locHash loc
          interior = zoneInteriorness lg loc region
          cls      = case region of
            Just (Region n) -> lastWord n
            Nothing         -> ""
          -- Density: sparse by default — roughly one sprite per 80×80
          -- pixel area.  Interiorness modulates so a deep-field
          -- location still feels more populated than a field edge,
          -- but nothing gets crowded.
          baseCount = (panelW * panelH) `div` 6400
          target    = max 4 (scaleCount interior baseCount)
          -- Sprites are ~12-18 px at scale=3.  Keep ~40 px of
          -- minimum spacing between them so the background reads as
          -- texture, not noise.
          minGap    = 40
          positions = spaced minGap (4 * target) seed panelW panelH
          keep (x, y) = not (inAnyRect (x + panelX, y + panelY) exclusions)
          picks = zipWith
                    (\(x, y) k -> SpritePlacement
                       { spX     = x + panelX
                       , spY     = y + panelY
                       , spClass = cls
                       , spIndex = k
                       })
                    (filter keep positions)
                    (map (\i -> (i + seed) `mod` 32) [0 :: Int ..])
      in take target picks

-- | Extract the last whitespace-separated word of a region name.  Used
-- to key the sprite pool — @"North Field"@ → @"Field"@.
lastWord :: String -> String
lastWord s = case reverse (words s) of
  (w:_) -> w
  []    -> ""

-- | Is a point inside any of the given rects?
inAnyRect :: (Int, Int) -> [(Int, Int, Int, Int)] -> Bool
inAnyRect (px, py) = any (\(x, y, w, h) ->
  px >= x && px < x + w && py >= y && py < y + h)

-- | Deterministic pseudo-random pixel positions within a panel.  Same
-- LCG scheme as the old grid-coord scatter, just producing pixel
-- coords directly.
pixelPositions :: Int -> Int -> Int -> [(Int, Int)]
pixelPositions s panelW panelH = go s
  where
    go k =
      let k1 = k * 1103515245 + 12345
          k2 = k1 * 1103515245 + 12345
          px = abs (k1 `div` 65536) `mod` max 1 panelW
          py = abs (k2 `div` 65536) `mod` max 1 panelH
      in (px, py) : go k2

-- | Take up to @n@ positions from the seeded stream, rejecting any
-- candidate that's within @gap@ Chebyshev distance of an already-kept
-- position.  Gives a crude Poisson-disk feel for free — sprites won't
-- clump into a busy blob the way a pure random scatter can.  We pull
-- from the stream generously (~4× target) so rejections don't starve
-- the final list.
spaced :: Int -> Int -> Int -> Int -> Int -> [(Int, Int)]
spaced gap n seed panelW panelH =
  go n (take (max 1 (4 * n)) (pixelPositions (abs seed + 1) panelW panelH)) []
  where
    go 0 _      acc = reverse acc
    go _ []     acc = reverse acc
    go k (p:ps) acc
      | any (tooClose p) acc = go k ps acc
      | otherwise            = go (k - 1) ps (p : acc)
    tooClose (x1, y1) (x2, y2) =
      abs (x1 - x2) < gap && abs (y1 - y2) < gap

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
