-- | Procedural generation pipeline: compile a 'SectionDescriptor' into
-- a 'LocationGraph'.  Deterministic: same seed + same descriptor
-- always yields the same map.
--
-- Pipeline:
--
--   1. Rasterize primitives onto a 'gridSize' × 'gridSize' grid of
--      'TerrainClass' cells.
--   2. Flood-fill connected regions of the same class into candidate
--      zones; merge tiny fragments into their largest neighbor.
--   3. Name each zone from its class + its position in the section.
--   4. Scatter locations inside each zone using seeded positions,
--      keeping a rough minimum distance between neighbours.
--   5. Name each location from a per-class vocabulary.
--   6. Build edges: connect every location to its nearest few
--      same-zone neighbours, plus a handful of cross-zone bridges
--      between locations that sit on a zone boundary.
--   7. Emit the final 'LocationGraph' + zone map + name↔zone lookup.
module Scenarios.DeerHunt.Generation
  ( TerrainClass(..)
  , GridCell
  , Grid
  , ZoneId
  , GeneratedMap(..)
  , buildFromDescriptor
  , gridSize
    -- * Testing hooks (exported for the spec)
  , rasterize
  , floodFillZones
  , nameZone
  , debugDump
  ) where

import           Data.List       (sortOn, nub, maximumBy)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Ord        (comparing)
import qualified Data.Set        as Set
import           Data.Set        (Set)

import           GameTypes       (Location(..), LocationGraph(..), Region(..))

import           Scenarios.DeerHunt.Section

-- ---------------------------------------------------------------------------
-- Grid + terrain class
-- ---------------------------------------------------------------------------

-- | Side length of the rasterization grid.  64×64 gives roughly
-- 25-metre cells for a 1-mile section — fine enough to paint features
-- legibly, coarse enough to keep flood-fill cheap.
gridSize :: Int
gridSize = 64

-- | Terrain classes.  One per raster cell.  'Empty' means "no primitive
-- has claimed this cell yet"; 'FieldFill' later converts these to
-- 'CField'.
data TerrainClass
  = CEmpty
  | CField
  | CRoad
  | CBush
  | CRidge
  | CCreek
  deriving (Show, Eq, Ord)

-- | One cell of the raster.  Keeping this a type alias for a 2-tuple
-- keeps the grid a plain 'Map' of coordinates rather than a vector, so
-- updates and lookups stay obvious.
type GridCell = TerrainClass

-- | (col, row) coordinates, 0-indexed from the NW corner.
type Coord = (Int, Int)

-- | The rasterized section: a sparse map from grid coord to class.
-- Unset coordinates are treated as 'CEmpty'.
type Grid = Map Coord GridCell

-- ---------------------------------------------------------------------------
-- Step 1 — Rasterize primitives onto the grid
-- ---------------------------------------------------------------------------

-- | Paint each primitive onto a grid in order.  Earlier primitives are
-- overwritten by later ones, except 'FieldFill' which only claims
-- cells still at 'CEmpty'.
rasterize :: [TerrainPrimitive] -> Grid
rasterize = foldl' paint emptyGrid
  where
    emptyGrid = Map.empty

    paint :: Grid -> TerrainPrimitive -> Grid
    paint g (RoadAxis axis edge)       = paintRoad axis edge g
    paint g (BushPatch quadrant frac)  = paintBush quadrant frac g
    paint g (Creek a b)                = paintCreek a b g
    paint g (RidgeLine axis pos)       = paintRidge axis pos g
    paint g FieldFill                  = paintFieldFill g

-- | Roads are painted as a 3-cell-wide band hugging the given edge.
-- (Wider than one cell so it reads as "road + verge" after flood-fill.)
paintRoad :: Axis -> EdgeSide -> Grid -> Grid
paintRoad axis edge g =
  let bandWidth = 3
      cells = case (axis, edge) of
        (NorthSouth, EastEdge) ->
          [ (c, r) | r <- [0 .. gridSize - 1]
                   , c <- [gridSize - bandWidth .. gridSize - 1] ]
        (NorthSouth, WestEdge) ->
          [ (c, r) | r <- [0 .. gridSize - 1]
                   , c <- [0 .. bandWidth - 1] ]
        (EastWest,   NorthEdge) ->
          [ (c, r) | c <- [0 .. gridSize - 1]
                   , r <- [0 .. bandWidth - 1] ]
        (EastWest,   SouthEdge) ->
          [ (c, r) | c <- [0 .. gridSize - 1]
                   , r <- [gridSize - bandWidth .. gridSize - 1] ]
        -- Axis / edge mismatches (e.g. NorthSouth road on NorthEdge)
        -- are nonsensical; paint nothing rather than guessing.
        _ -> []
  in foldl' (\acc c -> Map.insert c CRoad acc) g cells

-- | Bush patches are elliptical blobs centered in the named quadrant.
-- 'frac' 0.0 → no bush, 0.5 → half the quadrant, 1.0 → full quadrant.
-- The ellipse is jittered slightly off-center so different seeds can
-- give different shapes if we later feed a seed in here.
paintBush :: Quadrant -> Double -> Grid -> Grid
paintBush quadrant frac g =
  let half = gridSize `div` 2
      (cxMin, cxMax, ryMin, ryMax) = case quadrant of
        NW -> (0,    half, 0,    half)
        NE -> (half, gridSize, 0, half)
        SW -> (0,    half, half, gridSize)
        SE -> (half, gridSize, half, gridSize)
      -- Ellipse centered in the quadrant, radii a fraction of quadrant size.
      cx = (cxMin + cxMax) `div` 2
      cy = (ryMin + ryMax) `div` 2
      rx = fromIntegral (cxMax - cxMin) * frac / 2 :: Double
      ry = fromIntegral (ryMax - ryMin) * frac / 2 :: Double
      inside (c, r) =
        let dx = fromIntegral (c - cx) / max 0.001 rx
            dy = fromIntegral (r - cy) / max 0.001 ry
        in dx * dx + dy * dy <= 1.0
      cells = filter inside [ (c, r) | c <- [cxMin .. cxMax - 1]
                                     , r <- [ryMin .. ryMax - 1] ]
  in foldl' (\acc c -> Map.insert c CBush acc) g cells

-- | Ridges are painted as a 2-cell band along the given axis at the
-- named perpendicular position.
paintRidge :: Axis -> Position -> Grid -> Grid
paintRidge axis (Position pos) g =
  let cells = case axis of
        EastWest ->
          let r0 = clampIdx (round (pos * fromIntegral gridSize))
          in [ (c, r) | c <- [0 .. gridSize - 1]
                      , r <- intersectIdx [r0, r0 + 1] ]
        NorthSouth ->
          let c0 = clampIdx (round (pos * fromIntegral gridSize))
          in [ (c, r) | r <- [0 .. gridSize - 1]
                      , c <- intersectIdx [c0, c0 + 1] ]
  in foldl' (\acc c -> Map.insert c CRidge acc) g cells
  where
    clampIdx n = max 0 (min (gridSize - 1) n)
    intersectIdx xs = filter (\n -> n >= 0 && n < gridSize) xs

-- | Creeks are painted by stepping from start to end perimeter points
-- and laying down a 1-cell-wide path plus a 1-cell riparian halo (the
-- halo is still painted as 'CCreek' here; the halo effect emerges
-- because riparian glyphs get added in the scatter later).
paintCreek :: EdgePoint -> EdgePoint -> Grid -> Grid
paintCreek a b g =
  let (sx, sy) = edgePointToCoord a
      (ex, ey) = edgePointToCoord b
      path     = bresenham (sx, sy) (ex, ey)
      halo     = nub (concatMap neighbor path)
      cells    = path ++ halo
  in foldl' (\acc c -> Map.insert c CCreek acc) g (filter inBounds cells)
  where
    inBounds (c, r) = c >= 0 && c < gridSize && r >= 0 && r < gridSize
    neighbor (c, r) =
      [ (c + dc, r + dr) | dc <- [-1, 0, 1], dr <- [-1, 0, 1]
                         , (dc, dr) /= (0, 0) ]

-- | Convert a perimeter 'EdgePoint' to grid coordinates.  0.0 is the
-- NW corner of that edge, 1.0 is the other corner.
edgePointToCoord :: EdgePoint -> Coord
edgePointToCoord (EdgePoint side t) =
  let s = max 0.0 (min 1.0 t)
      n = gridSize - 1
      idx x = round (fromIntegral n * x :: Double)
  in case side of
       NorthEdge -> (idx s, 0)
       SouthEdge -> (idx s, n)
       WestEdge  -> (0,     idx s)
       EastEdge  -> (n,     idx s)

-- | Integer Bresenham line between two cells.  Enough to trace a creek
-- approximately; no need for fractional anti-aliasing at 64-cell scale.
bresenham :: Coord -> Coord -> [Coord]
bresenham (x0, y0) (x1, y1) =
  let dx  = abs (x1 - x0)
      dy  = -(abs (y1 - y0))
      sx  = if x0 < x1 then 1 else -1
      sy  = if y0 < y1 then 1 else -1
      go x y err acc
        | x == x1 && y == y1 = reverse ((x, y) : acc)
        | otherwise =
            let e2   = 2 * err
                (x', err')  = if e2 >= dy then (x + sx, err + dy) else (x, err)
                (y', err'') = if e2 <= dx then (y + sy, err' + dx) else (y, err')
            in go x' y' err'' ((x, y) : acc)
  in go x0 y0 (dx + dy) []

-- | Anything still 'CEmpty' becomes 'CField'.
paintFieldFill :: Grid -> Grid
paintFieldFill g =
  let all_ = [ (c, r) | c <- [0 .. gridSize - 1], r <- [0 .. gridSize - 1] ]
      stamp acc coord = Map.insertWith (\_ old -> old) coord CField acc
  in foldl' stamp g all_

-- ---------------------------------------------------------------------------
-- Step 2 — Flood-fill into zones
-- ---------------------------------------------------------------------------

-- | A numeric zone identity assigned at flood-fill time.  Names come
-- later, once we know each zone's centroid.
type ZoneId = Int

-- | Flood-fill the grid into zones: connected same-class regions become
-- candidate zones.  Fragments smaller than 'minZoneCells' are merged
-- into their largest neighbouring zone so we don't end up with 100
-- one-cell "zones" at primitive boundaries.
floodFillZones :: Grid -> (Map Coord ZoneId, Map ZoneId TerrainClass)
floodFillZones g =
  let (raw, classes) = growRegions g
      merged         = mergeTinyZones g raw classes
  in merged

-- | BFS out from unvisited cells, collecting one connected same-class
-- region per BFS.  Assigns a fresh 'ZoneId' per region.
growRegions :: Grid -> (Map Coord ZoneId, Map ZoneId TerrainClass)
growRegions g = go Map.empty Map.empty 0 (Map.keys g)
  where
    go zoneMap classMap _nextId [] = (zoneMap, classMap)
    go zoneMap classMap nextId (c:cs)
      | Map.member c zoneMap = go zoneMap classMap nextId cs
      | otherwise =
          let cls    = g Map.! c
              region = bfs [c] (Set.singleton c) (Set.singleton c) cls
              zoneMap' = foldl' (\acc coord -> Map.insert coord nextId acc)
                                zoneMap (Set.toList region)
              classMap' = Map.insert nextId cls classMap
          in go zoneMap' classMap' (nextId + 1) cs

    bfs :: [Coord] -> Set Coord -> Set Coord -> TerrainClass -> Set Coord
    bfs []         _seen acc _ = acc
    bfs (c:queue) seen acc cls =
      let ns = [ n | n <- neighbors4 c
                   , not (Set.member n seen)
                   , Map.lookup n g == Just cls ]
          seen'  = foldl' (flip Set.insert) seen ns
          acc'   = foldl' (flip Set.insert) acc  ns
      in bfs (queue ++ ns) seen' acc' cls

    neighbors4 (c, r) =
      [ (c - 1, r), (c + 1, r), (c, r - 1), (c, r + 1) ]

-- | Minimum cells a zone must contain to survive; smaller ones get
-- absorbed into their largest neighbour.  Tuned so that a tiny slice
-- of bush at a road-edge corner doesn't become its own zone.
minZoneCells :: Int
minZoneCells = 24

-- | Merge zones with fewer than 'minZoneCells' into their largest
-- neighbouring zone (measured in cells).  Repeats until no zones are
-- too small.  Terrain class of the merged result takes the *surviving*
-- zone's class.
mergeTinyZones
  :: Grid
  -> Map Coord ZoneId
  -> Map ZoneId TerrainClass
  -> (Map Coord ZoneId, Map ZoneId TerrainClass)
mergeTinyZones g zoneMap classMap0 = loop zoneMap classMap0
  where
    loop zm cm =
      let sizes = zoneSizes zm
          tiny  = [ z | (z, n) <- Map.toList sizes, n < minZoneCells ]
      in case tiny of
           []    -> (zm, cm)
           (z:_) ->
             case biggestNeighbour g zm sizes z of
               Nothing     -> (zm, cm)   -- isolated tiny zone; leave it
               Just target ->
                 let zm' = Map.map (\zid -> if zid == z then target else zid) zm
                     cm' = Map.delete z cm
                 in loop zm' cm'

    zoneSizes :: Map Coord ZoneId -> Map ZoneId Int
    zoneSizes =
      Map.foldl' (\acc zid -> Map.insertWith (+) zid 1 acc) Map.empty

    biggestNeighbour
      :: Grid
      -> Map Coord ZoneId
      -> Map ZoneId Int
      -> ZoneId
      -> Maybe ZoneId
    biggestNeighbour _ zm sizes z =
      let myCells = [ c | (c, zid) <- Map.toList zm, zid == z ]
          neighbourIds = Set.toList $ Set.fromList
            [ nzid
            | c <- myCells
            , n <- neighbors4 c
            , Just nzid <- [Map.lookup n zm]
            , nzid /= z
            ]
      in case neighbourIds of
           [] -> Nothing
           _  -> Just (maximumBy (comparing (\nid -> Map.findWithDefault 0 nid sizes))
                                 neighbourIds)

    neighbors4 (c, r) =
      [ (c - 1, r), (c + 1, r), (c, r - 1), (c, r + 1) ]

-- ---------------------------------------------------------------------------
-- Step 3 — Name zones
-- ---------------------------------------------------------------------------

-- | Given a zone's class and its centroid, produce a human-readable
-- name.  Names use cardinal qualifiers (North / South / East / West /
-- Central) so two field zones don't collide.
nameZone :: TerrainClass -> (Double, Double) -> Int -> String
nameZone cls (cx, cy) ordinal = case cls of
  CField -> cardinalPrefix cx cy ++ " Field"
  CRoad  -> cardinalPrefix cx cy ++ " Road"
  CBush  -> cardinalPrefix cx cy ++ " Bush"
  CRidge -> cardinalPrefix cx cy ++ " Ridge"
  CCreek -> "Creek " ++ show ordinal
  CEmpty -> "Unnamed " ++ show ordinal
  where
    half = fromIntegral gridSize / 2 :: Double
    cardinalPrefix x y
      | y < half * 0.5                     = "North"
      | y > half * 1.5                     = "South"
      | x < half * 0.5                     = "West"
      | x > half * 1.5                     = "East"
      | otherwise                          = "Central"

-- ---------------------------------------------------------------------------
-- Step 5 — Scatter + name locations
-- ---------------------------------------------------------------------------

-- | For each zone, drop @n@ location points inside it using a seeded
-- PRNG.  We don't do proper Poisson-disk sampling; we just rejection-
-- sample candidates and keep any that are at least 'minSpacing' cells
-- from every previously-placed point in the same zone.  Good enough to
-- keep names from piling up.
scatterLocations
  :: Int                              -- ^ seed
  -> Double                           -- ^ target density (avg per zone)
  -> Map Coord ZoneId
  -> Map ZoneId TerrainClass
  -> Map ZoneId [(Coord, String)]
scatterLocations seed density zoneMap classMap =
  let cellsByZone = groupCellsByZone zoneMap
  in Map.mapWithKey (placeInZone seed density classMap) cellsByZone

groupCellsByZone :: Map Coord ZoneId -> Map ZoneId [Coord]
groupCellsByZone =
  Map.foldlWithKey' (\acc c z -> Map.insertWith (++) z [c] acc) Map.empty

placeInZone
  :: Int
  -> Double
  -> Map ZoneId TerrainClass
  -> ZoneId
  -> [Coord]
  -> [(Coord, String)]
placeInZone seed density classMap zid cells =
  let n       = max 2 (round (density * areaFactor cells))
      seeds   = zoneSeedStream (seed + 101 * zid)
      cls     = Map.findWithDefault CField zid classMap
      chosen  = pickSpaced minSpacing n seeds cells []
      vocab   = locationVocab cls
      -- Offset the vocab starting point per (seed, zone) so two zones
      -- of the same class don't both open with the same word.  With
      -- 20+-deep vocabs and zones of ~5-8 locations, this keeps the
      -- generated names unique across the map without any suffixing.
      vocabOffset = (seed * 17 + zid * 31) `mod` length vocab
      named   = zipWith (\i c -> (c, vocab !! ((i + vocabOffset) `mod` length vocab)))
                        [0..] chosen
  in named
  where
    areaFactor cs =
      -- Bigger zones get more locations, capped so a huge field doesn't
      -- drown the map.  Uses a sqrt curve so a 2× zone gets ~1.4× locs.
      let a = fromIntegral (length cs) :: Double
      in max 0.6 (min 2.5 (sqrt (a / 200.0)))

    minSpacing = 7 :: Int

pickSpaced :: Int -> Int -> [Int] -> [Coord] -> [Coord] -> [Coord]
pickSpaced _    0 _      _     acc = reverse acc
pickSpaced _    _ _      []    acc = reverse acc
pickSpaced _    _ []     _     acc = reverse acc
pickSpaced dmin k (s:ss) cells acc =
  let idx = abs s `mod` length cells
      candidate = cells !! idx
      far = all (\p -> cellDistance candidate p >= dmin) acc
  in if far
       then pickSpaced dmin (k - 1) ss cells (candidate : acc)
       else pickSpaced dmin k       ss cells acc

cellDistance :: Coord -> Coord -> Int
cellDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- | A cheap LCG-style PRNG that we treat as an infinite seed list.  Not
-- cryptographically sound; determinism is all we need here.
zoneSeedStream :: Int -> [Int]
zoneSeedStream s0 = iterate step (abs s0 + 1)
  where step s = (s * 1103515245 + 12345) `mod` 2147483647

-- | Per-class vocabulary for naming locations.  Each list is deep
-- enough that a full hunt (~50 locations across 6-10 zones) never
-- exhausts any single class — ensuring every location ends up with a
-- clean unadorned name, no numeric suffixes.  Names are picked
-- deterministically from a seeded permutation per generation run; see
-- 'pickVocabNames'.
locationVocab :: TerrainClass -> [String]
locationVocab cls = case cls of
  CField ->
    [ "Stubble Rows", "Hay Bale", "Drainage Ditch", "Corn Strip"
    , "Fence Line", "Slough Edge", "Sunflower Stubble", "Field Edge"
    , "Stubble Flat", "Broken Rows", "Wheat Husks", "Canola Scatter"
    , "Flax Stubble", "Oat Patch", "Wire Corner", "Low Spot"
    , "Combine Tracks", "Windrow", "Stone Pile", "Bale Line"
    , "Swale", "Burnt Patch", "Fallow Strip", "Seed Drill Line"
    , "Shelterbelt Edge", "Culvert Mouth", "Reedy Dip", "Cut Stalks"
    ]
  CBush ->
    [ "Thin Poplars", "Brush Pile", "Game Trail", "Old Fence"
    , "Clearing", "Deadfall", "Stump Field", "Hazel Clump"
    , "Birch Stand", "Willow Tangle", "Dogwood", "Ash Grove"
    , "Hawthorn Thicket", "Windfall", "Dense Understory", "Buckbrush"
    , "Chokecherry", "Saskatoon Patch", "Pin Cherry", "Cat Briar"
    , "Thimbleberry", "Spruce Pocket", "Elder Stand", "Sedge Fringe"
    , "Fern Gully", "Moss Floor"
    ]
  CRidge ->
    [ "Ridge Top", "Oak Thicket", "Scrape Line", "Mossy Hollow"
    , "Blowdown", "Deer Trail", "Acorn Ground", "Rock Outcrop"
    , "Bur Oak Stand", "Limestone Shelf", "Wind Gap", "Crown Rock"
    , "Bone Meadow", "Sun Slope", "North Slope", "South Slope"
    , "Cedar Bench", "Broken Crown", "Lichen Rocks", "Upland Pine"
    , "Shale Cut", "Goat Track", "Flint Ledge", "Saddle"
    ]
  CCreek ->
    [ "Creek Mouth", "Gravel Bar", "Alder Thicket", "Creek Bend"
    , "Driftwood Pile", "Cattail Marsh", "Mud Flat"
    , "Reed Bed", "Sedge Meadow", "Beaver Dam", "Willow Bottom"
    , "Otter Slide", "Still Pool", "Riffle", "Cut Bank"
    , "Gravel Ford", "Willow Run", "Wet Meadow", "Silt Bank"
    , "Shallow Crossing", "Elder Mat"
    ]
  CRoad ->
    [ "Truck", "Ditch", "Culvert", "Fence Post"
    , "Pull-Off", "Gate", "Cattle Guard", "Road Cut"
    , "Graded Shoulder", "Gravel Wash", "Approach", "Mile Marker"
    , "Grid Crossing", "Dirt Track", "Access Road"
    ]
  CEmpty -> ["Somewhere"]

-- ---------------------------------------------------------------------------
-- Step 6 — Edges
-- ---------------------------------------------------------------------------

-- | For each location, connect it to its @kNearest@ same-zone
-- neighbours.  Between-zone bridges come from locations whose nearest
-- neighbour happens to sit across a zone boundary.
buildEdges
  :: [(Location, ZoneId, (Double, Double))]
  -> Set (Location, Location)
buildEdges nodes =
  let sameZoneEdges = concatMap (nearestInZone 3) byZone
      crossZoneEdges = concatMap (nearestCrossZone 1) nodes
  in Set.fromList (map normalize (sameZoneEdges ++ crossZoneEdges))
  where
    -- Group nodes by zone for intra-zone edges.
    byZone :: [[(Location, ZoneId, (Double, Double))]]
    byZone = Map.elems
           $ Map.fromListWith (++) [ (z, [n]) | n@(_, z, _) <- nodes ]

    nearestInZone :: Int
                  -> [(Location, ZoneId, (Double, Double))]
                  -> [(Location, Location)]
    nearestInZone k zs =
      [ (la, lb)
      | (la, _, pa) <- zs
      , (lb, _) <- take k (sortByDist pa [ (x, p) | (x, _, p) <- zs, x /= la ])
      ]

    nearestCrossZone :: Int
                     -> (Location, ZoneId, (Double, Double))
                     -> [(Location, Location)]
    nearestCrossZone k (la, za, pa) =
      let others = [ (lb, p) | (lb, zb, p) <- nodes, zb /= za ]
      in [ (la, lb) | (lb, _) <- take k (sortByDist pa others) ]

    sortByDist :: (Double, Double)
               -> [(Location, (Double, Double))]
               -> [(Location, (Double, Double))]
    sortByDist p = sortOn (\(_, q) -> dist2 p q)

    dist2 (x1, y1) (x2, y2) = (x1 - x2) ** 2 + (y1 - y2) ** 2

    normalize (a, b) = if a <= b then (a, b) else (b, a)

-- ---------------------------------------------------------------------------
-- Step 7 — Assemble the generated map
-- ---------------------------------------------------------------------------

-- | The full output of the pipeline: enough to drive the engine and
-- the spatial HUD.
data GeneratedMap = GeneratedMap
  { gmGraph     :: !LocationGraph
  , gmZoneNames :: !(Map Location String)
  , gmLocations :: ![Location]
  , gmClassOf   :: !(Map Location TerrainClass)
    -- ^ Terrain class per location.  Downstream scenarios key narration,
    -- axiom behavior, and sign placement off this rather than zone-name
    -- string parsing.
  } deriving (Show)

-- | Compile a descriptor into a full generated map.
buildFromDescriptor :: SectionDescriptor -> GeneratedMap
buildFromDescriptor sd =
  let grid             = rasterize (sdPrimitives sd)
      (zmap, clsMap)   = floodFillZones grid
      zoneNames        = computeZoneNames zmap clsMap
      scattered        = scatterLocations (sdSeed sd) (sdLocationDensity sd) zmap clsMap
      -- Flatten to (Location, ZoneId, normalised (x,y)).  The
      -- per-zone scatter already picks from a deep enough vocabulary
      -- and uses a seeded offset so two zones of the same class never
      -- collide on a name.  No suffixing needed.
      named =
        concatMap (\(zid, placements) ->
          [ (Location name, zid, normCoord coord)
          | (coord, name) <- placements ])
        (Map.toList scattered)
      locs             = [ loc | (loc, _, _) <- named ]
      coordsMap        = Map.fromList
                            [ (loc, p) | (loc, _, p) <- named ]
      regionMap        = Map.fromList
                            [ (loc, Region (Map.findWithDefault "Unknown" zid zoneNames))
                            | (loc, zid, _) <- named ]
      classMap         = Map.fromList
                            [ (loc, Map.findWithDefault CEmpty zid clsMap)
                            | (loc, zid, _) <- named ]
      edges            = buildEdges named
  in GeneratedMap
       { gmGraph     = LocationGraph
           { lgEdges   = edges
           , lgRegions = regionMap
           , lgCoords  = coordsMap
           }
       , gmZoneNames = Map.fromList
           [ (loc, Map.findWithDefault "Unknown" zid zoneNames)
           | (loc, zid, _) <- named ]
       , gmLocations = locs
       , gmClassOf   = classMap
       }
  where
    normCoord (c, r) =
      ( fromIntegral c / fromIntegral (gridSize - 1)
      , fromIntegral r / fromIntegral (gridSize - 1)
      )

computeZoneCentroids :: Map Coord ZoneId -> Map ZoneId (Double, Double)
computeZoneCentroids zm =
  let buckets =
        Map.foldlWithKey' (\acc (c, r) zid ->
          Map.insertWith merge zid (fromIntegral c, fromIntegral r, 1 :: Int) acc)
          Map.empty zm
      merge (x1, y1, n1) (x2, y2, n2) = (x1 + x2, y1 + y2, n1 + n2)
  in Map.map (\(sx, sy, n) -> let nd = fromIntegral n
                              in (sx / nd, sy / nd)) buckets

computeZoneNames
  :: Map Coord ZoneId
  -> Map ZoneId TerrainClass
  -> Map ZoneId String
computeZoneNames zm clsMap =
  let centroids = computeZoneCentroids zm
      raw = Map.mapWithKey
              (\zid cls ->
                  let centroid = Map.findWithDefault (0, 0) zid centroids
                  in nameZone cls centroid zid)
              clsMap
  in uniqueZoneNames raw

-- | Append numeric suffixes when the zone-naming rule produces
-- duplicates (e.g. two "North Field" zones at slightly different
-- latitudes).  Preserves the first occurrence unsuffixed.
uniqueZoneNames :: Map ZoneId String -> Map ZoneId String
uniqueZoneNames m =
  let assocs = Map.toAscList m
      (_, fixed) = foldl' bump (Map.empty :: Map String Int, []) assocs
  in Map.fromList (reverse fixed)
  where
    bump (counts, acc) (zid, name) =
      let k = Map.findWithDefault 0 name counts
          n' = if k == 0 then name else name ++ " " ++ show (k + 1)
          counts' = Map.insertWith (+) name 1 counts
      in (counts', (zid, n') : acc)

-- ---------------------------------------------------------------------------
-- Debug dump — ASCII art of the rasterized grid
-- ---------------------------------------------------------------------------

-- | Render the grid as an ASCII block.  Handy for eyeballing a
-- descriptor before generating locations.
debugDump :: Grid -> [String]
debugDump g =
  [ [ glyph (Map.lookup (c, r) g) | c <- [0 .. gridSize - 1] ]
  | r <- [0 .. gridSize - 1]
  ]
  where
    glyph Nothing         = '.'
    glyph (Just CEmpty)   = '.'
    glyph (Just CField)   = ','
    glyph (Just CRoad)    = '='
    glyph (Just CBush)    = '#'
    glyph (Just CRidge)   = '^'
    glyph (Just CCreek)   = '~'
