-- | The scenario-facing wrapper around a 'GeneratedMap'.  All DeerHunt
-- code now talks to 'HuntWorld' rather than referencing specific
-- location identifiers.  Each run of the hunt builds one 'HuntWorld'
-- from its seed at scenario init and threads it through axioms,
-- narration, and start-location logic.
--
-- Everything on 'HuntWorld' is pure — the same seed produces the same
-- world, and the record packs precomputed queries (class lookups,
-- class-bucketed location lists, the nearest-truck function) so
-- callers don't re-derive them on every tick.
module Scenarios.DeerHunt.World
  ( HuntWorld(..)
  , PositionHint(..)
  , RareFind(..)
  , huntWorld
  , hwClass
  , hwLocsOfClass
  , hwPositionHint
  , hwNearestTruck
  , hwStart
  , hwDeerStart
  , hwCoords
  , hwRegion
  , rareFindCatalog
  , placeFinds
  ) where

import           Data.List        (sortOn)
import qualified Data.Map.Strict  as Map
import           Data.Map.Strict  (Map)
import qualified Data.Set         as Set
import           System.Random    (mkStdGen, randomR)

import           GameTypes       (Location(..), LocationGraph(..), Region(..))

import           Scenarios.DeerHunt.Generation
import           Scenarios.DeerHunt.Section

-- | Where a location sits inside its zone.  'Edge' locations touch a
-- cross-zone boundary (at least one neighbor is a different class);
-- 'Bridge' locations are the same, spelled distinctly for narration;
-- 'Interior' locations are surrounded by same-class neighbours.  The
-- narration pool keys off this to distinguish "deep in the bush" from
-- "at the bush's edge" without the scenario author annotating each
-- location.
data PositionHint
  = Interior
  | Edge
  | Bridge
  deriving (Show, Eq, Ord)

-- | All the map-derived facts a running DeerHunt scenario needs.  This
-- is built once at scenario init and carried alongside 'GameWorld' via
-- closure capture in the axioms/actions.
data HuntWorld = HuntWorld
  { hwMap        :: !GeneratedMap
  , hwSeed       :: !Int
  , hwClassMap   :: !(Map Location TerrainClass)
  , hwByClass    :: !(Map TerrainClass [Location])
  , hwPosHint    :: !(Map Location PositionHint)
  , hwTrucks     :: ![Location]
    -- ^ Road-class locations whose name starts with @"Truck"@.  These
    -- are the hunter's vehicles — candidate starts and the deer-escape
    -- axiom's target points.
  , hwStartLoc   :: !Location
    -- ^ Seeded choice from 'hwTrucks'; the player starts here.
  , hwDeerStartLoc :: !Location
    -- ^ Seeded choice from cover locations (bush/ridge/creek); the
    -- deer starts here.
  , hwFinds      :: !(Map Location String)
    -- ^ Rare location-bound finds placed deterministically from the
    -- seed.  The value is the find's name (matches the 'Discovery'
    -- entry and the 'spriteByName' key).  Each find is independently
    -- rolled; many hunts will have none of a given find.  Populated
    -- by 'Scenarios.DeerHunt.Finds.placeFinds'.
  }

-- | Build the complete 'HuntWorld' from a seed.  Uses the canonical
-- @manitoba1mi@ descriptor but overrides its seed so each game gets
-- a fresh map.
huntWorld :: Int -> HuntWorld
huntWorld seed =
  let gmap   = buildFromDescriptor (manitoba1mi { sdSeed = seed })
      clsOf  = gmClassOf gmap
      byCls  = groupByClass clsOf
      hints  = computePositionHints gmap
      -- Any road-class location is a valid starting candidate — the
      -- hunter parked somewhere along a road, and the specific name
      -- on the generated map doesn't matter for game mechanics.
      trucks = [ l | l <- gmLocations gmap
                   , Map.lookup l clsOf == Just CRoad ]
      -- Seeded start: one of the road locations.
      start  = seededPickDef (Location "spawn") (seed * 31 + 7) trucks
      deerCover = [ l | l <- gmLocations gmap
                      , case Map.lookup l clsOf of
                          Just c  -> c `elem` [CBush, CRidge, CCreek]
                          Nothing -> False ]
      deerStart = seededPickDef start (seed * 71 + 13) deerCover
      finds     = placeFinds seed byCls
  in HuntWorld
       { hwMap          = gmap
       , hwSeed         = seed
       , hwClassMap     = clsOf
       , hwByClass      = byCls
       , hwPosHint      = hints
       , hwTrucks       = trucks
       , hwStartLoc     = start
       , hwDeerStartLoc = deerStart
       , hwFinds        = finds
       }

-- | Look up a location's terrain class.  Unknown locations are
-- reported as 'CEmpty' — a defensive default, never expected in
-- practice since every generated location has a class.
hwClass :: HuntWorld -> Location -> TerrainClass
hwClass hw loc = Map.findWithDefault CEmpty loc (hwClassMap hw)

-- | All generated locations of a given terrain class.  Cached so
-- callers can do cheap bucketed operations (e.g. "pick a random
-- ridge location").
hwLocsOfClass :: HuntWorld -> TerrainClass -> [Location]
hwLocsOfClass hw cls = Map.findWithDefault [] cls (hwByClass hw)

-- | Interior / Edge / Bridge classification for a location.
hwPositionHint :: HuntWorld -> Location -> PositionHint
hwPositionHint hw loc = Map.findWithDefault Interior loc (hwPosHint hw)

-- | Euclidean-nearest truck to the given location.  Used by the
-- deer-escape axiom to decide where a spooked deer flees from.
-- Returns 'hwStartLoc' as a fallback when there are no trucks (shouldn't
-- happen with any sensible descriptor, but we're defensive).
hwNearestTruck :: HuntWorld -> Location -> Location
hwNearestTruck hw loc = case hwTrucks hw of
  []        -> hwStartLoc hw
  (t0:tRest) ->
    let trucks = t0 : tRest
        coords = lgCoords (gmGraph (hwMap hw))
    in case Map.lookup loc coords of
         Nothing -> t0
         Just p  -> case sortOn (\t -> dist2 p (Map.findWithDefault (0,0) t coords)) trucks of
           (nearest:_) -> nearest
           []          -> t0
  where
    dist2 (x1, y1) (x2, y2) = (x1 - x2) ** 2 + (y1 - y2) ** 2

-- | The player's start location for this hunt.
hwStart :: HuntWorld -> Location
hwStart = hwStartLoc

-- | The deer's start location for this hunt.
hwDeerStart :: HuntWorld -> Location
hwDeerStart = hwDeerStartLoc

-- | Look up (x, y) coords for a location in [0, 1]^2.
hwCoords :: HuntWorld -> Location -> Maybe (Double, Double)
hwCoords hw loc = Map.lookup loc (lgCoords (gmGraph (hwMap hw)))

-- | Look up the region name for a location.
hwRegion :: HuntWorld -> Location -> Maybe Region
hwRegion hw loc = Map.lookup loc (lgRegions (gmGraph (hwMap hw)))

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Seeded pick from a list.  Returns the default if the list is empty.
seededPickDef :: a -> Int -> [a] -> a
seededPickDef d _    []  = d
seededPickDef _ seed xs  =
  let (idx, _) = randomR (0, length xs - 1) (mkStdGen seed)
  in xs !! idx

-- | Group locations by their terrain class.
groupByClass :: Map Location TerrainClass -> Map TerrainClass [Location]
groupByClass =
  Map.foldlWithKey' (\acc loc cls ->
    Map.insertWith (++) cls [loc] acc) Map.empty

-- | For each location, decide Interior / Edge / Bridge based on the
-- classes of its graph neighbours.  A location is 'Bridge' if it has
-- at least one neighbour whose class differs from its own; 'Edge' if
-- any of its neighbours-of-neighbours cross a class boundary;
-- otherwise 'Interior'.  This is a cheap approximation of "depth
-- inside the zone" that doesn't require centroid math.
computePositionHints :: GeneratedMap -> Map Location PositionHint
computePositionHints gm =
  let edges   = lgEdges (gmGraph gm)
      clsOf   = gmClassOf gm
      adj     = adjacencyMap edges
      classOf l = Map.findWithDefault CEmpty l clsOf
      myClass = classOf
      neighborsOf l = Set.toList (Map.findWithDefault Set.empty l adj)
      isBridge l = any (\n -> classOf n /= myClass l) (neighborsOf l)
      isEdge   l = any isBridge (neighborsOf l)
      hint l
        | isBridge l = Bridge
        | isEdge   l = Edge
        | otherwise  = Interior
  in Map.fromList [ (l, hint l) | l <- gmLocations gm ]

-- | Convert an edge set into an adjacency map (both directions).
adjacencyMap :: Set.Set (Location, Location) -> Map Location (Set.Set Location)
adjacencyMap =
  Set.foldl' (\m (a, b) ->
    Map.insertWith Set.union a (Set.singleton b) $
    Map.insertWith Set.union b (Set.singleton a) m)
    Map.empty

-- ---------------------------------------------------------------------------
-- Rare finds
-- ---------------------------------------------------------------------------
--
-- Each 'RareFind' is rolled once per hunt section with 'rfChance'.  If
-- it hits, a location is picked from the first terrain class in
-- 'rfTerrain' that has any candidates.  Most hunts will see one or
-- two finds total; some will see none.  Deterministic from 'hwSeed'
-- so the same section always surfaces the same set.

-- | Specification for a rare location-bound find — the kind of thing
-- you tell someone about after a hunt.  @rfName@ also keys into
-- 'SDL.Sprites.spriteByName' for rendering.
data RareFind = RareFind
  { rfName    :: String
  , rfTerrain :: [TerrainClass]
  , rfChance  :: Double
  }

-- | Catalog of rare finds.  The chances are intentionally low — the
-- rusty car especially is the story you tell.  Tune here.
rareFindCatalog :: [RareFind]
rareFindCatalog =
  [ RareFind "rusty 50s car"   [CBush]         0.15
  , RareFind "shed antler"     [CBush, CRidge] 0.40
  , RareFind "abandoned stand" [CBush, CRidge] 0.25
  , RareFind "survey stake"    [CRoad]         0.30
  , RareFind "beaver stump"    [CCreek]        0.35
  , RareFind "skull"           [CBush, CRidge] 0.20
  ]

-- | Roll each find independently.  Returns a map of
-- @location -> find name@ for finds that landed.  Multiple finds can
-- share a location in principle; in practice the small catalog and
-- low chances make that vanishingly rare, so we accept the collision
-- (later write wins) rather than add bookkeeping.
placeFinds :: Int -> Map TerrainClass [Location] -> Map Location String
placeFinds seed byCls =
  Map.fromList (concatMap (placeOne seed byCls) (zip [0 ..] rareFindCatalog))

placeOne :: Int -> Map TerrainClass [Location] -> (Int, RareFind) -> [(Location, String)]
placeOne seed byCls (idx, rf) =
  let g0            = mkStdGen (seed * 10007 + idx * 97 + 3)
      (roll,    g1) = randomR (0.0 :: Double, 1.0) g0
      candidates    = concatMap (\cls -> Map.findWithDefault [] cls byCls) (rfTerrain rf)
  in if roll < rfChance rf && not (null candidates)
       then
         let (lidx, _) = randomR (0, length candidates - 1) g1
         in [(candidates !! lidx, rfName rf)]
       else []
