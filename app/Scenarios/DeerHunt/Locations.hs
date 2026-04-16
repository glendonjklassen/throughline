module Scenarios.DeerHunt.Locations
  ( -- * Zones
    Zone(..)
  , allZones
  , locationZone
  , zoneLocations
    -- * Locations — Roads
  , truckNorth, ditchNorth
  , truckSouth, ditchSouth
  , truckWest,  ditchWest
    -- * Locations — North Field
  , nFieldEdge, stubbleRows, hayBale, drainageDitch
    -- * Locations — South Field
  , sFieldEdge, stubbleFlat, fenceLine, sloughEdge
    -- * Locations — Bush Edge
  , thinPoplars, brushPile, gameTrailEntrance, oldFence, clearing, deadfall
    -- * Locations — Oak Ridge
  , ridgeTop, oakThicket, scrapeLine, mossyHollow, blowdown, deerTrail
    -- * Locations — Willow Bottom
  , cattailMarsh, willowTangle, creekCrossing, mudFlat, beaverDam, dryHummock
    -- * Locations — Poplar Stand
  , poplarAlley, birchClump, rubLine, openUnderstory, gameTrailFork, windbreak
    -- * Queries
  , allLocations
  , truckLocations
  , adjacency
  , adjacentTo
  , zoneOf
  , isFieldZone
  , isBushZone
  , isRoadZone
    -- * Terrain properties
  , TerrainNoise(..)
  , TerrainVisibility(..)
  , terrainNoise
  , terrainVisibility
    -- * Coordinates
  , locationCoords
  , locationCoordMap
    -- * LocationGraph
  , huntLocationGraph
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           GameTypes (Location(..), LocationGraph(..), Region(..))

-- ---------------------------------------------------------------------------
-- Zones
-- ---------------------------------------------------------------------------

data Zone
  = NorthRoad | SouthRoad | WestRoad
  | NorthField | SouthField
  | BushEdge | OakRidge | WillowBottom | PoplarStand
  deriving (Eq, Ord, Show, Enum, Bounded)

allZones :: [Zone]
allZones = [minBound .. maxBound]

isRoadZone :: Zone -> Bool
isRoadZone z = z `elem` [NorthRoad, SouthRoad, WestRoad]

isFieldZone :: Zone -> Bool
isFieldZone z = z `elem` [NorthField, SouthField]

isBushZone :: Zone -> Bool
isBushZone z = z `elem` [BushEdge, OakRidge, WillowBottom, PoplarStand]

-- ---------------------------------------------------------------------------
-- Locations — Roads
-- ---------------------------------------------------------------------------

truckNorth, ditchNorth :: Location
truckNorth = Location "Your Truck (North Road)"
ditchNorth = Location "North Road Ditch"

truckSouth, ditchSouth :: Location
truckSouth = Location "Your Truck (South Road)"
ditchSouth = Location "South Road Ditch"

truckWest, ditchWest :: Location
truckWest  = Location "Your Truck (West Road)"
ditchWest  = Location "West Road Ditch"

-- ---------------------------------------------------------------------------
-- Locations — North Field
-- ---------------------------------------------------------------------------

nFieldEdge, stubbleRows, hayBale, drainageDitch :: Location
nFieldEdge    = Location "North Field Edge"
stubbleRows   = Location "Stubble Rows"
hayBale       = Location "Hay Bale"
drainageDitch = Location "Drainage Ditch"

-- ---------------------------------------------------------------------------
-- Locations — South Field
-- ---------------------------------------------------------------------------

sFieldEdge, stubbleFlat, fenceLine, sloughEdge :: Location
sFieldEdge  = Location "South Field Edge"
stubbleFlat = Location "Stubble Flat"
fenceLine   = Location "Fence Line"
sloughEdge  = Location "Slough Edge"

-- ---------------------------------------------------------------------------
-- Locations — Bush Edge
-- ---------------------------------------------------------------------------

thinPoplars, brushPile, gameTrailEntrance, oldFence, clearing, deadfall :: Location
thinPoplars       = Location "Thin Poplars"
brushPile         = Location "Brush Pile"
gameTrailEntrance = Location "Game Trail Entrance"
oldFence          = Location "Old Fence"
clearing          = Location "Clearing"
deadfall          = Location "Deadfall"

-- ---------------------------------------------------------------------------
-- Locations — Oak Ridge
-- ---------------------------------------------------------------------------

ridgeTop, oakThicket, scrapeLine, mossyHollow, blowdown, deerTrail :: Location
ridgeTop    = Location "Ridge Top"
oakThicket  = Location "Oak Thicket"
scrapeLine  = Location "Scrape Line"
mossyHollow = Location "Mossy Hollow"
blowdown    = Location "Blowdown"
deerTrail   = Location "Deer Trail"

-- ---------------------------------------------------------------------------
-- Locations — Willow Bottom
-- ---------------------------------------------------------------------------

cattailMarsh, willowTangle, creekCrossing, mudFlat, beaverDam, dryHummock :: Location
cattailMarsh  = Location "Cattail Marsh"
willowTangle  = Location "Willow Tangle"
creekCrossing = Location "Creek Crossing"
mudFlat       = Location "Mud Flat"
beaverDam     = Location "Beaver Dam"
dryHummock    = Location "Dry Hummock"

-- ---------------------------------------------------------------------------
-- Locations — Poplar Stand
-- ---------------------------------------------------------------------------

poplarAlley, birchClump, rubLine, openUnderstory, gameTrailFork, windbreak :: Location
poplarAlley     = Location "Poplar Alley"
birchClump      = Location "Birch Clump"
rubLine         = Location "Rub Line"
openUnderstory  = Location "Open Understory"
gameTrailFork   = Location "Game Trail Fork"
windbreak       = Location "Windbreak"

-- ---------------------------------------------------------------------------
-- All locations
-- ---------------------------------------------------------------------------

allLocations :: [Location]
allLocations = concatMap zoneLocations allZones

truckLocations :: [Location]
truckLocations = [truckNorth, truckSouth, truckWest]

zoneLocations :: Zone -> [Location]
zoneLocations NorthRoad    = [truckNorth, ditchNorth]
zoneLocations SouthRoad    = [truckSouth, ditchSouth]
zoneLocations WestRoad     = [truckWest,  ditchWest]
zoneLocations NorthField   = [nFieldEdge, stubbleRows, hayBale, drainageDitch]
zoneLocations SouthField   = [sFieldEdge, stubbleFlat, fenceLine, sloughEdge]
zoneLocations BushEdge     = [thinPoplars, brushPile, gameTrailEntrance, oldFence, clearing, deadfall]
zoneLocations OakRidge     = [ridgeTop, oakThicket, scrapeLine, mossyHollow, blowdown, deerTrail]
zoneLocations WillowBottom = [cattailMarsh, willowTangle, creekCrossing, mudFlat, beaverDam, dryHummock]
zoneLocations PoplarStand  = [poplarAlley, birchClump, rubLine, openUnderstory, gameTrailFork, windbreak]

-- ---------------------------------------------------------------------------
-- Zone lookup
-- ---------------------------------------------------------------------------

zoneOf :: Location -> Maybe Zone
zoneOf loc = Map.lookup loc zoneMap

locationZone :: Location -> Zone
locationZone loc = case zoneOf loc of
  Just z  -> z
  Nothing -> error $ "locationZone: unknown location " <> show loc

zoneMap :: Map.Map Location Zone
zoneMap = Map.fromList
  [ (loc, z) | z <- allZones, loc <- zoneLocations z ]

-- ---------------------------------------------------------------------------
-- Adjacency
-- ---------------------------------------------------------------------------

-- | All edges as undirected pairs. Each pair listed once; adjacentTo
-- expands to both directions.
adjacency :: [(Location, Location)]
adjacency = intraZone ++ crossZone

-- | Lookup: which locations are reachable from the given one?
adjacentTo :: Location -> [Location]
adjacentTo loc = Map.findWithDefault [] loc adjacencyMap

adjacencyMap :: Map.Map Location [Location]
adjacencyMap = Map.fromListWith (++)
  [ pair
  | (a, b) <- adjacency
  , pair   <- [(a, [b]), (b, [a])]
  ]

-- ---------------------------------------------------------------------------
-- Intra-zone edges
-- ---------------------------------------------------------------------------

intraZone :: [(Location, Location)]
intraZone = concat
  [ -- Roads
    [ (truckNorth, ditchNorth)
    , (truckSouth, ditchSouth)
    , (truckWest,  ditchWest)
    ]
  , -- North Field
    [ (nFieldEdge,    stubbleRows)
    , (stubbleRows,   hayBale)
    , (hayBale,       drainageDitch)
    , (nFieldEdge,    drainageDitch)    -- loop
    ]
  , -- South Field
    [ (sFieldEdge,  stubbleFlat)
    , (stubbleFlat, fenceLine)
    , (fenceLine,   sloughEdge)
    , (sFieldEdge,  sloughEdge)         -- loop
    ]
  , -- Bush Edge
    [ (thinPoplars,       brushPile)
    , (thinPoplars,       gameTrailEntrance)
    , (brushPile,         clearing)
    , (gameTrailEntrance, oldFence)
    , (gameTrailEntrance, deadfall)
    , (oldFence,          clearing)
    , (clearing,          deadfall)
    ]
  , -- Oak Ridge
    [ (ridgeTop,    oakThicket)
    , (oakThicket,  scrapeLine)
    , (scrapeLine,  mossyHollow)
    , (mossyHollow, blowdown)
    , (ridgeTop,    deerTrail)
    , (deerTrail,   scrapeLine)         -- loop
    , (blowdown,    deerTrail)
    ]
  , -- Willow Bottom
    [ (cattailMarsh,  willowTangle)
    , (willowTangle,  creekCrossing)
    , (creekCrossing, mudFlat)
    , (mudFlat,       beaverDam)
    , (beaverDam,     dryHummock)
    , (cattailMarsh,  dryHummock)       -- loop
    , (willowTangle,  mudFlat)
    ]
  , -- Poplar Stand
    [ (poplarAlley,    birchClump)
    , (birchClump,     rubLine)
    , (rubLine,        openUnderstory)
    , (openUnderstory, gameTrailFork)
    , (gameTrailFork,  windbreak)
    , (poplarAlley,    gameTrailFork)   -- shortcut
    , (birchClump,     openUnderstory)
    ]
  ]

-- ---------------------------------------------------------------------------
-- Cross-zone edges
-- Zone graph:
--   North Road -- North Field -- Bush Edge -- Oak Ridge
--                                  |             |
--   West Road -- South Field -- Poplar Stand -- Willow Bottom
--                   |
--               South Road
-- ---------------------------------------------------------------------------

crossZone :: [(Location, Location)]
crossZone =
  [ (ditchNorth,  nFieldEdge)          -- North Road   → North Field
  , (drainageDitch, thinPoplars)       -- North Field  → Bush Edge
  , (gameTrailEntrance, deerTrail)     -- Bush Edge    → Oak Ridge
  , (oldFence,    gameTrailFork)       -- Bush Edge    → Poplar Stand
  , (ditchWest,   sFieldEdge)          -- West Road    → South Field
  , (sloughEdge,  windbreak)           -- South Field  → Poplar Stand
  , (ditchSouth,  fenceLine)           -- South Road   → South Field
  , (mossyHollow, cattailMarsh)        -- Oak Ridge    → Willow Bottom
  , (rubLine,     creekCrossing)       -- Poplar Stand → Willow Bottom
  ]

-- ---------------------------------------------------------------------------
-- Terrain properties
-- ---------------------------------------------------------------------------

data TerrainNoise = Quiet | Moderate | Loud
  deriving (Show, Eq, Ord, Enum, Bounded)

data TerrainVisibility = Open | Partial | Dense
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | How loud your movement is at this location.
terrainNoise :: Location -> TerrainNoise
terrainNoise l
  -- Roads: gravel but expected
  | l `elem` [truckNorth, ditchNorth, truckSouth, ditchSouth, truckWest, ditchWest]
    = Quiet
  -- Open field: stubble is quiet
  | l `elem` [nFieldEdge, stubbleRows, hayBale, drainageDitch,
              sFieldEdge, stubbleFlat, fenceLine, sloughEdge]
    = Quiet
  -- Trails: worn path, quieter footing
  | l `elem` [gameTrailEntrance, deerTrail, gameTrailFork, rubLine]
    = Quiet
  -- Ridge top: above canopy, quiet footing
  | l == ridgeTop
    = Quiet
  -- Thin bush: some cover, some noise
  | l `elem` [thinPoplars, clearing, openUnderstory, poplarAlley]
    = Moderate
  -- Wet ground: squelchy but not crunchy
  | l `elem` [cattailMarsh, mudFlat, creekCrossing]
    = Moderate
  -- Scrape/sign areas: dense, deer-worn but thick
  | l `elem` [scrapeLine, mossyHollow]
    = Moderate
  -- Dense bush: loud underfoot
  | l `elem` [oakThicket, willowTangle, brushPile, deadfall, blowdown]
    = Loud
  -- Remaining locations default to moderate
  | otherwise
    = Moderate

-- | How far you can see / how exposed you are at this location.
terrainVisibility :: Location -> TerrainVisibility
terrainVisibility l
  -- Roads: wide open
  | l `elem` [truckNorth, ditchNorth, truckSouth, ditchSouth, truckWest, ditchWest]
    = Open
  -- Open field: nothing blocks sight
  | l `elem` [nFieldEdge, stubbleRows, hayBale, drainageDitch,
              sFieldEdge, stubbleFlat, fenceLine, sloughEdge]
    = Open
  -- Ridge top: above the canopy
  | l == ridgeTop
    = Open
  -- Field edge and thin bush: transition zones
  | l `elem` [thinPoplars, clearing, openUnderstory, poplarAlley]
    = Partial
  -- Trails: worn path with some sightlines
  | l `elem` [gameTrailEntrance, deerTrail, gameTrailFork, rubLine]
    = Partial
  -- Wet ground: partial cover
  | l `elem` [cattailMarsh, mudFlat, creekCrossing, dryHummock, beaverDam]
    = Partial
  -- Dense bush: can't see far
  | l `elem` [oakThicket, willowTangle, brushPile, deadfall, blowdown]
    = Dense
  -- Scrape/sign areas: dense, thick
  | l `elem` [scrapeLine, mossyHollow]
    = Dense
  -- Remaining locations default to partial
  | otherwise
    = Partial

-- ---------------------------------------------------------------------------
-- LocationGraph (engine-level representation)
-- ---------------------------------------------------------------------------

-- | Convert the scenario's adjacency pairs and zone assignments into an
-- engine-level LocationGraph.
huntLocationGraph :: LocationGraph
huntLocationGraph = LocationGraph
  { lgEdges   = Set.fromList adjacency
  , lgRegions = Map.mapWithKey (\_ z -> Region (show z)) zoneMap
  , lgCoords  = locationCoordMap
  }

-- ---------------------------------------------------------------------------
-- Location coordinates
-- ---------------------------------------------------------------------------

-- | Spatial position of each location on the section (1 mile × 1 mile).
-- (0.0, 0.0) = southwest corner, (1.0, 1.0) = northeast corner.
-- x = east-west, y = north-south.
locationCoords :: Location -> (Double, Double)
-- North Road
locationCoords l | l == truckNorth     = (0.40, 1.00)
                 | l == ditchNorth     = (0.40, 0.97)
-- South Road
                 | l == truckSouth     = (0.35, 0.00)
                 | l == ditchSouth     = (0.35, 0.03)
-- West Road
                 | l == truckWest      = (0.00, 0.40)
                 | l == ditchWest      = (0.03, 0.40)
-- North Field
                 | l == nFieldEdge     = (0.38, 0.90)
                 | l == stubbleRows    = (0.42, 0.85)
                 | l == hayBale        = (0.48, 0.82)
                 | l == drainageDitch  = (0.35, 0.80)
-- South Field
                 | l == sFieldEdge     = (0.12, 0.35)
                 | l == stubbleFlat    = (0.18, 0.28)
                 | l == fenceLine      = (0.25, 0.20)
                 | l == sloughEdge     = (0.22, 0.38)
-- Bush Edge
                 | l == thinPoplars    = (0.35, 0.75)
                 | l == brushPile      = (0.40, 0.72)
                 | l == gameTrailEntrance = (0.45, 0.70)
                 | l == oldFence       = (0.42, 0.65)
                 | l == clearing       = (0.38, 0.68)
                 | l == deadfall       = (0.48, 0.67)
-- Oak Ridge
                 | l == ridgeTop       = (0.72, 0.70)
                 | l == oakThicket     = (0.68, 0.65)
                 | l == scrapeLine     = (0.65, 0.60)
                 | l == mossyHollow    = (0.62, 0.55)
                 | l == blowdown       = (0.70, 0.58)
                 | l == deerTrail      = (0.60, 0.65)
-- Willow Bottom
                 | l == cattailMarsh   = (0.60, 0.48)
                 | l == willowTangle   = (0.55, 0.42)
                 | l == creekCrossing  = (0.52, 0.38)
                 | l == mudFlat        = (0.58, 0.35)
                 | l == beaverDam      = (0.62, 0.32)
                 | l == dryHummock     = (0.65, 0.40)
-- Poplar Stand
                 | l == poplarAlley    = (0.38, 0.55)
                 | l == birchClump     = (0.42, 0.50)
                 | l == rubLine        = (0.48, 0.45)
                 | l == openUnderstory = (0.45, 0.52)
                 | l == gameTrailFork  = (0.40, 0.58)
                 | l == windbreak      = (0.32, 0.45)
                 | otherwise           = (0.50, 0.50)  -- fallback center

locationCoordMap :: Map.Map Location (Double, Double)
locationCoordMap = Map.fromList
  [ (loc, locationCoords loc) | loc <- allLocations ]
