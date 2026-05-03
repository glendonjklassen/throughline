-- | Section descriptors: a small DSL for describing a one-mile parcel
-- of land in terrain primitives.  Compiles down to a 'LocationGraph' via
-- "Scenarios.DeerHunt.Generation" — same descriptor + same seed =
-- identical map, always.
--
-- Why: the hand-authored DeerHunt map in "Scenarios.DeerHunt.Locations"
-- encodes a specific southern-Manitoba section (road along the east
-- edge, bush in the NW, fields in between, a creek cutting SE).
-- Expressing it as data means two things: we can re-roll for variety
-- without losing the "feel" of the place, and the section can travel
-- with the scenario as pure data.
module Scenarios.DeerHunt.Section
  ( SectionDescriptor(..)
  , TerrainPrimitive(..)
  , Axis(..)
  , EdgeSide(..)
  , Quadrant(..)
  , EdgePoint(..)
  , Position(..)
  , manitoba1mi
  ) where

-- | How a feature is oriented on the section.
data Axis = NorthSouth | EastWest
  deriving (Show, Eq)

-- | Which edge of the square section a feature sits on.
data EdgeSide = NorthEdge | SouthEdge | EastEdge | WestEdge
  deriving (Show, Eq)

-- | Which quadrant of the section a patch claims.
data Quadrant = NW | NE | SW | SE
  deriving (Show, Eq)

-- | A point on the perimeter: which edge, and how far along it (0.0–1.0
-- measured in the edge's natural direction, e.g. W→E for top/bottom).
data EdgePoint = EdgePoint !EdgeSide !Double
  deriving (Show, Eq)

-- | A scalar 0.0–1.0 along an axis.  Used to anchor line features like
-- ridges inside the section rather than on its perimeter.
newtype Position = Position Double
  deriving (Show, Eq)

-- | A terrain primitive.  Primitives are painted in order onto the
-- rasterized grid; later primitives overwrite earlier ones (except
-- 'FieldFill', which only claims cells that are still empty).
data TerrainPrimitive
  = RoadAxis !Axis !EdgeSide
    -- ^ Road running along the given axis, seated against the given edge.
  | BushPatch !Quadrant !Double
    -- ^ Bush covering a quadrant at the given size fraction (0.0–1.0 of
    -- the quadrant's area — smaller values leave a field margin).
  | Creek !EdgePoint !EdgePoint
    -- ^ A creek traced as a rough line from one perimeter point to
    -- another, one cell wide.  Paints water cells plus a thin riparian
    -- zone alongside.
  | RidgeLine !Axis !Position
    -- ^ A ridge running along the given axis, positioned at the given
    -- scalar along the perpendicular axis.  Paints a narrow band.
  | FieldFill
    -- ^ Fill anything still unclaimed with field cells.  Usually the
    -- last primitive.
  deriving (Show, Eq)

-- | Describes a section of land in enough detail to generate a
-- 'LocationGraph'.  Grid size is hard-wired in the generator (64×64 at
-- time of writing); metric dimensions are stored here for future
-- density tuning and for authors who want to think in meters.
data SectionDescriptor = SectionDescriptor
  { sdSeed            :: !Int
  , sdWidthMeters     :: !Double
  , sdHeightMeters    :: !Double
  , sdPrimitives      :: ![TerrainPrimitive]
  , sdLocationDensity :: !Double
    -- ^ Average locations per zone.  Per-zone counts jitter around this
    -- based on zone area and the seed.
  } deriving (Show, Eq)

-- | The canonical Deer Hunt section: southern Manitoba, one square mile,
-- road running along the east and west edges, bush in the NW and SE,
-- an oak ridge, a creek, and fields filling the middle.  Same layout
-- the hand-authored map describes — but now expressible as data.
manitoba1mi :: SectionDescriptor
manitoba1mi = SectionDescriptor
  { sdSeed            = 42
  , sdWidthMeters     = 1609
  , sdHeightMeters    = 1609
  , sdPrimitives      =
      [ RoadAxis   NorthSouth EastEdge           -- section-line road
      , RoadAxis   EastWest   WestEdge           -- the W road
      , BushPatch  NW         0.55               -- bush edge
      , RidgeLine  EastWest   (Position 0.35)    -- oak ridge along the NE
      , Creek      (EdgePoint NorthEdge 0.55)
                   (EdgePoint SouthEdge 0.70)    -- creek bed
      , BushPatch  SE         0.35               -- poplar stand
      , FieldFill                                -- fields fill remainder
      ]
  , sdLocationDensity = 4.5
  }
