-- | Spec for the DeerHunt procedural map generator.
--
-- The generator is deterministic by design: same descriptor + same seed
-- must always yield an identical 'LocationGraph'.  These tests pin that
-- property and check shape invariants so accidental nondeterminism
-- (e.g. Map ordering, unsorted edge pairs) breaks CI instead of
-- producing different maps in production.
module Scenarios.DeerHuntGenerationSpec (spec) where

import           Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import           GameTypes       (LocationGraph(..), Region(..), Location)

import           Scenarios.DeerHunt.Section
import           Scenarios.DeerHunt.Generation

-- | A second descriptor, structurally different from 'manitoba1mi', used
-- to verify that changing the input actually changes the output.
manitoba1miAlt :: SectionDescriptor
manitoba1miAlt = manitoba1mi
  { sdSeed       = 7777
  , sdPrimitives =
      [ RoadAxis  NorthSouth WestEdge
      , BushPatch NE         0.50
      , Creek     (EdgePoint WestEdge  0.30) (EdgePoint EastEdge 0.40)
      , RidgeLine NorthSouth (Position 0.60)
      , FieldFill
      ]
  }

spec :: Spec
spec = describe "DeerHunt procedural generation" $ do

  describe "rasterize" $ do
    it "paints something onto the grid for the canonical descriptor" $ do
      let grid = rasterize (sdPrimitives manitoba1mi)
      Map.size grid `shouldSatisfy` (> 0)
    it "fills all cells when FieldFill is the last primitive" $ do
      let grid = rasterize (sdPrimitives manitoba1mi)
      Map.size grid `shouldBe` gridSize * gridSize

  describe "floodFillZones" $ do
    it "produces at least one zone" $ do
      let grid          = rasterize (sdPrimitives manitoba1mi)
          (_zm, clsMap) = floodFillZones grid
      Map.size clsMap `shouldSatisfy` (>= 1)
    it "produces many zones for a primitive-rich descriptor" $ do
      let grid          = rasterize (sdPrimitives manitoba1mi)
          (_zm, clsMap) = floodFillZones grid
      Map.size clsMap `shouldSatisfy` (>= 4)

  describe "buildFromDescriptor" $ do
    let gm = buildFromDescriptor manitoba1mi

    it "is deterministic (same input → same output)" $ do
      let gm1 = buildFromDescriptor manitoba1mi
          gm2 = buildFromDescriptor manitoba1mi
      gmGraph gm1 `shouldBe` gmGraph gm2

    it "changes output when the descriptor changes" $ do
      let gm1 = buildFromDescriptor manitoba1mi
          gm2 = buildFromDescriptor manitoba1miAlt
      lgCoords (gmGraph gm1) `shouldNotBe` lgCoords (gmGraph gm2)

    it "produces a non-trivial number of locations" $ do
      length (gmLocations gm) `shouldSatisfy` (>= 8)

    it "assigns every location a region" $ do
      let locs  = gmLocations gm
          rMap  = lgRegions (gmGraph gm)
      mapM_ (\l -> Map.member l rMap `shouldBe` True) locs

    it "assigns every location a coordinate in [0,1]^2" $ do
      let locs  = gmLocations gm
          cMap  = lgCoords (gmGraph gm)
      mapM_ (\l -> case Map.lookup l cMap of
                     Just (x, y) -> do
                       x `shouldSatisfy` (\v -> v >= 0 && v <= 1)
                       y `shouldSatisfy` (\v -> v >= 0 && v <= 1)
                     Nothing     -> expectationFailure
                       ("no coord for " <> show l)
            ) locs

    it "produces a connected graph reachable from any spawn" $ do
      let locs  = gmLocations gm
          edges = lgEdges (gmGraph gm)
      case locs of
        []    -> pendingWith "no locations generated"
        (l:_) -> reachable l edges locs `shouldBe` Set.fromList locs

    it "keeps all edges between locations that actually exist" $ do
      let locs = Set.fromList (gmLocations gm)
          eds  = lgEdges (gmGraph gm)
      mapM_ (\(a, b) -> do
                Set.member a locs `shouldBe` True
                Set.member b locs `shouldBe` True
            ) (Set.toList eds)

    -- A location with only one exit reads as a dead-end: the player
    -- arrives, sees one way forward, and the HUD has no sense of
    -- "landscape."  Two neighbours is the minimum for continuity.
    it "gives every location at least two neighbours" $ do
      let locs  = gmLocations gm
          eds   = lgEdges (gmGraph gm)
          deg l = length [ ()
                         | (a, b) <- Set.toList eds
                         , a == l || b == l
                         ]
      mapM_ (\l -> deg l `shouldSatisfy` (>= 2)) locs

    -- Shared-neighbour continuity: when the player walks from A to B,
    -- B's neighbours should include some of A's neighbours (or A's
    -- near-neighbours) so the map feels geometrically continuous
    -- rather than teleporting between disjoint scene graphs.  We
    -- sample all edges and check that, on average, >= 1 of A's
    -- neighbours is also a neighbour of B.  Empirically the new
    -- sector-fill edge policy gives well over that; the bound is
    -- deliberately loose so future tuning doesn't break the spec.
    it "makes neighbours share at least one neighbour on average" $ do
      let locs  = gmLocations gm
          eds   = lgEdges (gmGraph gm)
          nbrs l = Set.fromList $
            [ if a == l then b else a
            | (a, b) <- Set.toList eds
            , a == l || b == l
            ]
          shared a b =
            Set.size (Set.intersection (nbrs a) (nbrs b)
                      `Set.difference` Set.fromList [a, b])
          sharedCounts = [ shared a b | (a, b) <- Set.toList eds ]
          avg = fromIntegral (sum sharedCounts)
                / fromIntegral (max 1 (length sharedCounts)) :: Double
      length locs `shouldSatisfy` (> 0)
      avg `shouldSatisfy` (>= 1.0)

    it "produces the canonical set of biome region names" $ do
      let regions = Set.fromList
            [ name | Region name <- Map.elems (lgRegions (gmGraph gm)) ]
          expected = Set.fromList
            ["Field", "Road", "Bush", "Ridge", "Creek"]
      -- Region names use cardinal prefixes like "North Field", "East Road",
      -- so we check that every generated region contains one of the
      -- known biome words.
      let matches name = any (`isInfix` name) (Set.toList expected)
      mapM_ (`shouldSatisfy` matches) (Set.toList regions)
      where
        isInfix needle hay = needle `Set.member` Set.fromList (words hay)

-- | BFS from one location through the edge set, returning every
-- location it can reach.  Used to check connectivity.
reachable :: Location -> Set.Set (Location, Location) -> [Location] -> Set.Set Location
reachable start eds _universe =
  let adj = adjMap eds
      go seen [] = seen
      go seen (l:ls)
        | Set.member l seen = go seen ls
        | otherwise =
            let ns = Map.findWithDefault [] l adj
            in go (Set.insert l seen) (ns ++ ls)
  in go Set.empty [start]
  where
    adjMap :: Set.Set (Location, Location) -> Map.Map Location [Location]
    adjMap =
      Set.foldl' (\m (a, b) ->
        Map.insertWith (++) a [b] $ Map.insertWith (++) b [a] m) Map.empty
