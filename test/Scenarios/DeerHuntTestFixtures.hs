-- | Test fixtures for the procedurally-generated DeerHunt scenario.
--
-- Before the generator, tests used specific location identifiers like
-- @stubbleRows@ and @truckNorth@.  Those names no longer exist; the
-- map is a function of the seed.  This module gives tests stable
-- handles — "the player's starting location", "a field location in
-- the same region as the deer", "a 3-step walk into a field" — that
-- evaluate against whatever map the generator produces.
--
-- The fixtures pin a canonical seed for deterministic tests, but
-- everything takes the seed as an argument so per-test variation is
-- trivial.
module Scenarios.DeerHuntTestFixtures
  ( fixtureSeed
  , fixtureHuntWorld
  , fixtureStart
  , fixtureDeerStart
  , fixturePubkey
  , fixtureProgress
  , deerHuntForTests
  , pickByClass
  , pickAdjacentByClass
  , walkPath
  , inFieldRegion
  , anyEdgeActionId
  , coLocateAtClass
  , withoutRollover
  ) where

import           Crypto.Error            (CryptoFailable (..))
import qualified Crypto.PubKey.Ed25519   as Ed25519
import qualified Data.ByteString         as BS
import           Data.List               (find)
import qualified Data.Map.Strict         as Map
import           Data.Maybe              (fromMaybe)
import qualified Data.Set                as Set
import           Data.Time               (UTCTime (..), fromGregorian,
                                          secondsToDiffTime)

import           Engine.Sync.Progress    (LifetimeFindState (..), Progress (..))
import           GameTypes

import           Scenarios.DeerHunt            (deerHunt)
import           Scenarios.DeerHunt.Generation (GeneratedMap(..), TerrainClass(..))
import           Scenarios.DeerHunt.World      (HuntWorld(..), huntWorld, hwClass,
                                                hwLocsOfClass, hwStart, hwDeerStart)

-- | Canonical seed used across the test suite.  Using a fixed seed
-- means all these fixtures return the same locations in every run.
fixtureSeed :: Int
fixtureSeed = 0

-- | The 'HuntWorld' every test fixture consults.
fixtureHuntWorld :: HuntWorld
fixtureHuntWorld = huntWorld fixtureSeed

-- | The player's starting location for the fixture hunt.
fixtureStart :: Location
fixtureStart = hwStart fixtureHuntWorld

-- | The deer's starting location for the fixture hunt.
fixtureDeerStart :: Location
fixtureDeerStart = hwDeerStart fixtureHuntWorld

-- | A deterministic Ed25519 public key for tests.  Derived from a
-- fixed 32-byte secret seed so every test run sees the same key —
-- the white stag's pubkey-derived rendering and eligibility roll need
-- a stable input to be reproducible across CI.
fixturePubkey :: Ed25519.PublicKey
fixturePubkey = case Ed25519.secretKey (BS.replicate 32 7 :: BS.ByteString) of
  CryptoPassed sk -> Ed25519.toPublic sk
  CryptoFailed e  -> error ("fixturePubkey: secretKey failed: " <> show e)

-- | A baseline 'Progress' suitable for tests that don't care about
-- the white stag.  @huntCount = 0@ means the gamma roll is never
-- eligible (per 'gammaThreshold'), so the stag won't appear in the
-- world.  Tests that need a stag-eligible hunt should construct
-- their own 'Progress' (typically with a hunt count whose roll for
-- 'fixturePubkey' lands under the threshold) and pass it directly
-- to 'Scenarios.DeerHunt.deerHunt'.
fixtureProgress :: Progress
fixtureProgress = Progress
  { progressEpoch        = 1
  , progressHuntCount    = 0
  , progressLifetimeFind = FindPending
  , progressUpdatedAt    = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)
  }

-- | Build the DeerHunt scenario with the test fixture's progress and
-- pubkey.  Drop-in replacement for callers that previously did
-- @deerHunt fixtureSeed you@ before the white-stag wiring required
-- per-identity context.  Tests asserting white-stag behavior should
-- call 'deerHunt' directly with crafted progress.
deerHuntForTests :: Int -> CharacterId -> Scenario
deerHuntForTests seed you = deerHunt seed you fixtureProgress fixturePubkey

-- | Pick an arbitrary location of a given terrain class from the
-- fixture map.  Errors (loudly) if no location of that class exists —
-- tests that rely on this should use a class the canonical descriptor
-- always produces (field, bush, ridge, creek, road).
pickByClass :: TerrainClass -> Location
pickByClass cls = case hwLocsOfClass fixtureHuntWorld cls of
  (l:_) -> l
  []    -> error $ "pickByClass: no " <> show cls <> " locations in fixture"

-- | Pick a neighbour of the given location whose class matches.  Used
-- for "stand just outside the field, walk in" style tests.
pickAdjacentByClass :: Location -> TerrainClass -> Maybe Location
pickAdjacentByClass loc cls =
  let edges     = Set.toList (lgEdges (gmGraph (hwMap fixtureHuntWorld)))
      neighbors = [ b | (a, b) <- edges, a == loc ]
               ++ [ a | (a, b) <- edges, b == loc ]
  in find (\n -> hwClass fixtureHuntWorld n == cls) neighbors

-- | Build a walk path from the given location toward any location of
-- the given target class.  The @k@ parameter is a floor — the path
-- will be at least that long — but BFS will take the shortest
-- reachable target-class location if that's deeper.  Returns
-- @(path, finalLocation)@ where path is a list of @(from, to)@ edge
-- pairs.  Errors if no target-class location is reachable at all.
walkPath :: Location -> TerrainClass -> Int -> ([(Location, Location)], Location)
walkPath start targetClass k =
  let lg     = gmGraph (hwMap fixtureHuntWorld)
      edges  = Set.toList (lgEdges lg)
      neighbors l = [ b | (a, b) <- edges, a == l ]
                  ++ [ a | (a, b) <- edges, b == l ]
      -- BFS: (currentLoc, pathSoFar-reversed).  Stop on first node of
      -- target class whose path length is at least @k@.
      bfs [] _ = Nothing
      bfs ((cur, path) : rest) seen
        | length path >= k && hwClass fixtureHuntWorld cur == targetClass =
            Just (reverse path, cur)
        | length path > 20 = bfs rest seen    -- absolute cap, safety only
        | otherwise =
            let expansions = [ (n, (cur, n) : path)
                             | n <- neighbors cur
                             , not (Set.member n seen) ]
                seen'      = foldr (Set.insert . fst) seen expansions
            in bfs (rest ++ expansions) seen'
  in fromMaybe (error $ "walkPath: no path from " <> show start
                      <> " to a " <> show targetClass
                      <> " location within 20 hops")
     $ bfs [(start, [])] (Set.singleton start)

-- | Is the given location in a generated field region?  Field regions
-- are named with cardinal prefixes like @"North Field"@.
inFieldRegion :: Location -> Bool
inFieldRegion loc = hwClass fixtureHuntWorld loc == CField

-- | Pick any edge in the fixture graph and return its @ActionId@.
-- Used by tests that just need "some walk action" to fire.
anyEdgeActionId :: ActionId
anyEdgeActionId =
  case Set.toList (lgEdges (gmGraph (hwMap fixtureHuntWorld))) of
    ((a, b) : _) -> ActionId ("walk:" <> locationName a <> ":" <> locationName b)
    []           -> error "anyEdgeActionId: fixture map has no edges"

-- | Override a world's location map so @(player, deer)@ are both at
-- the same location of the given class.  Used by shot-outcome tests
-- that need to place both characters at a known co-located spot.
coLocateAtClass :: TerrainClass -> CharacterId -> CharacterId -> GameWorld -> GameWorld
coLocateAtClass cls player deerId w =
  let loc = pickByClass cls
  in w { worldLocations = Map.insert player loc
                       $ Map.insert deerId loc
                       $ worldLocations w }

-- | Strip the day-rollover axiom from a scenario.  Tests that assert
-- per-day world tags (deerKilled, deerGone, hunterShot) immediately
-- after the shot need the raw action outcome, not the rolled-over
-- state where those tags have already been cleared for the next day.
withoutRollover :: Scenario -> Scenario
withoutRollover s = s { scenarioAxioms = filter (not . isRollover) (scenarioAxioms s) }
  where
    isRollover ax = axiomId ax == ScenarioAxiom "dayRollover"
