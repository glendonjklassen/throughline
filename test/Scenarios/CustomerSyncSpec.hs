-- =============================================================================
-- Scenarios.CustomerSyncSpec
--
-- The customer scenario is a walking simulation: a customer navigates a retail
-- store's scene graph (parking -> entrance -> salesFloor -> elec, etc.) using
-- "walk:from:to" actions. This spec verifies that headless execution, log
-- merging, and snapshot merging all behave correctly.
--
-- The two merge paths tested here are:
--   1. Snapshot merge: CRDT union of two final GameWorlds (mergeWorlds)
--   2. Log merge: merge event logs (mergeLogs), then replay from a shared
--      initial world (replayFrom)
-- Both must agree on the resulting world state.
--
-- Cross-scenario co-location interaction tests live in CoLocationSpec.
-- =============================================================================
module Scenarios.CustomerSyncSpec (spec) where

import           Data.Either            (isRight)
import qualified Data.Map.Strict        as Map
import           Test.Hspec
import           Test.QuickCheck        (ioProperty, forAll, choose, property)

import           Engine.Author.Scene       (edgeActionId)
import           Engine.Author.Validate    (validateScenario, validateSceneGraph)
import           Engine.Core.Effects       (mergeWorlds)
import           Engine.Headless           (runHeadlessRandom, runHeadlessScript)
import           Engine.Sync.EventLog      (mergeLogs, replayFrom)
import           GameTypes
import           MonadStack                (AppError)
import           Scenarios.Customer        (customer)
import           Scenarios.Customer.WalkScene (customerGraph)
import           Scenarios.TopBuy.Locations

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Run two independent customer sessions via fixed action scripts, merge
-- their event logs, and replay from scratch. This simulates two people
-- playing the same scenario separately, then combining their histories
-- into a single coherent world.
--
-- runHeadlessScript runs a scenario without terminal I/O by executing a
-- fixed sequence of action IDs. It returns (final world, accumulated log
-- entries). Each player gets their own PlayerId so their log entries are
-- distinguishable after merge.
--
-- The replay uses scenA's initial world as the starting point. Both scripts
-- must move their respective player at least once, ensuring both CharIds
-- appear in worldLocations after replay.
mergeScripts
  :: [ActionId] -> [ActionId]
  -> IO (Either AppError GameWorld)
mergeScripts scriptA scriptB = do
  -- Two independent players, each with their own PlayerId.
  let pidA = PlayerId "customer-a"
      pidB = PlayerId "customer-b"
  -- Run each player's script independently. Neither player knows about the
  -- other's actions — they are completely unsynchronized sessions.
  Right (_, logA) <- runHeadlessScript (customer 0) pidA scriptA
  Right (_, logB) <- runHeadlessScript (customer 0) pidB scriptB
  -- mergeLogs interleaves both logs in Lamport clock order, producing a
  -- single timeline that respects causality.
  let (_, merged) = mergeLogs logA logB
      scenA = customer 0 (Named "customer-a")
  -- replayFrom executes the merged log against the initial world, producing
  -- the same result as if both players had acted in this interleaved order.
  replayFrom scenA (scenarioInitial scenA) merged

-- | Fold a list of logs into a single merged log by repeatedly calling
-- mergeLogs. Each call does a Lamport-sorted union. Used for N-player
-- merges where more than two play sessions need to be combined.
-- All logs are assumed to have no shared history (no common prefix).
foldLogs :: [[LogEntry]] -> [LogEntry]
foldLogs []     = []
foldLogs (l:ls) = foldl (\acc l' -> snd (mergeLogs acc l')) l ls

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do

  -- =========================================================================
  -- Deterministic runs
  --
  -- Verify that runHeadlessScript faithfully executes a fixed action
  -- sequence and produces the expected world state. These are sanity
  -- checks before we test merging.
  -- =========================================================================

  describe "runHeadlessScript" $ do

    it "places the customer at the expected location after a fixed walk" $ do
      -- A single player walking through the store's scene graph.
      let pid = PlayerId "customer-a"
          you = Named "customer-a"    -- the CharacterId for this player
      -- Script: walk from parking to entrance, then entrance to salesFloor.
      -- The customer scenario's scene graph connects these locations.
      Right (w, _) <- runHeadlessScript (customer 0) pid
        [edgeActionId parkingLot entrance, edgeActionId entrance salesFloor]
      -- After two walks, the customer should be on the sales floor.
      Map.lookup you (worldLocations w) `shouldBe` Just salesFloor

    it "same script produces the same world regardless of PlayerId" $ do
      -- The script is deterministic — same actions, same outcome. PlayerId
      -- only affects log metadata (who did it), not the world state. Two
      -- different players running the same script should end up at the same
      -- locations.
      let script = [edgeActionId parkingLot entrance, edgeActionId entrance salesFloor, edgeActionId salesFloor electronics]
      Right (w1, _) <- runHeadlessScript (customer 0) (PlayerId "c-1") script
      Right (w2, _) <- runHeadlessScript (customer 0) (PlayerId "c-2") script
      -- Compare location values only (keys differ because CharIds differ).
      let loc1 = Map.elems (worldLocations w1)
          loc2 = Map.elems (worldLocations w2)
      loc1 `shouldBe` loc2

  -- =========================================================================
  -- Random runs
  --
  -- runHeadlessRandom picks actions randomly from available ones using a
  -- seeded RNG. It takes a step count and seed, returning (final world,
  -- log). Same seed + same PlayerId = deterministic replay.
  -- =========================================================================

  describe "runHeadlessRandom" $ do

    it "same seed and PlayerId produces the same log" $ do
      let pid = PlayerId "customer-a"
      -- Both runs use seed 42 and 10 steps. Deterministic RNG means
      -- identical action choices, identical logs.
      Right (_, log1) <- runHeadlessRandom (customer 0) pid 10 42
      Right (_, log2) <- runHeadlessRandom (customer 0) pid 10 42
      log1 `shouldBe` log2

    it "different seeds produce different logs (almost always)" $ do
      let pid = PlayerId "customer-a"
      -- Seeds 1 and 2 should produce different random walks through the
      -- store. Not a hard guarantee (RNG could theoretically collide),
      -- but true for these seeds and this action graph.
      Right (_, log1) <- runHeadlessRandom (customer 0) pid 10 1
      Right (_, log2) <- runHeadlessRandom (customer 0) pid 10 2
      log1 `shouldNotBe` log2

  -- =========================================================================
  -- Two-player merge
  --
  -- Two customers play independently and their logs are merged. After
  -- replay, both players' locations should be present in the merged world.
  -- =========================================================================

  describe "two-player merge" $ do

    it "both players' locations are present after merge" $ do
      -- Player A walks to salesFloor (2 steps), player B only reaches entrance (1 step).
      result <- mergeScripts
        [edgeActionId parkingLot entrance, edgeActionId entrance salesFloor]
        [edgeActionId parkingLot entrance]
      case result of
        Left err -> expectationFailure ("merge failed: " <> show err)
        Right w  -> do
          -- After merge-replay, both players exist in the world with their
          -- respective final locations preserved.
          Map.lookup (Named "customer-a") (worldLocations w) `shouldBe` Just salesFloor
          Map.lookup (Named "customer-b") (worldLocations w) `shouldBe` Just entrance

    it "merge is commutative: A into B and B into A give the same locations" $ do
      -- Commutativity is a CRDT guarantee. mergeLogs(A,B) and mergeLogs(B,A)
      -- may produce different entry orderings, but after replay the final
      -- world state must be identical.
      let pidA = PlayerId "customer-a"
          pidB = PlayerId "customer-b"
          youA = Named "customer-a"   -- CharacterId for player A
          youB = Named "customer-b"   -- CharacterId for player B
          scenA = customer 0 youA     -- scenario parameterized for player A
          scenB = customer 0 youB     -- scenario parameterized for player B
      -- Player A walks to salesFloor, player B walks to entrance.
      Right (_, logA) <- runHeadlessScript (customer 0) pidA
        [edgeActionId parkingLot entrance, edgeActionId entrance salesFloor]
      Right (_, logB) <- runHeadlessScript (customer 0) pidB
        [edgeActionId parkingLot entrance]
      -- Merge in both directions.
      let (_, mergedAB) = mergeLogs logA logB  -- A's log first
          (_, mergedBA) = mergeLogs logB logA  -- B's log first
      -- Replay each merged log from the respective scenario's initial world.
      Right wAB <- replayFrom scenA (scenarioInitial scenA) mergedAB
      Right wBA <- replayFrom scenB (scenarioInitial scenB) mergedBA
      -- Both orderings must produce the same locations for both players.
      Map.lookup youA (worldLocations wAB) `shouldBe` Map.lookup youA (worldLocations wBA)
      Map.lookup youB (worldLocations wAB) `shouldBe` Map.lookup youB (worldLocations wBA)

  -- =========================================================================
  -- N-player merge
  --
  -- Five players run random walks independently. Their logs are folded into
  -- a single merged log via foldLogs, then replayed. All five players must
  -- have locations in the resulting world.
  -- =========================================================================

  describe "N-player merge" $ do

    it "all five players' locations survive a fold merge" $ do
      -- Five players, each with a unique PlayerId and RNG seed.
      let pids   = map (\n -> PlayerId ("c-" <> show n)) [1..5 :: Int]
          youOf n = Named ("c-" <> show (n :: Int))  -- CharacterId for player n
          scenOf  = customer 0 . youOf                -- scenario for player n
          seeds  = [10, 20, 30, 40, 50]               -- different seeds = different walks
      -- Run each player for 6 random steps.
      results <- mapM (\(pid, seed) -> runHeadlessRandom (customer 0) pid 6 seed)
                   (zip pids seeds)
      -- Extract logs from successful runs.
      let logs = [log' | Right (_, log') <- results]
      length logs `shouldBe` 5  -- all five runs must succeed
      -- Fold all five logs into a single merged timeline.
      let merged = foldLogs logs
      -- Replay the merged log from player 1's initial world.
      result <- replayFrom (scenOf 1) (scenarioInitial (scenOf 1)) merged
      case result of
        Left err -> expectationFailure ("N-player merge failed: " <> show err)
        -- Every player must have a location entry in the merged world.
        Right w  -> mapM_ (\n -> Map.member (youOf n) (worldLocations w) `shouldBe` True) [1..5 :: Int]

  -- =========================================================================
  -- QuickCheck: merge convergence
  --
  -- Property-based tests that hold for arbitrary seed/step combinations.
  -- These catch edge cases that deterministic scripts might miss.
  -- =========================================================================

  describe "merge properties" $ do

    -- Invariant: after merging two random customer sessions, BOTH players
    -- must have location entries in the resulting world. A merge that loses
    -- a player's location data is a convergence failure.
    it "two random customers both have locations after merge" $
      property $ \s1 s2 ->
        forAll (choose (1, 8 :: Int)) $ \steps -> ioProperty $ do
          let pidA = PlayerId "customer-a"
              pidB = PlayerId "customer-b"
              youA = Named "customer-a"   -- CharacterId for player A
              youB = Named "customer-b"   -- CharacterId for player B
              scenA = customer 0 youA
          -- Run both players for the same number of random steps with
          -- different seeds (s1, s2 from QuickCheck).
          Right (_, logA) <- runHeadlessRandom (customer 0) pidA steps s1
          Right (_, logB) <- runHeadlessRandom (customer 0) pidB steps s2
          -- Merge logs and replay.
          let (_, merged) = mergeLogs logA logB
          result <- replayFrom scenA (scenarioInitial scenA) merged
          case result of
            Left _  -> return False
            Right w ->
              -- Both players must exist in the merged world's locations.
              return ( Map.member youA (worldLocations w)
                    && Map.member youB (worldLocations w) )

    -- Invariant: the two merge paths must agree on final locations.
    --
    -- Snapshot merge (mergeWorlds): CRDT union of two final worlds. This is
    -- a direct structural merge with no replay — it unions the location maps,
    -- stat maps, etc. Fast but coarse.
    --
    -- Log merge (mergeLogs + replayFrom): interleave the event logs in
    -- Lamport order, then replay every action from scratch against the
    -- initial world. Slower but semantically precise.
    --
    -- Because each player only ever sets their own location (no player moves
    -- another player), both paths must produce identical location maps. If
    -- they disagree, either the CRDT merge or the replay has a bug.
    it "snapshot merge and log replay agree on locations for all players" $
      property $ \s1 s2 ->
        forAll (choose (1, 8 :: Int)) $ \steps -> ioProperty $ do
          let pidA = PlayerId "customer-a"
              pidB = PlayerId "customer-b"
              youA = Named "customer-a"
              youB = Named "customer-b"
              scenA = customer 0 youA
          Right (wA, logA) <- runHeadlessRandom (customer 0) pidA steps s1
          Right (wB, logB) <- runHeadlessRandom (customer 0) pidB steps s2
          -- Path 1: snapshot merge — CRDT union of the two final worlds.
          let snap          = mergeWorlds wA wB
          -- Path 2: log merge — interleave logs, replay from initial world.
              (_, merged)   = mergeLogs logA logB
          result <- replayFrom scenA (scenarioInitial scenA) merged
          case result of
            Left _      -> return False
            Right replay ->
              -- Both paths must agree on where each player ended up.
              return ( Map.lookup youA (worldLocations snap) == Map.lookup youA (worldLocations replay)
                    && Map.lookup youB (worldLocations snap) == Map.lookup youB (worldLocations replay) )

  -- =========================================================================
  -- Validation
  --
  -- Static checks on the customer scenario's structure. These catch
  -- authoring errors (duplicate ActionIds, disconnected scene graphs)
  -- at test time rather than at runtime.
  -- =========================================================================

  describe "validation" $ do
    -- Every ActionId in the scenario must be unique. Duplicates would cause
    -- ambiguous action resolution during headless execution and log replay.
    it "customer scenario has no duplicate ActionIds" $
      isRight (validateScenario (customer 0 (Named "test"))) `shouldBe` True
    -- The scene graph must be connected: every location must be reachable
    -- from every other location. A disconnected graph means a player could
    -- get stuck with no available walk actions.
    it "customer scene graph is connected" $
      isRight (validateSceneGraph customerGraph) `shouldBe` True
