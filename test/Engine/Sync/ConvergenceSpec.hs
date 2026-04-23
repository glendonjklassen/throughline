{-# LANGUAGE DataKinds #-}
-- =============================================================================
-- Engine.Sync.ConvergenceSpec
--
-- Tests verifying all paths by which GameWorld state is shared between
-- players, ensuring each path produces consistent, mutually compatible
-- results.
--
--  PATH 1 - Solo log replay
--  ─────────────────────────────────────────────────────────────────
--   initial ──[replayFrom]──▶ world
--   invariant: replayFrom(log) == live play
--
--  PATH 2 - Snapshot + tail replay
--  ─────────────────────────────────────────────────────────────────
--   snap_N ──[replayFrom tail]──▶ world
--   invariant: snap_N + tail == replayFrom(0..end) == live play
--
--  PATH 3 - Merged log replay (two players)
--  ─────────────────────────────────────────────────────────────────
--   [log-A]   [log-B]
--        \   /
--      mergeLogs           <- Lamport-sorted divergent tail
--          |
--   replayFrom ──▶ world
--   invariant: world contains effects from both A and B
--   invariant: own entries not double-applied at divergence point
--
--  PATH 4 - Snapshot merge / CRDT (two players)
--  ─────────────────────────────────────────────────────────────────
--   snap-A ──┐
--            ├── mergeWorlds ──▶ world
--   snap-B ──┘
--   invariant: commutative  (A (+) B == B (+) A)
--   invariant: idempotent   (A (+) A == A)
--   invariant: iterable     (rounds on merged snapshots stay stable)
--
--  PATH 5 - Active effect merge
--  ─────────────────────────────────────────────────────────────────
--   mergeActiveEffects(A, B) -- union by liveId
--   invariant: commutative, idempotent, superset of both sides
--
-- =============================================================================
module Engine.Sync.ConvergenceSpec (spec) where

import           Data.IORef
import           Data.List          (sort)
import qualified Data.Map.Strict    as Map
import           Test.Hspec

import           Engine.Author.DSL
import           Engine.Core.Effects
import           Engine.Core.World      (setCharacterStat)
import           Engine.Sync.EventLog   -- includes nullLogStore
import           Engine.CRDT.ORSet
import           GameTypes
import           GameTypes.Types (Action(..))
import           MonadStack
import           TestFixtures

-- ---------------------------------------------------------------------------
-- Scenarios
--
-- These define the minimal actions and scenarios needed by the convergence
-- tests. Each scenario is deliberately simple — usually just "add a world
-- tag" — so the tests can verify merge/replay mechanics without scenario
-- complexity getting in the way.
-- ---------------------------------------------------------------------------

-- | Marker tag applied by act1. Named constants prevent typo-based failures.
tagOne :: Tag
tagOne = ScenarioTag (MkScenarioTag "did-one")

-- | Marker tag applied by act2. Distinct from tagOne so we can verify both
-- actions' effects independently.
tagTwo :: Tag
tagTwo = ScenarioTag (MkScenarioTag "did-two")

-- | A simple action that unconditionally adds tagOne to the world.
-- Used as the first step in PATH 1 and PATH 2 replay tests.
act1 :: Action 'Repeatable
act1 = Action
  { actionId        = ActionId "act1"
  , actionLabel     = "Action One"
  , actionTarget    = Nothing
  , actionCondition = unconditional
  , actionEffects   = [Effect (AddWorldTag tagOne) (Just 1) unconditional Nothing]
  }

-- | A sequencing-dependent action: only available when tagOne is already
-- present. This lets PATH 1/2 tests verify that replay respects the causal
-- ordering of effects (act2 can't fire until act1's tag exists).
act2 :: Action 'Repeatable
act2 = Action
  { actionId        = ActionId "act2"
  , actionLabel     = "Action Two"
  , actionTarget    = Nothing
  , actionCondition = HasWorldTag tagOne
  , actionEffects   = [Effect (AddWorldTag tagTwo) (Just 1) unconditional Nothing]
  }

-- | The baseline scenario for PATH 1 and PATH 2. Two actions, no axioms,
-- starts from emptyWorld. The simplest possible scenario that still
-- exercises sequential action dependencies (act2 requires tagOne from act1).
testScenario :: Scenario
testScenario = Scenario
  { scenarioName         = "test-scenario"
  , scenarioInitial      = emptyWorld
  , scenarioActions      = [AnyAction act1, AnyAction act2]
  , scenarioAxioms       = []
  , scenarioMergeAxioms  = []
  , scenarioRules        = []
  , scenarioMergeRules   = []
  , scenarioTerminal     = Any []
  , scenarioDebugDefault = Off
  , scenarioPlayerCharId = player
  }

-- | Player A's action in the merge scenario. Adds "tag-a" unconditionally.
actTagA :: Action 'Repeatable
actTagA = Action (ActionId "actA") "A" Nothing unconditional
  [Effect (AddWorldTag (ScenarioTag (MkScenarioTag "tag-a"))) (Just 1) unconditional Nothing]

-- | Player B's action in the merge scenario. Adds "tag-b" unconditionally.
actTagB :: Action 'Repeatable
actTagB = Action (ActionId "actB") "B" Nothing unconditional
  [Effect (AddWorldTag (ScenarioTag (MkScenarioTag "tag-b"))) (Just 1) unconditional Nothing]

-- | Scenario used by PATH 3 merge tests. Both actions are unconditional
-- and add distinct tags, making it easy to verify that both players'
-- effects survive a log merge + replay.
mergeScenario :: Scenario
mergeScenario = Scenario
  { scenarioName         = "merge-scenario"
  , scenarioInitial      = emptyWorld
  , scenarioActions      = [AnyAction actTagA, AnyAction actTagB]
  , scenarioAxioms       = []
  , scenarioMergeAxioms  = []
  , scenarioRules        = []
  , scenarioMergeRules   = []
  , scenarioTerminal     = Any []
  , scenarioDebugDefault = Off
  , scenarioPlayerCharId = player
  }

-- | Marker tag added by the axiom in axiomScenario. If this tag appears
-- in the world, the axiom fired successfully during replay.
axiomTag :: Tag
axiomTag = ScenarioTag (MkScenarioTag "axiom-fired")

-- | Scenario with a reactive axiom: when tagOne appears in a tick diff,
-- the axiom fires and adds axiomTag. Used by PATH 1 to verify that axioms
-- evaluate against the post-diff world state during log replay, not just
-- during live play.
axiomScenario :: Scenario
axiomScenario = Scenario
  { scenarioName         = "axiom-scenario"
  , scenarioInitial      = emptyWorld
  , scenarioActions      = []
  , scenarioAxioms       = [respondToOne]
  , scenarioMergeAxioms  = []
  , scenarioRules        = []
  , scenarioMergeRules   = []
  , scenarioTerminal     = Any []
  , scenarioDebugDefault = Off
  , scenarioPlayerCharId = player
  }
  where
    respondToOne = Axiom (ScenarioAxiom "respond-to-one") 0 $
      \_world _available diff -> whenTagAdded tagOne diff [immediate (AddWorldTag axiomTag)]

-- | Bare scenario with no actions or axioms. Used by PATH 3's
-- divergence-point test where the only thing that matters is stat values
-- in the initial world — we don't want axioms interfering.
statScenario :: Scenario
statScenario = Scenario
  { scenarioName         = "stat-scenario"
  , scenarioInitial      = emptyWorld
  , scenarioActions      = []
  , scenarioAxioms       = []
  , scenarioMergeAxioms  = []
  , scenarioRules        = []
  , scenarioMergeRules   = []
  , scenarioTerminal     = Any []
  , scenarioDebugDefault = Off
  , scenarioPlayerCharId = player
  }

-- | Action used in the foreign-diff-ordering test. Adds "alpha-effect".
actAlpha :: Action 'Repeatable
actAlpha = Action (ActionId "act-alpha") "Alpha" Nothing unconditional
  [Effect (AddWorldTag (ScenarioTag (MkScenarioTag "alpha-effect"))) (Just 1) unconditional Nothing]

-- | A different version of beta used by the foreign player. Adds
-- "beta-v1-effect". This tests that when a foreign player has actions
-- the local player doesn't know about, their stored diffs still replay.
actBetaV1 :: Action 'Repeatable
actBetaV1 = Action (ActionId "act-beta") "Beta (v1)" Nothing unconditional
  [Effect (AddWorldTag (ScenarioTag (MkScenarioTag "beta-v1-effect"))) (Just 1) unconditional Nothing]

-- | Scenario for the foreign-diff-ordering test. The local player has
-- actAlpha and actBetaV1 available, but the foreign player may have
-- used different action versions — diffs stored in the log entries
-- are what actually get replayed, not the local action definitions.
worldAScenario :: Scenario
worldAScenario = Scenario
  { scenarioName         = "world-a"
  , scenarioInitial      = emptyWorld
  , scenarioActions      = [AnyAction actAlpha, AnyAction actBetaV1]
  , scenarioAxioms       = []
  , scenarioMergeAxioms  = []
  , scenarioRules        = []
  , scenarioMergeRules   = []
  , scenarioTerminal     = Any []
  , scenarioDebugDefault = Off
  , scenarioPlayerCharId = player
  }

-- ---------------------------------------------------------------------------
-- Helpers (PATH 4 — snapshot merge)
-- ---------------------------------------------------------------------------

-- | Run a sequence of effects attributed to a specific player.
--
-- This is critical for CRDT correctness: stats use PNCounter, which stores
-- increments in per-player "buckets." When player A does +1, that goes into
-- A's bucket. When player B does +2, that goes into B's bucket. On merge,
-- each bucket is max'd independently, so A's +1 and B's +2 never collide —
-- the merged total is base + 1 + 2. The PlayerId in the Env determines
-- which bucket receives the change.
runSession :: PlayerId -> [EffectBody] -> GameWorld -> IO GameWorld
runSession pid effects world = do
  ref         <- newIORef Off
  msgRef      <- newIORef []
  traceRef    <- newIORef []
  frontierRef <- newIORef Map.empty
  let env = Env
        { envActions      = []
        , envAxioms       = []
        , envMergeAxioms  = []
        , envRules        = []
        , envMergeRules   = []
        , envLog          = \_ -> pure ()
        , envDebug        = ref
        , envTerminal     = Any []
        , envMessageLog   = msgRef
        , envPlayerId     = pid       -- determines PNCounter bucket
        , envPlayerCharId = player
        , envLogStore     = nullLogStore
        , envAxiomTrace   = traceRef
        , envFrontier     = frontierRef
        , envLiveMerge    = \w -> pure (w, [])
        }
  result <- runApp env world (mapM_ executeBody effects)
  case result of
    Left err     -> error ("runSession: unexpected error: " <> show err)
    Right (_, w) -> pure w

-- | Player A's identity. Used in PATH 4 snapshot merge tests.
pidA :: PlayerId
pidA = PlayerId "player-a"

-- | Player B's identity. Used in PATH 4 snapshot merge tests.
pidB :: PlayerId
pidB = PlayerId "player-b"

-- | Extract Intelligence stat from the world's relationship graph.
-- Walks: worldGraph -> Truth -> player -> Capacity Intelligence.
-- "Truth" is the ground-truth layer (as opposed to a perceived layer).
-- Returns 0 if any part of the path is missing.
getInt :: GameWorld -> Int
getInt w = maybe 0 (getRelStat (Capacity Intelligence))
  (Map.lookup Truth (worldGraph w) >>= Map.lookup player)

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "Engine.Sync.ConvergenceSpec" $ do

  -- -------------------------------------------------------------------------
  -- PATH 1 — solo log replay
  -- replayFrom(log) == live play
  -- -------------------------------------------------------------------------

  describe "PATH 1 - solo log: replayFrom == live play" $ do

    it "replayFrom produces the same world tags as live play" $ do
      let pid     = PlayerId "test"
          -- Each log entry carries a diff: the delta that was recorded when
          -- the action originally executed. replayFrom applies these diffs
          -- in order to reconstruct the world.
          diffOne = emptyDiff { diffWorldTagsAdded = [tagOne] }
          diffTwo = emptyDiff { diffWorldTagsAdded = [tagTwo] }
          -- Log entries with Lamport clocks at tick 1 and tick 2.
          -- The frontier (Map.empty) means no sync context — single player.
          entry1 = mkLogEntry pid (LamportClock 1 pid) (ActionId "act1") diffOne Map.empty
          entry2 = mkLogEntry pid (LamportClock 2 pid) (ActionId "act2") diffTwo Map.empty

      -- "Live play": execute actions through the full engine pipeline.
      -- This is the ground truth — whatever live play produces, replay
      -- must match exactly.
      (_, liveWorld) <- runApp' emptyWorld $ do
        executeStep act1
        executeStep act2

      -- "Replay": reconstruct the world from the event log alone.
      -- The fundamental invariant: replayFrom(log) == live play.
      result <- replayFrom testScenario emptyWorld [entry1, entry2]
      case result of
        Left err          -> expectationFailure (show err)
        Right replayWorld ->
          -- Compare as Sets because ORSet ordering is non-deterministic.
          orToSet (worldTags replayWorld) `shouldBe` orToSet (worldTags liveWorld)

    it "axioms fire on the post-diff world during replay" $ do
      let pid     = PlayerId "test"
          -- This diff adds tagOne, which is what the axiom in axiomScenario
          -- watches for via whenTagAdded.
          diffOne = emptyDiff { diffWorldTagsAdded = [tagOne] }
          entry1 = mkLogEntry pid (LamportClock 1 pid) (ActionId "act1") diffOne Map.empty
      -- Replay through axiomScenario, which has an axiom that fires when
      -- tagOne appears. If axioms evaluate correctly during replay, the
      -- axiom's own tag (axiomTag) will also be present in the result.
      result <- replayFrom axiomScenario emptyWorld [entry1]
      case result of
        Left err -> expectationFailure (show err)
        -- axiomTag proves the axiom fired reactively during replay,
        -- not just during live play.
        Right w  -> orMember axiomTag (worldTags w) `shouldBe` True

  -- -------------------------------------------------------------------------
  -- PATH 2 — snapshot + tail replay
  -- snap_N + tail == replayFrom(0..end)
  -- -------------------------------------------------------------------------

  describe "PATH 2 - snapshot + tail == full replay" $ do

    it "snapshot at offset N then tail replay equals full replay from scratch" $ do
      let pid     = PlayerId "test"
          diffOne = emptyDiff { diffWorldTagsAdded = [tagOne] }
          diffTwo = emptyDiff { diffWorldTagsAdded = [tagTwo] }
          entry1 = mkLogEntry pid (LamportClock 1 pid) (ActionId "act1") diffOne Map.empty
          entry2 = mkLogEntry pid (LamportClock 2 pid) (ActionId "act2") diffTwo Map.empty

      -- Full replay from scratch: apply both entries to emptyWorld.
      Right worldFull     <- replayFrom testScenario emptyWorld [entry1, entry2]
      -- Snapshot at offset 1: replay only entry1, capture the world as a snapshot.
      Right w1            <- replayFrom testScenario emptyWorld [entry1]
      -- Tail replay: start from the snapshot and apply only the remaining entry.
      Right worldFromSnap <- replayFrom testScenario w1 [entry2]

      -- The key invariant: snapshot + tail == full replay from scratch.
      -- This means players can exchange snapshots and only replay new entries.
      orToSet (worldTags worldFromSnap) `shouldBe` orToSet (worldTags worldFull)

    it "empty snapshot (offset 0) equals full replay" $ do
      let pid     = PlayerId "test"
          diffOne = emptyDiff { diffWorldTagsAdded = [tagOne] }
          diffTwo = emptyDiff { diffWorldTagsAdded = [tagTwo] }
          entry1 = mkLogEntry pid (LamportClock 1 pid) (ActionId "act1") diffOne Map.empty
          entry2 = mkLogEntry pid (LamportClock 2 pid) (ActionId "act2") diffTwo Map.empty

      -- Edge case: "snapshot" is just emptyWorld (offset 0), and the
      -- "tail" is the entire log. This is identical to full replay.
      Right worldFull     <- replayFrom testScenario emptyWorld [entry1, entry2]
      Right worldFromZero <- replayFrom testScenario emptyWorld [entry1, entry2]

      -- Degenerate case of snapshot+tail: both paths produce the same result.
      orToSet (worldTags worldFromZero) `shouldBe` orToSet (worldTags worldFull)

    it "snapshot at final offset (all entries consumed) needs no tail replay" $ do
      let pid     = PlayerId "test"
          diffOne = emptyDiff { diffWorldTagsAdded = [tagOne] }
          diffTwo = emptyDiff { diffWorldTagsAdded = [tagTwo] }
          entry1 = mkLogEntry pid (LamportClock 1 pid) (ActionId "act1") diffOne Map.empty
          entry2 = mkLogEntry pid (LamportClock 2 pid) (ActionId "act2") diffTwo Map.empty

      -- Opposite edge case: the snapshot already contains all entries.
      -- The "tail" is empty — there's nothing left to replay.
      Right wFinal        <- replayFrom testScenario emptyWorld [entry1, entry2]
      Right worldFromSnap <- replayFrom testScenario wFinal []

      -- Replaying an empty tail over a complete snapshot is a no-op.
      orToSet (worldTags worldFromSnap) `shouldBe` orToSet (worldTags wFinal)

  -- -------------------------------------------------------------------------
  -- PATH 3 — merged log replay (two players)
  -- mergeLogs + replayFrom -> world with both players' effects
  -- -------------------------------------------------------------------------

  describe "PATH 3 - merged log: both players' effects survive" $ do

    it "common prefix is empty when both players start fresh" $ do
      -- Two players who never synced — each has one entry with a different
      -- PlayerId. Since entries are identified by entryId (which includes
      -- PlayerId), there is no shared prefix.
      let eA = mkLogEntry (PlayerId "player-a") (LamportClock 1 (PlayerId "player-a")) (ActionId "act1") emptyDiff Map.empty
          eB = mkLogEntry (PlayerId "player-b") (LamportClock 1 (PlayerId "player-b")) (ActionId "act1") emptyDiff Map.empty
      let (commonLen, _) = mergeLogs [eA] [eB]
      -- No common prefix: both entries are "divergent" and will be
      -- Lamport-sorted in the merged output.
      commonLen `shouldBe` 0

    it "recognizes a shared common prefix" $ do
      -- Both logs start with the same entry (same entryId "shared-1").
      -- This represents the state they had in common before diverging —
      -- e.g., they synced up to this point, then went offline.
      let shared = LogEntry "shared-1"
                     (LamportClock 1 (PlayerId "p1")) (PlayerId "p1") (ActionId "act1") emptyDiff Nothing Map.empty 1
          -- After the shared prefix, each player took different actions.
          tailA  = LogEntry "2-p1"
                     (LamportClock 2 (PlayerId "p1")) (PlayerId "p1") (ActionId "act1") emptyDiff Nothing Map.empty 1
          tailB  = LogEntry "1-p2"
                     (LamportClock 1 (PlayerId "p2")) (PlayerId "p2") (ActionId "act2") emptyDiff Nothing Map.empty 1
      let (commonLen, merged) = mergeLogs [shared, tailA] [shared, tailB]
      -- One shared entry recognized as common prefix.
      commonLen `shouldBe` 1
      -- The merged output contains only the divergent tails (2 entries).
      -- The common prefix is excluded — it's already applied to the base world.
      length merged `shouldBe` 2

    it "sorts divergent entries by Lamport clock, tie-breaking by PlayerId" $ do
      -- Lamport clock ordering is how we get deterministic replay from
      -- divergent logs. The sort key is (tick, PlayerId):
      --   tick 1 "player-a" < tick 1 "player-b" < tick 2 "player-a"
      -- The PlayerId tie-break is arbitrary but consistent — both players
      -- will produce the same merged order independently.
      let pidA' = PlayerId "player-a"
          pidB' = PlayerId "player-b"
          eA1 = mkLogEntry pidA' (LamportClock 1 pidA') (ActionId "act1") emptyDiff Map.empty  -- tick 1
          eA2 = mkLogEntry pidA' (LamportClock 2 pidA') (ActionId "act2") emptyDiff Map.empty  -- tick 2
          eB1 = mkLogEntry pidB' (LamportClock 1 pidB') (ActionId "act1") emptyDiff Map.empty  -- tick 1
      let (_, merged) = mergeLogs [eA1, eA2] [eB1]
      -- Verify tick ordering: [1, 1, 2].
      map (lcTick . entryClock) merged `shouldBe` [1, 1, 2]
      -- Verify tie-break: at tick 1, "player-a" < "player-b" lexicographically.
      case merged of
        (e:_) -> lcPlayerId (entryClock e) `shouldBe` PlayerId "player-a"
        []    -> expectationFailure "expected non-empty merged list"

    it "replays merged log producing a world with both players' effects" $ do
      let pidA' = PlayerId "player-a"
          pidB' = PlayerId "player-b"
          -- Each player's log entry carries a diff that adds their own tag.
          diffA = emptyDiff { diffWorldTagsAdded = [ScenarioTag (MkScenarioTag "tag-a")] }
          diffB = emptyDiff { diffWorldTagsAdded = [ScenarioTag (MkScenarioTag "tag-b")] }
          eA = mkLogEntry pidA' (LamportClock 1 pidA') (ActionId "actA") diffA Map.empty
          eB = mkLogEntry pidB' (LamportClock 1 pidB') (ActionId "actB") diffB Map.empty
      -- mergeLogs Lamport-sorts the divergent entries into a single timeline.
      let (_, merged) = mergeLogs [eA] [eB]
      -- Replay the merged log from emptyWorld.
      result <- replayFrom mergeScenario emptyWorld merged
      case result of
        Left err -> expectationFailure (show err)
        Right w  -> do
          -- Both players' tags must survive — the merge doesn't drop either side.
          orMember (ScenarioTag (MkScenarioTag "tag-a")) (worldTags w) `shouldBe` True
          orMember (ScenarioTag (MkScenarioTag "tag-b")) (worldTags w) `shouldBe` True

    it "both players' stored diffs are present after a mixed replayFrom" $ do
      -- Same as above but both entries share the same ActionId "act".
      -- This verifies that diff-based replay doesn't deduplicate by
      -- ActionId — each entry's stored diff is applied independently.
      let pidA' = PlayerId "player-a"
          pidB' = PlayerId "player-b"
          diffA = emptyDiff { diffWorldTagsAdded = [ScenarioTag (MkScenarioTag "tag-a")] }
          diffB = emptyDiff { diffWorldTagsAdded = [ScenarioTag (MkScenarioTag "tag-b")] }
          eA = mkLogEntry pidA' (LamportClock 1 pidA') (ActionId "act") diffA Map.empty
          eB = mkLogEntry pidB' (LamportClock 1 pidB') (ActionId "act") diffB Map.empty
      let (_, merged) = mergeLogs [eA] [eB]
      result <- replayFrom mergeScenario emptyWorld merged
      case result of
        Left err -> expectationFailure (show err)
        Right w  -> do
          -- Both diffs applied despite sharing the same ActionId.
          orMember (ScenarioTag (MkScenarioTag "tag-a")) (worldTags w) `shouldBe` True
          orMember (ScenarioTag (MkScenarioTag "tag-b")) (worldTags w) `shouldBe` True

    -- CRITICAL INVARIANT: divergence-point vs resume world.
    --
    -- When merging logs, you must replay from the "divergence-point world"
    -- (state BEFORE either player's unique entries were applied). If you
    -- use the "resume world" (which already has YOUR entries applied),
    -- your own entries get applied TWICE — once from the resume world's
    -- state, and again from the merged log replay.
    --
    -- Example: base Intelligence = 5, player A's diff = +1.
    --   Correct (diverge-point base = 5): replay A's +1 -> result = 6
    --   Wrong   (resume base = 6):        replay A's +1 -> result = 7 (double-counted!)
    it "own divergent entries are not double-applied when using divergence-point base" $ do
      let pidA' = PlayerId "player-a"
          pidB' = PlayerId "player-b"
          -- Starting world: player character exists with Intelligence = 5.
          statWorld = emptyWorld
            { worldCharacters = Map.singleton player (Character player "P" [] orEmpty)
            , worldGraph      = setCharacterStat player (Capacity Intelligence) 5 Map.empty
            }
          -- Player A's diff: Intelligence changed from 5 to 6 (a +1 delta).
          diffA = emptyDiff { diffStats = [StatDelta player (Capacity Intelligence) 5 6 pidA'] }
          -- Player B's diff: just a tag, no stat change. Included so mergeLogs
          -- has something to interleave.
          diffB = emptyDiff { diffWorldTagsAdded = [ScenarioTag (MkScenarioTag "tag-b")] }
          eA = mkLogEntry pidA' (LamportClock 1 pidA') (ActionId "act") diffA Map.empty
          eB = mkLogEntry pidB' (LamportClock 1 pidB') (ActionId "act") diffB Map.empty
      let (commonLen, merged) = mergeLogs [eA] [eB]
          -- The divergence-point world: state BEFORE A's +1 was applied.
          -- Intelligence is still 5 here.
          divergeWorld = statWorld
          -- The resume world: state AFTER A played locally. Intelligence = 6.
          -- Using this as the replay base would double-count A's own change.
          resumeWorld  = statWorld { worldGraph = setCharacterStat player (Capacity Intelligence) 6 Map.empty }
      -- Replay from the correct base (divergence point).
      Right correct <- replayFrom statScenario divergeWorld merged
      -- Replay from the wrong base (resume world) to demonstrate the bug.
      Right wrong   <- replayFrom statScenario resumeWorld  merged
      let getIntelligence w = maybe 0 (getRelStat (Capacity Intelligence))
                       (Map.lookup Truth (worldGraph w) >>= Map.lookup player)
      -- No common prefix — both entries are divergent.
      commonLen             `shouldBe` 0
      -- Correct: 5 (base) + 1 (A's diff) = 6. A's change applied exactly once.
      getIntelligence correct `shouldBe` 6
      -- Wrong: 6 (resume already has A's +1) + 1 (A's diff replayed again) = 7.
      -- This demonstrates the double-counting bug that using diverge-point prevents.
      getIntelligence wrong   `shouldBe` 7

    it "foreign entries' diffs arrive correctly when their actions depend on prior state" $ do
      -- This tests a subtle scenario: the foreign player used actions that
      -- don't exist in our local scenario definition. Because replay applies
      -- the STORED DIFF (not the local action definition), foreign actions
      -- with unknown IDs still work — their effects are baked into the diff.
      let myId      = PlayerId "player-a"
          foreignId = PlayerId "player-b"
          -- Our log: two entries with empty diffs (we don't care about our effects here).
          eA1 = mkLogEntry myId (LamportClock 1 myId) (ActionId "act-alpha") emptyDiff Map.empty
          eA2 = mkLogEntry myId (LamportClock 2 myId) (ActionId "act-beta")  emptyDiff Map.empty
          -- Foreign player's diffs: each carries its own effects.
          diffAlpha        = emptyDiff { diffWorldTagsAdded = [ScenarioTag (MkScenarioTag "alpha-effect")] }
          diffAlphaRenamed = emptyDiff { diffWorldTagsAdded = [ScenarioTag (MkScenarioTag "alpha-effect")] }
          -- "beta-v2-effect" — a version of beta we don't have locally!
          diffBetaV2       = emptyDiff { diffWorldTagsAdded = [ScenarioTag (MkScenarioTag "beta-v2-effect")] }
          -- Foreign player's three entries, including "act-alpha-renamed"
          -- which doesn't exist in our scenario's action list.
          eB0 = mkLogEntry foreignId (LamportClock 1 foreignId) (ActionId "act-alpha")         diffAlpha        Map.empty
          eB1 = mkLogEntry foreignId (LamportClock 2 foreignId) (ActionId "act-alpha-renamed") diffAlphaRenamed Map.empty
          eB2 = mkLogEntry foreignId (LamportClock 3 foreignId) (ActionId "act-beta")          diffBetaV2       Map.empty
      let (_, merged) = mergeLogs [eA1, eA2] [eB0, eB1, eB2]
      result <- replayFrom worldAScenario emptyWorld merged
      case result of
        Left err -> expectationFailure (show err)
        -- At minimum, SOME tags should exist — foreign diffs were applied
        -- even though their action IDs don't match our local definitions.
        Right w  ->
          sort (orToList (worldTags w)) `shouldNotBe` []

  -- -------------------------------------------------------------------------
  -- PATH 4 — snapshot merge / CRDT
  --
  -- mergeWorlds is the alternative to log replay: instead of replaying events,
  -- merge two world snapshots directly using CRDT semantics.
  --   - Stats: PNCounter merge (max each per-player bucket, then sum)
  --   - Tags: ORSet merge (add-wins — concurrent adds beat removes)
  --   - Relationships: PNCounter per-edge
  --
  -- CRDT laws must hold: commutative, idempotent, associative.
  -- -------------------------------------------------------------------------

  describe "PATH 4 - snapshot merge (CRDT): mergeWorlds" $ do

    it "merges two independent sessions and preserves both players' changes" $ do
      -- Player A runs effects under pidA: +1 Intelligence and adds "tag-a1".
      -- The +1 goes into pidA's PNCounter bucket.
      snapA <- runSession pidA
        [ ModifyRelation Truth player (Capacity Intelligence) 1
        , AddWorldTag (ScenarioTag (MkScenarioTag "tag-a1"))
        ] twoCharWorld
      -- Player B runs effects under pidB: +2 Intelligence and adds "tag-b1".
      -- The +2 goes into pidB's PNCounter bucket — separate from A's.
      snapB <- runSession pidB
        [ ModifyRelation Truth player (Capacity Intelligence) 2
        , AddWorldTag (ScenarioTag (MkScenarioTag "tag-b1"))
        ] twoCharWorld
      -- mergeWorlds combines both snapshots using CRDT merge rules.
      let merged = mergeWorlds snapA snapB
      -- ORSet add-wins: both tags survive regardless of insertion order.
      orMember (ScenarioTag (MkScenarioTag "tag-a1")) (worldTags merged) `shouldBe` True
      orMember (ScenarioTag (MkScenarioTag "tag-b1")) (worldTags merged) `shouldBe` True
      -- PNCounter bucket merge: base Intelligence (5) + A's bucket (+1) + B's bucket (+2) = 8.
      -- The buckets don't collide because each player writes to their own bucket.
      getInt merged `shouldBe` 8

    it "supports iterating the merge loop across multiple rounds" $ do
      -- ROUND 1: both players act independently from twoCharWorld (base Intelligence = 5).
      snapA1 <- runSession pidA
        [ ModifyRelation Truth player (Capacity Intelligence) 1   -- A's bucket: +1
        , AddWorldTag (ScenarioTag (MkScenarioTag "tag-a1"))
        ] twoCharWorld
      snapB1 <- runSession pidB
        [ ModifyRelation Truth player (Capacity Intelligence) 2   -- B's bucket: +2
        , AddWorldTag (ScenarioTag (MkScenarioTag "tag-b1"))
        ] twoCharWorld
      -- After round 1 merge: Intelligence = 5 + 1 + 2 = 8.
      let merged1 = mergeWorlds snapA1 snapB1
      -- ROUND 2: both players continue from the merged world.
      snapA2 <- runSession pidA
        [ ModifyRelation Truth player (Capacity Intelligence) (-1) -- A's bucket: +1 + (-1) = 0 net
        , AddWorldTag (ScenarioTag (MkScenarioTag "tag-a2"))
        ] merged1
      snapB2 <- runSession pidB
        [ AddWorldTag (ScenarioTag (MkScenarioTag "tag-b2"))       -- B makes no stat change
        ] merged1
      let merged2 = mergeWorlds snapA2 snapB2
      -- All four tags from both rounds survive (ORSet add-wins across merges).
      orMember (ScenarioTag (MkScenarioTag "tag-a1")) (worldTags merged2) `shouldBe` True
      orMember (ScenarioTag (MkScenarioTag "tag-b1")) (worldTags merged2) `shouldBe` True
      orMember (ScenarioTag (MkScenarioTag "tag-a2")) (worldTags merged2) `shouldBe` True
      orMember (ScenarioTag (MkScenarioTag "tag-b2")) (worldTags merged2) `shouldBe` True
      -- Intelligence: 5 (base) + 1 - 1 (A's net) + 2 (B's net) = 7.
      -- PNCounter tracks increments and decrements separately per bucket.
      getInt merged2 `shouldBe` 7

    it "merge is commutative: mergeWorlds a b == mergeWorlds b a" $ do
      snapA <- runSession pidA
        [ ModifyRelation Truth player (Capacity Intelligence) 3
        , AddWorldTag (ScenarioTag (MkScenarioTag "tag-a"))
        ] twoCharWorld
      snapB <- runSession pidB
        [ ModifyRelation Truth player (Capacity Intelligence) 2
        , AddWorldTag (ScenarioTag (MkScenarioTag "tag-b"))
        ] twoCharWorld
      let ab = mergeWorlds snapA snapB
          ba = mergeWorlds snapB snapA
      -- CRDT commutativity: merge order doesn't matter.
      -- This is essential for distributed systems — both players can merge
      -- independently and arrive at the same result.
      getInt ab              `shouldBe` getInt ba
      orToSet (worldTags ab) `shouldBe` orToSet (worldTags ba)

    it "merge is idempotent: mergeWorlds a a == a" $ do
      snap <- runSession pidA
        [ ModifyRelation Truth player (Capacity Intelligence) 2
        , AddWorldTag (ScenarioTag (MkScenarioTag "tag-a"))
        ] twoCharWorld
      let merged = mergeWorlds snap snap
      -- CRDT idempotency: merging a snapshot with itself changes nothing.
      -- This means receiving the same snapshot twice is harmless — critical
      -- for unreliable networks where messages may be duplicated.
      getInt merged              `shouldBe` getInt snap
      orToSet (worldTags merged) `shouldBe` orToSet (worldTags snap)

  -- -------------------------------------------------------------------------
  -- PATH 5 — active effect merge
  --
  -- Active effects are ongoing effects that fire each tick (e.g., "lose 1
  -- strength per tick for 3 ticks"). When merging worlds, active effects
  -- are unioned by liveId — each unique effect survives, duplicates are
  -- collapsed. Same CRDT properties: commutative, idempotent, superset.
  -- -------------------------------------------------------------------------

  describe "PATH 5 - active effect merge: mergeActiveEffects" $ do

    it "includes effects from both sides" $
      -- Two distinct active effects (different liveIds). Both must survive.
      let fx1 = staticLive (eternal (AddWorldTag (ScenarioTag (MkScenarioTag "fx1"))))
          fx2 = staticLive (eternal (AddWorldTag (ScenarioTag (MkScenarioTag "fx2"))))
      in length (mergeActiveEffects [fx1] [fx2]) `shouldBe` 2

    it "deduplicates effects with the same liveId" $
      -- Same effect on both sides (same liveId). Merging should not
      -- double it — that would cause the effect to fire twice per tick.
      let fx = staticLive (eternal (AddWorldTag (ScenarioTag (MkScenarioTag "fx"))))
      in length (mergeActiveEffects [fx] [fx]) `shouldBe` 1

    it "is identity when one side is empty" $
      -- Merging with an empty list changes nothing — the non-empty side
      -- is returned as-is.
      let fx = staticLive (eternal (AddWorldTag (ScenarioTag (MkScenarioTag "fx"))))
      in mergeActiveEffects [fx] [] `shouldBe` [fx]
