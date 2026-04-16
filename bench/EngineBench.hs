module EngineBench (engineBenchmarks) where

import           Control.DeepSeq      (force)
import           Control.Exception    (evaluate)

import           Test.Tasty.Bench

import           Engine.Core.Axioms    (diffWorlds, runAxioms)
import           Engine.Core.Conditions (checkCondition)
import           Engine.Core.Effects   (mergeWorlds)
import           Engine.Sync.EventLog  (mergeLogs)
import           GameTypes

import           Fixtures

-- ---------------------------------------------------------------------------
-- Top-level benchmark group
-- ---------------------------------------------------------------------------

engineBenchmarks :: Benchmark
engineBenchmarks = bgroup "Engine"
  [ diffWorldsBench
  , conditionsBench
  , axiomsBench
  , mergeLogsBench
  , mergeWorldsBench
  ]

-- ---------------------------------------------------------------------------
-- diffWorlds
-- ---------------------------------------------------------------------------

diffWorldsBench :: Benchmark
diffWorldsBench = bgroup "diffWorlds"
  [ env (setup 5)  $ \ ~(w1, w2) -> bench  "5 characters"  $ nf (diffWorlds pid w1) w2
  , env (setup 20) $ \ ~(w1, w2) -> bench "20 characters"  $ nf (diffWorlds pid w1) w2
  , env (setup 50) $ \ ~(w1, w2) -> bench "50 characters"  $ nf (diffWorlds pid w1) w2
  , env (setup 100)$ \ ~(w1, w2) -> bench "100 characters" $ nf (diffWorlds pid w1) w2
  ]
  where
    pid = PlayerId "bench"
    setup n = evaluate $ force (scaledWorld n, scaledWorldMutated n)

-- ---------------------------------------------------------------------------
-- checkCondition
-- ---------------------------------------------------------------------------

conditionsBench :: Benchmark
conditionsBench = bgroup "checkCondition"
  [ env (setupDeep 5)  $ \ ~(w, c) -> bench "depth 5"   $ nf (checkCondition w) c
  , env (setupDeep 10) $ \ ~(w, c) -> bench "depth 10"  $ nf (checkCondition w) c
  , env (setupDeep 15) $ \ ~(w, c) -> bench "depth 15"  $ nf (checkCondition w) c
  , env (setupWide 10) $ \ ~(w, c) -> bench "width 10"  $ nf (checkCondition w) c
  , env (setupWide 50) $ \ ~(w, c) -> bench "width 50"  $ nf (checkCondition w) c
  , env (setupWide 100)$ \ ~(w, c) -> bench "width 100" $ nf (checkCondition w) c
  ]
  where
    setupDeep d = evaluate $ force (scaledWorld 20, deepCondition d)
    setupWide n = evaluate $ force (scaledWorld (max n 20), wideCondition n)

-- ---------------------------------------------------------------------------
-- runAxioms
-- ---------------------------------------------------------------------------

axiomsBench :: Benchmark
axiomsBench = bgroup "runAxioms"
  [ env setupSmall $ \ ~(w, d) -> bench "system axioms only" $ nf (runAxioms [] w []) d
  , env setupSmall $ \ ~(w, d) -> bench "10 scenario axioms"  $ nf (runAxioms (dummyAxioms 10) w []) d
  , env setupSmall $ \ ~(w, d) -> bench "50 scenario axioms"  $ nf (runAxioms (dummyAxioms 50) w []) d
  ]
  where
    setupSmall = do
      let w1 = scaledWorld 10
          w2 = scaledWorldMutated 10
          d  = diffWorlds (PlayerId "bench") w1 w2
      evaluate $ force (w2, d)

    dummyAxioms n =
      [ Axiom
          { axiomId       = ScenarioAxiom ("dummy-" ++ show i)
          , axiomPriority = 10
          , axiomEvaluate = \_ _ _ -> []
          }
      | i <- [1..n :: Int]
      ]

-- ---------------------------------------------------------------------------
-- mergeLogs
-- ---------------------------------------------------------------------------

mergeLogsBench :: Benchmark
mergeLogsBench = bgroup "mergeLogs"
  [ env (setupLogs 100 50)   $ \ ~(a, b) -> bench "100 entries, 50 shared" $ nf (mergeLogs a) b
  , env (setupLogs 500 250)  $ \ ~(a, b) -> bench "500 entries, 250 shared" $ nf (mergeLogs a) b
  , env (setupLogs 1000 500) $ \ ~(a, b) -> bench "1000 entries, 500 shared" $ nf (mergeLogs a) b
  ]
  where
    setupLogs total shared = evaluate $ force (logA, logB)
      where
        pid = PlayerId "bench"
        mkEntry i p = LogEntry
          { entryId        = show i ++ "-" ++ p
          , entryClock     = LamportClock i pid
          , entryPlayerId  = PlayerId p
          , entryActionId  = ActionId "act"
          , entryDiff      = WorldDiff [] [] [] [] [] [] []
          , entrySignature = Nothing
          }
        common     = [ mkEntry i "shared" | i <- [1..shared] ]
        divergentA = [ mkEntry (shared + i) "player-a" | i <- [1..(total - shared)] ]
        divergentB = [ mkEntry (shared + i) "player-b" | i <- [1..(total - shared)] ]
        logA = common ++ divergentA
        logB = common ++ divergentB

-- ---------------------------------------------------------------------------
-- mergeWorlds
-- ---------------------------------------------------------------------------

mergeWorldsBench :: Benchmark
mergeWorldsBench = bgroup "mergeWorlds"
  [ env (setup 5)  $ \ ~(a, b) -> bench  "5 characters" $ nf (mergeWorlds a) b
  , env (setup 20) $ \ ~(a, b) -> bench "20 characters" $ nf (mergeWorlds a) b
  , env (setup 50) $ \ ~(a, b) -> bench "50 characters" $ nf (mergeWorlds a) b
  ]
  where
    setup n = evaluate $ force (scaledWorld n, scaledWorldMutated n)
