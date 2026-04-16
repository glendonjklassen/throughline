module IntegrationBench (integrationBenchmarks) where

import           Control.DeepSeq     (rnf)
import           Control.Exception   (evaluate)

import           Test.Tasty.Bench

import           Engine.Headless      (runHeadlessRandom)
import           Engine.Sync.EventLog (replayFrom)
import           GameTypes
import           Scenarios.Customer   (customer)
import           Scenarios.TopBuy     (topBuy)

-- ---------------------------------------------------------------------------
-- Top-level benchmark group
-- ---------------------------------------------------------------------------

integrationBenchmarks :: Benchmark
integrationBenchmarks = bgroup "Integration"
  [ scenarioBench
  , replayBench
  ]

-- ---------------------------------------------------------------------------
-- Full scenario headless runs
-- ---------------------------------------------------------------------------

scenarioBench :: Benchmark
scenarioBench = bgroup "Scenario"
  [ bench "Customer 50 ticks"  $ nfIO (run customer  50)
  , bench "Customer 200 ticks" $ nfIO (run customer 200)
  , bench "TopBuy 50 ticks"    $ nfIO (run topBuy    50)
  , bench "TopBuy 200 ticks"   $ nfIO (run topBuy   200)
  ]
  where
    pid  = PlayerId "bench-player0"
    seed = 42
    run mkScen n = do
      result <- runHeadlessRandom mkScen pid n seed
      case result of
        Left err     -> error ("Benchmark scenario failed: " <> show err)
        Right (w, _) -> evaluate (rnf w) >> pure w

-- ---------------------------------------------------------------------------
-- Log replay at scale
-- ---------------------------------------------------------------------------

replayBench :: Benchmark
replayBench = bgroup "Replay"
  [ env (generateEntries 50)  $ \ ~(w, entries) ->
      bench "replay 50 entries"  $ nfIO (replayCustomer w entries)
  , env (generateEntries 200) $ \ ~(w, entries) ->
      bench "replay 200 entries" $ nfIO (replayCustomer w entries)
  ]
  where
    pid  = PlayerId "bench-player0"
    seed = 42
    you  = Named (take 12 "bench-player0")

    generateEntries n = do
      result <- runHeadlessRandom customer pid n seed
      case result of
        Left err           -> error ("Benchmark log generation failed: " <> show err)
        Right (_, entries) -> do
          let w0 = scenarioInitial (customer you)
          evaluate (rnf entries)
          evaluate (rnf w0)
          pure (w0, entries)

    replayCustomer w entries = do
      let scen = customer you
      result <- replayFrom scen w entries
      case result of
        Left err -> error ("Benchmark replay failed: " <> show err)
        Right w' -> evaluate (rnf w') >> pure w'
