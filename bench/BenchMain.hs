module Main (main) where

import           Test.Tasty.Bench

import           EngineBench      (engineBenchmarks)
import           IntegrationBench (integrationBenchmarks)

main :: IO ()
main = defaultMain
  [ engineBenchmarks
  , integrationBenchmarks
  ]
