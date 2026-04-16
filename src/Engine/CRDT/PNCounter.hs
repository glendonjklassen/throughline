-- | Positive-Negative Counter: a per-player increment CRDT with high-water-mark merge.
module Engine.CRDT.PNCounter
  ( module Engine.CRDT.PNCounter.Types
  , pnZero
  , pnValue
  , pnModify
  , pnMerge
  ) where

import qualified Data.Map.Strict as Map

import           Engine.CRDT.PNCounter.Instances ()
import           Engine.CRDT.PNCounter.Types

-- | Construct a counter with the given baseline and no deltas.
pnZero :: Int -> PNCounter k
pnZero n = PNCounter { pnBase = n, pnP = Map.empty, pnN = Map.empty }

-- | The current value: baseline + sum of increments - sum of decrements.
pnValue :: PNCounter k -> Int
pnValue c = pnBase c + sum (Map.elems (pnP c)) - sum (Map.elems (pnN c))

-- | Apply a signed delta under the given key.
-- Positive delta → pnP bucket; negative delta → pnN bucket (stored positive).
pnModify :: Ord k => k -> Int -> PNCounter k -> PNCounter k
pnModify key delta c
  | delta > 0 = c { pnP = Map.insertWith (+) key delta          (pnP c) }
  | delta < 0 = c { pnN = Map.insertWith (+) key (negate delta) (pnN c) }
  | otherwise = c

-- | Merge two counters. Baseline is taken from the left (both sides agree).
-- Each player's entry is a monotonically increasing running total; merge takes
-- the maximum observed value per player (high-water mark). This makes the merge
-- idempotent: merging with a copy of yourself changes nothing.
pnMerge :: Ord k => PNCounter k -> PNCounter k -> PNCounter k
pnMerge a b = PNCounter
  { pnBase = pnBase a
  , pnP    = Map.unionWith max (pnP a) (pnP b)
  , pnN    = Map.unionWith max (pnN a) (pnN b)
  }
