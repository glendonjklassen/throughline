{-# OPTIONS_GHC -fno-hpc    #-}
{-# LANGUAGE DeriveGeneric  #-}
-- | Internal types for PNCounter: per-player increment and decrement bucket maps.
module Engine.CRDT.PNCounter.Types where

import           Control.DeepSeq (NFData)
import qualified Data.Map.Strict as Map
import           GHC.Generics    (Generic)

-- | A grow-only counter split into per-key increment and decrement buckets.
-- Each participant writes only to their own key; merge is pointwise union.
-- The baseline (scenario-defined starting value) lives separately so it is
-- never doubled by a full-state merge.
data PNCounter k = PNCounter
  { pnBase :: Int           -- ^ scenario baseline; taken from either side on merge
  , pnP    :: Map.Map k Int -- ^ per-key positive deltas
  , pnN    :: Map.Map k Int -- ^ per-key negative deltas (stored as positive ints)
  } deriving (Show, Eq, Generic)

instance NFData k => NFData (PNCounter k)
