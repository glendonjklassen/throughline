{-# OPTIONS_GHC -fno-hpc    #-}
{-# LANGUAGE DeriveGeneric  #-}
-- | Internal types for ORSet: element entries keyed by unique tokens.
module Engine.CRDT.ORSet.Types where

import           Control.DeepSeq (NFData)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Data.Time.Clock (UTCTime)
import           GHC.Generics    (Generic)
import           Data.UUID       (UUID)

-- | Observed-Remove Set.  'orTombstoneAges' records the wall-clock
-- time each tombstone was created; it's optional per-tombstone
-- (tombstones minted before age tracking was added have no entry)
-- and is used only by the GC code in 'Engine.CRDT.TombstoneGC' — the
-- core add/remove/merge semantics don't look at it.
data ORSet a = ORSet
  { orEntries       :: Map.Map a (Set.Set UUID)
  , orTombstones    :: Set.Set UUID
  , orTombstoneAges :: Map.Map UUID UTCTime
    -- ^ When each timestamped tombstone was first minted.  Populated
    -- by 'orDeleteAt' (added in v0.12); plain 'orDelete' leaves the
    -- entry unset, which the GC interprets as \"never expire\".
  } deriving (Show, Eq, Generic)

instance NFData a => NFData (ORSet a)
