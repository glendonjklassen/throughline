{-# OPTIONS_GHC -fno-hpc    #-}
{-# LANGUAGE DeriveGeneric  #-}
-- | Internal types for ORSet: element entries keyed by unique tokens.
module Engine.CRDT.ORSet.Types where

import           Control.DeepSeq (NFData)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           GHC.Generics    (Generic)
import           Data.UUID       (UUID)

data ORSet a = ORSet
  { orEntries    :: Map.Map a (Set.Set UUID)
  , orTombstones :: Set.Set UUID
  } deriving (Show, Eq, Generic)

instance NFData a => NFData (ORSet a)
