{-# OPTIONS_GHC -fno-hpc     #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Orphan Aeson instances for PNCounter types.
module Engine.CRDT.PNCounter.Instances where

import           Data.Aeson       (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

import           Engine.CRDT.PNCounter.Types

instance (ToJSONKey k) => ToJSON   (PNCounter k)
instance (Ord k, FromJSONKey k) => FromJSON (PNCounter k)
