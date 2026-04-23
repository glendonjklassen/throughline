{-# OPTIONS_GHC -fno-hpc        #-}
{-# OPTIONS_GHC -Wno-orphans   #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | Orphan Aeson instances for ORSet types.
module Engine.CRDT.ORSet.Instances where

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Data.Aeson      (ToJSON (..), FromJSON (..), object, (.=),
                                  withObject, (.:), (.:?), (.!=))

import           Engine.CRDT.ORSet.Types

instance (ToJSON a, Ord a) => ToJSON (ORSet a) where
  toJSON s = object
    [ "entries"        .= [(k, Set.toList vs) | (k, vs) <- Map.toList (orEntries s)]
    , "tombstones"     .= Set.toList (orTombstones s)
    , "tombstoneAges"  .= Map.toList (orTombstoneAges s)
    ]

-- | The @tombstoneAges@ field is optional on parse so older log
-- entries (pre-GC era) round-trip cleanly with no entries.
instance (FromJSON a, Ord a) => FromJSON (ORSet a) where
  parseJSON = withObject "ORSet" $ \o -> do
    entryList <- o .: "entries"
    tombList  <- o .: "tombstones"
    ageList   <- o .:? "tombstoneAges" .!= []
    let entries    = Map.fromList [(k, Set.fromList vs) | (k, vs) <- entryList]
        tombstones = Set.fromList tombList
        ages       = Map.fromList ageList
    pure ORSet { orEntries = entries, orTombstones = tombstones, orTombstoneAges = ages }
