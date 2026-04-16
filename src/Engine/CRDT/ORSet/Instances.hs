{-# OPTIONS_GHC -fno-hpc        #-}
{-# OPTIONS_GHC -Wno-orphans   #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | Orphan Aeson instances for ORSet types.
module Engine.CRDT.ORSet.Instances where

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Data.Aeson      (ToJSON (..), FromJSON (..), object, (.=), withObject, (.:))

import           Engine.CRDT.ORSet.Types

instance (ToJSON a, Ord a) => ToJSON (ORSet a) where
  toJSON s = object
    [ "entries"    .= [(k, Set.toList vs) | (k, vs) <- Map.toList (orEntries s)]
    , "tombstones" .= Set.toList (orTombstones s)
    ]

instance (FromJSON a, Ord a) => FromJSON (ORSet a) where
  parseJSON = withObject "ORSet" $ \o -> do
    entryList <- o .: "entries"
    tombList  <- o .: "tombstones"
    let entries    = Map.fromList [(k, Set.fromList vs) | (k, vs) <- entryList]
        tombstones = Set.fromList tombList
    pure ORSet { orEntries = entries, orTombstones = tombstones }
