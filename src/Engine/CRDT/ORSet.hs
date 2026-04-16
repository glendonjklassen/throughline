-- | Observed-Remove Set: an add-wins CRDT with UUID-keyed tombstones for conflict-free set merging.
module Engine.CRDT.ORSet
  ( module Engine.CRDT.ORSet.Types
  , orEmpty
  , orInsert
  , orUpsert
  , orDelete
  , orDeleteWhere
  , orMember
  , orToList
  , orToSet
  , orMerge
  , orFromSet
  , orSingleton
  , orFromList
  , initToken
  ) where

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict      as Map
import qualified Data.Set             as Set
import           Data.UUID            (UUID)
import qualified Data.UUID            as UUID
import qualified Data.UUID.V5         as UUID.V5

import           Engine.CRDT.ORSet.Instances ()
import           Engine.CRDT.ORSet.Types

orEmpty :: ORSet a
orEmpty = ORSet Map.empty Set.empty

orInsert :: Ord a => UUID -> a -> ORSet a -> ORSet a
orInsert uuid x s = s { orEntries = Map.insertWith Set.union x (Set.singleton uuid) (orEntries s) }

orDelete :: Ord a => a -> ORSet a -> ORSet a
orDelete x s = case Map.lookup x (orEntries s) of
  Nothing    -> s
  Just uuids -> s { orTombstones = orTombstones s `Set.union` uuids }

orDeleteWhere :: Ord a => (a -> Bool) -> ORSet a -> ORSet a
orDeleteWhere p s = foldr orDelete s [x | x <- Map.keys (orEntries s), p x]

orMember :: Ord a => a -> ORSet a -> Bool
orMember x s = case Map.lookup x (orEntries s) of
  Nothing    -> False
  Just uuids -> not (Set.null (uuids `Set.difference` orTombstones s))

orToList :: ORSet a -> [a]
orToList s =
  [ x | (x, uuids) <- Map.toList (orEntries s)
      , not (Set.null (uuids `Set.difference` orTombstones s)) ]

orToSet :: Ord a => ORSet a -> Set.Set a
orToSet = Set.fromList . orToList

orMerge :: Ord a => ORSet a -> ORSet a -> ORSet a
orMerge a b = ORSet
  { orEntries    = Map.unionWith Set.union (orEntries a) (orEntries b)
  , orTombstones = orTombstones a `Set.union` orTombstones b
  }

-- | For static initialization: build an ORSet from a plain Set using
-- deterministic UUIDs derived from each element's JSON encoding.
-- Only use this for scenario/test setup; at runtime use orInsert with nextRandom.
orFromSet :: (Ord a, Aeson.ToJSON a) => Set.Set a -> ORSet a
orFromSet = Set.foldr (\x acc -> orInsert (initToken x) x acc) orEmpty

-- | Convenience: single-element ORSet for static initialization.
orSingleton :: (Ord a, Aeson.ToJSON a) => a -> ORSet a
orSingleton x = orInsert (initToken x) x orEmpty

-- | Convenience: list-to-ORSet for static initialization.
orFromList :: (Ord a, Aeson.ToJSON a) => [a] -> ORSet a
orFromList = foldr (\x acc -> orInsert (initToken x) x acc) orEmpty

-- | Insert with a fixed UUID, clearing that UUID from tombstones.
-- Use this with 'initToken' for singleton engine tags (Weather, TimeOfDay, etc.)
-- that are deduplicated on every write. Random-UUID inserts accumulate a new
-- tombstone per cycle; fixed-UUID upserts keep the ORSet bounded at O(distinct values).
orUpsert :: Ord a => UUID -> a -> ORSet a -> ORSet a
orUpsert uuid x s = ORSet
  { orEntries    = Map.insertWith Set.union x (Set.singleton uuid) (orEntries s)
  , orTombstones = Set.delete uuid (orTombstones s)
  }

-- | Deterministic UUID for static init. Not for runtime use.
-- WARNING: the UUID is derived from the value's JSON encoding.
-- Changing a type's ToJSON instance will produce different UUIDs,
-- silently breaking deduplication for existing ORSet entries.
initToken :: Aeson.ToJSON a => a -> UUID
initToken x = UUID.V5.generateNamed UUID.nil (BL.unpack (Aeson.encode x))
