module Engine.Core.Axioms.Diff
  ( diffWorlds
  ) where

import           Data.Maybe          (fromMaybe)
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set

import           Engine.CRDT.ORSet
import           GameTypes

-- | Compute the diff between two world states.
-- Only records what actually changed; unchanged fields produce empty lists.
-- The PlayerId identifies who caused the changes, for CRDT attribution.
diffWorlds :: PlayerId -> GameWorld -> GameWorld -> WorldDiff
diffWorlds pid before after = WorldDiff
  { diffStats            = statDeltas
  , diffRelations        = relationDeltas
  , diffTagsAdded        = charTagsAdded
  , diffTagsRemoved      = charTagsRemoved
  , diffWorldTagsAdded   = Set.toList (orToSet (worldTags after)  Set.\\ orToSet (worldTags before))
  , diffWorldTagsRemoved = Set.toList (orToSet (worldTags before) Set.\\ orToSet (worldTags after))
  , diffLocations        = locationDeltas
  }
  where
    statDeltas =
      let truthBefore = fromMaybe Map.empty (Map.lookup Truth (worldGraph before))
          truthAfter  = fromMaybe Map.empty (Map.lookup Truth (worldGraph after))
          emptyRel    = Relationship Map.empty
          -- Union of all character keys from both worlds — catches new characters
          allCids     = Map.keys (Map.union truthAfter truthBefore)
      in [ StatDelta cid st old new pid
         | cid <- allCids
         , let rel1 = fromMaybe emptyRel (Map.lookup cid truthBefore)
         , let rel2 = fromMaybe emptyRel (Map.lookup cid truthAfter)
         , st   <- [minBound..maxBound]
         , let old = getRelStat st rel1
         , let new = getRelStat st rel2
         , old /= new
         ]

    relationDeltas =
      let allFromKeys = Map.keys (Map.union (worldGraph after) (worldGraph before))
      in [ RelationDelta from to stat old new pid
         | from <- allFromKeys
         , from /= Truth  -- truth edges are captured in statDeltas
         , let edges1 = fromMaybe Map.empty (Map.lookup from (worldGraph before))
         , let edges2 = fromMaybe Map.empty (Map.lookup from (worldGraph after))
         , let allToKeys = Map.keys (Map.union edges2 edges1)
         , let emptyRel = Relationship Map.empty
         , to   <- allToKeys
         , let rel1 = fromMaybe emptyRel (Map.lookup to edges1)
         , let rel2 = fromMaybe emptyRel (Map.lookup to edges2)
         , stat <- [minBound..maxBound]
         , let old = getRelStat stat rel1
         , let new = getRelStat stat rel2
         , old /= new
         ]

    charTagsAdded =
      [ (cid, t)
      | (cid, c2) <- Map.toList (worldCharacters after)
      , let c1 = Map.lookup cid (worldCharacters before)
      , let tagsBefore = maybe Set.empty (orToSet . charTags) c1
      , t <- Set.toList (orToSet (charTags c2) Set.\\ tagsBefore)
      ]

    charTagsRemoved =
      [ (cid, t)
      | (cid, c1) <- Map.toList (worldCharacters before)
      , let c2 = Map.lookup cid (worldCharacters after)
      , let tagsAfter = maybe Set.empty (orToSet . charTags) c2
      , t <- Set.toList (orToSet (charTags c1) Set.\\ tagsAfter)
      ]

    locationDeltas =
      -- Changed locations (existed in both before and after)
      [ LocationDelta cid old new
      | (cid, old) <- Map.toList (worldLocations before)
      , Just new   <- [Map.lookup cid (worldLocations after)]
      , old /= new
      ]
      ++
      -- New locations (character appeared for the first time in after)
      [ LocationDelta cid loc loc
      | (cid, loc) <- Map.toList (worldLocations after)
      , not (Map.member cid (worldLocations before))
      ]
