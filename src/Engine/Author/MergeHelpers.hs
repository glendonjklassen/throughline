module Engine.Author.MergeHelpers where

import GameTypes

-- ---------------------------------------------------------------------------
-- Merge provenance helpers
-- ---------------------------------------------------------------------------

-- | Filter a MergeDiff to only changes from unaware sources.
unawareChanges :: MergeDiff -> MergeDiff
unawareChanges md = MergeDiff
  { mergeStats     = filter isUnaware (mergeStats md)
  , mergeRelations = filter isUnaware (mergeRelations md)
  , mergeTags      = filter isUnaware (mergeTags md)
  , mergeWorldTags = filter isUnaware (mergeWorldTags md)
  , mergeLocations = filter isUnaware (mergeLocations md)
  }
  where isUnaware d = mdProvenance d == Unaware

-- | Fire effects only when unaware state arrived in this merge.
whenUnaware :: MergeDiff -> [Effect] -> [Effect]
whenUnaware md effs
  | hasAnyUnaware md = effs
  | otherwise        = []

-- | Does the MergeDiff contain any unaware changes?
hasAnyUnaware :: MergeDiff -> Bool
hasAnyUnaware md =
  any isU (mergeStats md) || any isU (mergeRelations md)
  || any isU (mergeTags md) || any isU (mergeWorldTags md)
  || any isU (mergeLocations md)
  where isU d = mdProvenance d == Unaware

-- | Did any relation change for a specific character pair arrive unaware?
hasUnawareRelation :: CharacterId -> CharacterId -> StatType -> MergeDiff -> Bool
hasUnawareRelation from to stat md = any matches (mergeRelations md)
  where
    matches d = mdProvenance d == Unaware
             && relationDeltaFrom (mdValue d) == from
             && relationDeltaTo   (mdValue d) == to
             && relationDeltaStat (mdValue d) == stat

-- | Did any character arrive at a location from an unaware source?
hasUnawareArrival :: CharacterId -> Location -> MergeDiff -> Bool
hasUnawareArrival cid loc md = any matches (mergeLocations md)
  where
    matches d = mdProvenance d == Unaware
             && locationDeltaChar (mdValue d) == cid
             && locationDeltaTo   (mdValue d) == loc
