-- | Causal frontier tracking and provenance computation for cross-player merges.
module Engine.Sync.Causality
  ( computeProvenance
  , buildMergeDiff
  , emptyMergeDiff
  , runMergeAxioms
  , runMergeRules
  ) where

import           Data.List          (sortBy)
import qualified Data.Map.Strict    as Map
import           Data.Ord           (comparing)

import           Engine.Core.Axioms    (diffWorlds, evaluateMergeRule)
import           GameTypes

-- | Determine whether a foreign player knew about our state when they acted.
-- Compares their causal frontier against our log to see if they'd seen us.
computeProvenance :: [LogEntry]      -- ^ our log (chronological)
                  -> CausalFrontier  -- ^ their frontier at the time of their entry
                  -> PlayerId        -- ^ us
                  -> Provenance
computeProvenance ourLog theirFrontier us =
  case Map.lookup us theirFrontier of
    Nothing  -> Unaware       -- they never synced with us
    Just eid ->
      -- Compare by Lamport clock, not string ID. String comparison of entryIds
      -- breaks after tick 9 because "10-..." < "9-..." lexicographically.
      let ourLatestClock = case ourLog of
            [] -> LamportClock 0 (PlayerId "")
            _  -> entryClock (last ourLog)
          -- Find the clock of the entry they claim to have seen.
          theirSeenClock = case filter (\e -> entryId e == eid) ourLog of
            (e:_) -> entryClock e
            []    -> LamportClock 0 (PlayerId "")  -- unknown entry: treat as stale
      in if theirSeenClock >= ourLatestClock
           then Aware         -- they saw our latest (or later)
           else Stale         -- they synced with us before, but their knowledge is outdated

-- | Build a MergeDiff by comparing world state before and after a merge,
-- annotated with provenance from the foreign log's entries.
buildMergeDiff :: PlayerId       -- ^ our PlayerId
               -> [LogEntry]     -- ^ our log (for provenance comparison)
               -> [LogEntry]     -- ^ their divergent entries being merged
               -> GameWorld      -- ^ world before merge
               -> GameWorld      -- ^ world after merge
               -> MergeDiff
buildMergeDiff us ourLog theirEntries worldBefore worldAfter =
  let -- Use the last foreign entry's frontier for provenance.
      -- If there are multiple foreign entries, the last one has the most
      -- up-to-date frontier.
      foreignProv = case theirEntries of
        [] -> Unaware
        _  -> computeProvenance ourLog
                                (entryFrontier (last theirEntries))
                                us
      -- Use the foreign player's ID as origin. All entries should share
      -- the same PlayerId (they're from one foreign log).
      origin = case theirEntries of
        []    -> PlayerId "unknown"
        (e:_) -> entryPlayerId e
      -- Compute the raw diff
      rawDiff = diffWorlds origin worldBefore worldAfter
      -- Annotate each delta with provenance
      annotate x = MergeDelta { mdValue = x, mdOrigin = origin, mdProvenance = foreignProv }
  in MergeDiff
    { mergeStats     = map annotate (diffStats rawDiff)
    , mergeRelations = map annotate (diffRelations rawDiff)
    , mergeTags      = map annotate (diffTagsAdded rawDiff)
    , mergeWorldTags = map annotate (diffWorldTagsAdded rawDiff)
    , mergeLocations = map annotate (diffLocations rawDiff)
    }

-- | Empty MergeDiff (no changes).
emptyMergeDiff :: MergeDiff
emptyMergeDiff = MergeDiff [] [] [] [] []

-- | Run merge axioms against a merged world and MergeDiff.
-- Returns effects in axiom priority order (lowest first).
runMergeAxioms :: [MergeAxiom] -> GameWorld -> MergeDiff -> [Effect]
runMergeAxioms axioms world md =
  concatMap (\a -> mergeAxiomEvaluate a world md) sorted
  where sorted = map snd $ sortBy (comparing fst)
                   [(mergeAxiomPriority a, a) | a <- axioms]

-- | Run declarative merge rules against a merged world and MergeDiff.
-- Returns effects in rule priority order (lowest first).
runMergeRules :: [MergeAxiomRule] -> GameWorld -> MergeDiff -> [Effect]
runMergeRules rules world md =
  concatMap (evaluateMergeRule world md) sorted
  where sorted = sortBy (comparing mergeRulePriority) rules
