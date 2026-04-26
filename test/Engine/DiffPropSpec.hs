{-# LANGUAGE ScopedTypeVariables #-}
module Engine.DiffPropSpec (spec) where

import           Data.List       (nub)
import           Test.Hspec
import           Test.QuickCheck
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Data.UUID       (UUID)

import           Engine.Core.Axioms    (diffWorlds)
import           Engine.Core.Effects  (applyWorldDiff, mergeActiveEffects, mergeWorlds)
import           Engine.Sync.EventLog (mergeLogs)
import           Engine.CRDT.ORSet    (ORSet, orFromList, orToSet)
import           GameTypes
import           TestFixtures

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

lamportKey :: LogEntry -> (Int, String)
lamportKey e = let LamportClock t (PlayerId p) = entryClock e in (t, p)

isSortedBy :: Ord b => (a -> b) -> [a] -> Bool
isSortedBy f xs = let ys = map f xs in and (zipWith (<=) ys (drop 1 ys))

liveIds :: [LiveEffect] -> Set.Set UUID
liveIds = Set.fromList . map liveId

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "Diff and merge properties" $ do

  -- -------------------------------------------------------------------------
  -- diffWorlds self-diff
  -- -------------------------------------------------------------------------

  describe "diffWorlds self-diff" $ do

    it "world tags: no additions or removals" $ property $
      \(tags :: ORSet Tag) ->
        let w    = emptyWorld { worldTags = tags }
            diff = diffWorlds (PlayerId "p") w w
        in null (diffWorldTagsAdded diff) && null (diffWorldTagsRemoved diff)

    it "character tags: no additions or removals" $ property $
      \(ct1 :: ORSet Tag) (ct2 :: ORSet Tag) ->
        let w = emptyWorld { worldCharacters = Map.fromList
                  [ (player, Character player "P" [] ct1)
                  , (npc,    Character npc    "N" [] ct2)
                  ] }
            diff = diffWorlds (PlayerId "p") w w
        in null (diffTagsAdded diff) && null (diffTagsRemoved diff)

    it "locations: no deltas" $ property $
      \(pairs :: [(CharacterId, Small Int)]) ->
        let locs = Map.fromList [ (c, Location (show n)) | (c, Small n) <- take 4 pairs ]
            w    = emptyWorld { worldLocations = locs }
            diff = diffWorlds (PlayerId "p") w w
        in null (diffLocations diff)

    it "stats: no deltas" $ property $
      \(tags :: ORSet Tag) ->
        -- worldGraph is empty in emptyWorld; self-diff should produce no stat deltas
        let w = emptyWorld { worldTags = tags }
        in null (diffStats (diffWorlds (PlayerId "p") w w))

  -- -------------------------------------------------------------------------
  -- mergeLogs: Lamport ordering
  -- -------------------------------------------------------------------------

  describe "LamportClock Ord" $ do

    it "higher tick sorts after lower tick regardless of PlayerId" $ property $
      \(n :: Positive Int) ->
        LamportClock (getPositive n + 1) (PlayerId "aaa") > LamportClock (getPositive n) (PlayerId "zzz")

    it "at equal ticks, sorts by PlayerId as tie-breaker" $ property $
      \(n :: Positive Int) ->
        LamportClock (getPositive n) (PlayerId "aaa") < LamportClock (getPositive n) (PlayerId "zzz")

  describe "mergeLogs" $ do

    it "divergent tail is sorted by Lamport clock" $ property $
      \(logA :: [LogEntry]) (logB :: [LogEntry]) ->
        let (_, merged) = mergeLogs logA logB
        in isSortedBy lamportKey merged

    it "common prefix length does not exceed either log" $ property $
      \(logA :: [LogEntry]) (logB :: [LogEntry]) ->
        let (commonLen, _) = mergeLogs logA logB
        in commonLen <= length logA && commonLen <= length logB

    it "merged length equals sum of divergent portions" $ property $
      \(logA :: [LogEntry]) (logB :: [LogEntry]) ->
        let (commonLen, merged) = mergeLogs logA logB
        in length merged == (length logA - commonLen) + (length logB - commonLen)

  -- -------------------------------------------------------------------------
  -- mergeActiveEffects: OR-Set laws
  -- -------------------------------------------------------------------------

  describe "mergeActiveEffects" $ do

    it "is commutative (same set of IDs)" $ property $
      \(fxsA :: [LiveEffect]) (fxsB :: [LiveEffect]) ->
        liveIds (mergeActiveEffects fxsA fxsB)
          == liveIds (mergeActiveEffects fxsB fxsA)

    it "is idempotent" $ property $
      \(fxs :: [LiveEffect]) ->
        liveIds (mergeActiveEffects fxs fxs) == liveIds fxs

    it "is identity with empty on the right" $ property $
      \(fxs :: [LiveEffect]) ->
        liveIds (mergeActiveEffects fxs []) == liveIds fxs

    it "is identity with empty on the left" $ property $
      \(fxs :: [LiveEffect]) ->
        liveIds (mergeActiveEffects [] fxs) == liveIds fxs

    it "result contains all IDs from both sides" $ property $
      \(fxsA :: [LiveEffect]) (fxsB :: [LiveEffect]) ->
        let merged = liveIds (mergeActiveEffects fxsA fxsB)
        in liveIds fxsA `Set.isSubsetOf` merged
          && liveIds fxsB `Set.isSubsetOf` merged

  -- -------------------------------------------------------------------------
  -- diffWorlds structural invariants
  -- -------------------------------------------------------------------------

  describe "diffWorlds invariants" $ do

    it "world-tags added and removed are disjoint" $ property $
      \(s1 :: [Positive Int]) (s2 :: [Positive Int]) ->
        let toTags ns = orFromList (map (ScenarioTag . MkScenarioTag . show . getPositive) ns)
            w1   = emptyWorld { worldTags = toTags s1 }
            w2   = emptyWorld { worldTags = toTags s2 }
            diff = diffWorlds (PlayerId "p") w1 w2
            added   = Set.fromList (diffWorldTagsAdded diff)
            removed = Set.fromList (diffWorldTagsRemoved diff)
        in Set.disjoint added removed

  -- -------------------------------------------------------------------------
  -- mergeWorlds CRDT properties
  -- -------------------------------------------------------------------------

  describe "mergeWorlds" $ do

    it "worldTags merge is commutative" $ property $
      \(s1 :: [Positive Int]) (s2 :: [Positive Int]) ->
        let toTags ns = orFromList (map (ScenarioTag . MkScenarioTag . show . getPositive) ns)
            a = emptyWorld { worldTags = toTags s1 }
            b = emptyWorld { worldTags = toTags s2 }
        in orToSet (worldTags (mergeWorlds a b))
             == orToSet (worldTags (mergeWorlds b a))

    it "worldTags merge is idempotent" $ property $
      \(s :: [Positive Int]) ->
        let toTags ns = orFromList (map (ScenarioTag . MkScenarioTag . show . getPositive) ns)
            w = emptyWorld { worldTags = toTags s }
        in orToSet (worldTags (mergeWorlds w w)) == orToSet (worldTags w)

    it "clock is max of both sides" $ property $
      \(clkA :: LamportClock) (clkB :: LamportClock) ->
        let a = emptyWorld { worldClock = clkA }
            b = emptyWorld { worldClock = clkB }
        in worldClock (mergeWorlds a b) == max clkA clkB

  -- -------------------------------------------------------------------------
  -- applyWorldDiff roundtrip
  -- -------------------------------------------------------------------------

  describe "applyWorldDiff roundtrip" $ do

    it "roundtrips world tag membership" $ property $
      \(s1 :: [Positive Int]) (s2 :: [Positive Int]) ->
        ioProperty $ do
          let toTags ns = orFromList (map (ScenarioTag . MkScenarioTag . show . getPositive) ns)
              w1   = emptyWorld { worldTags = toTags s1 }
              w2   = emptyWorld { worldTags = toTags s2 }
              diff = diffWorlds (PlayerId "p") w1 w2
          (_, w1') <- runApp' w1 (applyWorldDiff diff)
          pure (orToSet (worldTags w1') === orToSet (worldTags w2))

    it "roundtrips locations" $ property $
      \(chars :: [CharacterId]) ->
        ioProperty $ do
          let uniq  = nub chars
              locs1 = Map.fromList [(c, Location "before") | c <- uniq]
              locs2 = Map.fromList [(c, Location "after")  | c <- uniq]
              w1   = emptyWorld { worldLocations = locs1 }
              w2   = emptyWorld { worldLocations = locs2 }
              diff = diffWorlds (PlayerId "p") w1 w2
          (_, w1') <- runApp' w1 (applyWorldDiff diff)
          pure (worldLocations w1' === worldLocations w2)
