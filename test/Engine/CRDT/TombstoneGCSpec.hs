-- | Tests for Engine.CRDT.TombstoneGC and the orDeleteAt / orGCTombstones
-- primitives that back it.
module Engine.CRDT.TombstoneGCSpec (spec) where

import           Data.Time.Calendar       (fromGregorian)
import           Data.Time.Clock          (UTCTime(..), addUTCTime,
                                           secondsToNominalDiffTime)
import qualified Data.UUID                as UUID
import qualified Data.Set                 as Set
import qualified Data.Map.Strict          as Map
import           Test.Hspec

import           Engine.CRDT.ORSet
import           Engine.CRDT.TombstoneGC

-- | A fixed clock so tests don't depend on wall time.
now :: UTCTime
now = UTCTime (fromGregorian 2026 4 23) 0

-- | @daysBefore n@ = @n@ whole days earlier than 'now'.
daysBefore :: Integer -> UTCTime
daysBefore d = addUTCTime (secondsToNominalDiffTime (fromInteger (negate d) * 86400)) now

uuid1, uuid2 :: UUID.UUID
uuid1 = UUID.fromWords 1 0 0 0
uuid2 = UUID.fromWords 2 0 0 0

-- | ORSet with two entries, one deleted long ago, one deleted recently.
sample :: ORSet String
sample =
  let s0 = orInsert uuid1 ("old" :: String) (orInsert uuid2 "new" orEmpty)
      s1 = orDeleteAt (daysBefore 400) "old" s0
      s2 = orDeleteAt (daysBefore 30)  "new" s1
  in s2

spec :: Spec
spec = describe "Engine.CRDT.TombstoneGC" $ do

  describe "orDeleteAt" $ do
    it "records an age for each tombstoned UUID" $ do
      let s0 = orInsert uuid1 ("old" :: String) orEmpty
          s  = orDeleteAt (daysBefore 1) "old" s0
      Map.size (orTombstoneAges s) `shouldBe` 1

    it "keeps the earliest age when re-deleting the same element" $ do
      let s0 = orInsert uuid1 ("x" :: String) orEmpty
          s1 = orDeleteAt (daysBefore 10) "x" s0
          s2 = orDeleteAt (daysBefore 1)  "x" s1
      Map.lookup uuid1 (orTombstoneAges s2) `shouldBe` Just (daysBefore 10)

  describe "orGCTombstones" $ do

    it "drops tombstones whose age matches the predicate" $ do
      let dropOld minted = minted < daysBefore 365
          gced = orGCTombstones dropOld sample
      -- The 400-day-old tombstone should go; the 30-day-old stays.
      Set.size (orTombstones gced) `shouldBe` 1
      Map.size (orTombstoneAges gced) `shouldBe` 1

    it "leaves tombstones without recorded ages alone" $ do
      let s0 = orInsert uuid1 ("legacy" :: String) orEmpty
          s1 = orDelete "legacy" s0           -- untimestamped
          gced = orGCTombstones (const True) s1
      Set.size (orTombstones gced) `shouldBe` 1

  describe "olderThan / olderThanDays" $ do

    it "olderThanDays 365 keeps a 100-day tombstone" $ do
      let rule = olderThanDays 365
      rule now (daysBefore 100) `shouldBe` False

    it "olderThanDays 365 drops a 400-day tombstone" $ do
      let rule = olderThanDays 365
      rule now (daysBefore 400) `shouldBe` True

  describe "gcORSetWith" $ do

    it "threads the rule through the ORSet" $ do
      let rule  = olderThanDays 365
          gced  = gcORSetWith rule now sample
      Set.size (orTombstones gced) `shouldBe` 1
