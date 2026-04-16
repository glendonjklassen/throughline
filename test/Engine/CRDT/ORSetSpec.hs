module Engine.CRDT.ORSetSpec (spec) where

import           Test.Hspec
import qualified Data.Set as Set

import           Data.Aeson         (encode, decode)
import           Data.UUID          (UUID, nil)
import           Data.UUID.V5       (generateNamed)

import           Engine.CRDT.ORSet
import           GameTypes

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- Deterministic UUIDs for test inserts, different from initToken's output.
uuidA, uuidB, uuidC :: UUID
uuidA = generateNamed nil (map (fromIntegral . fromEnum) ("a" :: String))
uuidB = generateNamed nil (map (fromIntegral . fromEnum) ("b" :: String))
uuidC = generateNamed nil (map (fromIntegral . fromEnum) ("c" :: String))

tagX, tagY :: Tag
tagX = ScenarioTag (MkScenarioTag "x")
tagY = ScenarioTag (MkScenarioTag "y")

spec :: Spec
spec = describe "ORSet" $ do

  -- -------------------------------------------------------------------------
  -- Basic operations
  -- -------------------------------------------------------------------------

  describe "orEmpty" $
    it "contains no elements" $
      orToList (orEmpty :: ORSet Tag) `shouldBe` []

  describe "orInsert" $ do
    it "makes an element a member" $
      orMember tagX (orInsert uuidA tagX orEmpty) `shouldBe` True

    it "is idempotent: inserting again with different UUID keeps element live" $
      let s = orInsert uuidB tagX (orInsert uuidA tagX orEmpty)
      in orMember tagX s `shouldBe` True

    it "does not make a different element a member" $
      orMember tagY (orInsert uuidA tagX orEmpty) `shouldBe` False

  describe "orDelete" $ do
    it "removes a live element" $
      let s = orDelete tagX (orInsert uuidA tagX orEmpty)
      in orMember tagX s `shouldBe` False

    it "is a no-op on an absent element" $
      let s = orDelete tagX orEmpty
      in orMember tagX s `shouldBe` False

    it "does not remove a different element" $
      let s = orDelete tagX (orInsert uuidA tagX (orInsert uuidB tagY orEmpty))
      in orMember tagY s `shouldBe` True

  describe "orToSet" $
    it "matches the set of live elements" $
      let s = orInsert uuidA tagX (orInsert uuidB tagY orEmpty)
      in orToSet s `shouldBe` Set.fromList [tagX, tagY]

  -- -------------------------------------------------------------------------
  -- CRDT merge laws
  -- -------------------------------------------------------------------------

  describe "orMerge identity" $ do
    it "merging with orEmpty on the left is identity" $
      let s = orInsert uuidA tagX orEmpty
      in orToSet (orMerge orEmpty s) `shouldBe` orToSet s

    it "merging with orEmpty on the right is identity" $
      let s = orInsert uuidA tagX orEmpty
      in orToSet (orMerge s orEmpty) `shouldBe` orToSet s

  describe "orMerge commutativity" $
    it "merge(A, B) and merge(B, A) have the same logical contents" $
      let a = orInsert uuidA tagX orEmpty
          b = orInsert uuidB tagY orEmpty
      in orToSet (orMerge a b) `shouldBe` orToSet (orMerge b a)

  describe "orMerge associativity" $
    it "merge(merge(A,B),C) = merge(A,merge(B,C)) logically" $
      let a = orInsert uuidA tagX orEmpty
          b = orInsert uuidB tagY orEmpty
          c = orInsert uuidC (ScenarioTag (MkScenarioTag "z")) orEmpty
      in orToSet (orMerge (orMerge a b) c)
           `shouldBe` orToSet (orMerge a (orMerge b c))

  describe "orMerge idempotency" $
    it "merge(A, A) = A logically" $
      let s = orInsert uuidA tagX orEmpty
      in orToSet (orMerge s s) `shouldBe` orToSet s

  -- -------------------------------------------------------------------------
  -- Add-wins semantics
  -- -------------------------------------------------------------------------

  describe "add-wins concurrent add+remove" $ do
    it "concurrent add wins over remove: element is present after merge" $
      -- Site A: add tagX with uuidA
      -- Site B: add tagX with uuidA (same initial state), then remove tagX
      -- Site C: add tagX with fresh uuidC (concurrent re-add)
      -- After merge: uuidA is tombstoned, but uuidC is live → tagX present
      let siteA  = orInsert uuidA tagX orEmpty
          siteB  = orDelete tagX siteA           -- tombstones uuidA
          siteC  = orInsert uuidC tagX siteA     -- fresh add after fork
          merged = orMerge siteB siteC
      in orMember tagX merged `shouldBe` True

    it "remove without concurrent re-add wins: element is absent after merge" $
      -- Both sides share the same initial add (uuidA). One side removes it.
      -- No re-add. After merge: uuidA is tombstoned → tagX absent.
      let base   = orInsert uuidA tagX orEmpty
          siteA  = base
          siteB  = orDelete tagX base
          merged = orMerge siteA siteB
      in orMember tagX merged `shouldBe` False

  -- -------------------------------------------------------------------------
  -- orDeleteWhere
  -- -------------------------------------------------------------------------

  describe "orDeleteWhere" $ do
    it "removes all elements matching a predicate" $
      let s = orInsert uuidA (weatherTag (WeatherDesc "sun"))
            . orInsert uuidB (weatherTag (WeatherDesc "rain"))
            . orInsert uuidC tagX
            $ orEmpty
          s' = orDeleteWhere isWeather s
      in ( orMember (weatherTag (WeatherDesc "sun"))  s'
         , orMember (weatherTag (WeatherDesc "rain")) s'
         , orMember tagX                s'
         ) `shouldBe` (False, False, True)

    it "is a no-op when predicate matches nothing" $
      let s = orInsert uuidA tagX orEmpty
      in orToSet (orDeleteWhere isWeather s) `shouldBe` orToSet s

  -- -------------------------------------------------------------------------
  -- JSON round-trip
  -- -------------------------------------------------------------------------

  describe "JSON round-trip" $ do
    it "preserves tombstone: removed element stays absent after decode" $
      let s       = orDelete tagX (orInsert uuidA tagX orEmpty)
          decoded = decode (encode s) :: Maybe (ORSet Tag)
      in fmap (orMember tagX) decoded `shouldBe` Just False

    it "add-wins survives round-trip: concurrent re-add is live after decode" $
      -- uuidA is tombstoned; uuidC is a concurrent re-add with a fresh UUID
      let s       = orInsert uuidC tagX (orDelete tagX (orInsert uuidA tagX orEmpty))
          decoded = decode (encode s) :: Maybe (ORSet Tag)
      in fmap (orMember tagX) decoded `shouldBe` Just True

    it "exact CRDT structure is preserved: entries and tombstones match" $
      let s       = orInsert uuidC tagX (orDelete tagX (orInsert uuidA tagX orEmpty))
          decoded = decode (encode s) :: Maybe (ORSet Tag)
      in ( fmap orEntries decoded, fmap orTombstones decoded )
           `shouldBe` ( Just (orEntries s), Just (orTombstones s) )

  -- -------------------------------------------------------------------------
  -- Static initializers
  -- -------------------------------------------------------------------------

  describe "orSingleton" $ do
    it "contains exactly the one element" $
      orToSet (orSingleton tagX) `shouldBe` Set.singleton tagX
    it "the element is a member" $
      orMember tagX (orSingleton tagX) `shouldBe` True

  describe "orFromList" $ do
    it "contains all elements from the list" $
      orToSet (orFromList [tagX, tagY]) `shouldBe` Set.fromList [tagX, tagY]
    it "deduplicates: same element twice is stored once" $
      orToSet (orFromList [tagX, tagX]) `shouldBe` Set.singleton tagX
    it "empty list gives empty set" $
      orToSet (orFromList ([] :: [Tag])) `shouldBe` Set.empty

  describe "orFromSet" $ do
    it "round-trips a Set through ORSet" $
      let s = Set.fromList [tagX, tagY]
      in orToSet (orFromSet s) `shouldBe` s
    it "empty set gives empty ORSet" $
      orToSet (orFromSet (Set.empty :: Set.Set Tag)) `shouldBe` Set.empty
