module Engine.CRDT.PNCounterSpec (spec) where

import           Test.Hspec

import           Engine.CRDT.PNCounter
import           GameTypes (PlayerId (..))

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

pA, pB :: PlayerId
pA = PlayerId "player-a"
pB = PlayerId "player-b"

-- Counter with baseline 5, no deltas.
base5 :: PNCounter PlayerId
base5 = pnZero 5

spec :: Spec
spec = describe "PNCounter" $ do

  -- -------------------------------------------------------------------------
  -- pnValue
  -- -------------------------------------------------------------------------

  describe "pnValue" $ do
    it "returns baseline when no deltas applied" $
      pnValue base5 `shouldBe` 5

    it "reflects a positive delta" $
      pnValue (pnModify pA 3 base5) `shouldBe` 8

    it "reflects a negative delta" $
      pnValue (pnModify pA (-2) base5) `shouldBe` 3

    it "accumulates multiple deltas from the same player" $
      pnValue (pnModify pA (-1) (pnModify pA (-1) base5)) `shouldBe` 3

    it "accumulates deltas from different players" $
      pnValue (pnModify pB 1 (pnModify pA (-3) base5)) `shouldBe` 3

  -- -------------------------------------------------------------------------
  -- pnModify
  -- -------------------------------------------------------------------------

  describe "pnModify" $ do
    it "zero delta is a no-op" $
      pnModify pA 0 base5 `shouldBe` base5

    it "positive delta is readable via pnValue" $
      pnValue (pnModify pA 2 (pnZero 0)) `shouldBe` 2

    it "negative delta is readable via pnValue" $
      pnValue (pnModify pA (-2) (pnZero 10)) `shouldBe` 8

  -- -------------------------------------------------------------------------
  -- pnMerge — CRDT laws
  -- -------------------------------------------------------------------------

  describe "pnMerge identity" $ do
    let c = pnModify pA 3 base5

    it "merge with zero on the left is identity" $
      pnValue (pnMerge (pnZero 5) c) `shouldBe` pnValue c

    it "merge with zero on the right is identity" $
      pnValue (pnMerge c (pnZero 5)) `shouldBe` pnValue c

  describe "pnMerge commutativity" $
    it "merge(A,B) and merge(B,A) produce the same value" $
      let a = pnModify pA   2  base5
          b = pnModify pB (-1) base5
      in pnValue (pnMerge a b) `shouldBe` pnValue (pnMerge b a)

  describe "pnMerge associativity" $
    it "merge(merge(A,B),C) and merge(A,merge(B,C)) produce the same value" $
      let a = pnModify pA   2  base5
          b = pnModify pB (-1) base5
          c = pnModify pA   1  base5
      in pnValue (pnMerge (pnMerge a b) c)
           `shouldBe` pnValue (pnMerge a (pnMerge b c))

  describe "pnMerge idempotency" $
    it "merge(A, A) = A" $
      let a = pnModify pA 3 base5
      in pnValue (pnMerge a a) `shouldBe` pnValue a

  -- -------------------------------------------------------------------------
  -- Concurrent update semantics
  -- -------------------------------------------------------------------------

  describe "concurrent updates" $ do
    it "player A increments and player B decrements: both changes reflected" $
      -- A starts from base5, increments by 2
      -- B starts from base5, decrements by 1
      -- Merged: base(5) + A's +2 + B's -1 = 6
      let a = pnModify pA   2  base5
          b = pnModify pB (-1) base5
      in pnValue (pnMerge a b) `shouldBe` 6

    it "two concurrent increments from different players both take effect" $
      let a = pnModify pA 3 base5
          b = pnModify pB 4 base5
      in pnValue (pnMerge a b) `shouldBe` 12

    it "baseline is not doubled by a full-state merge" $
      -- Both sides have the same base; merging should not double it
      pnValue (pnMerge base5 base5) `shouldBe` 5
