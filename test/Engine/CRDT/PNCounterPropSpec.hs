module Engine.CRDT.PNCounterPropSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck
import qualified Data.Map.Strict as Map

import           Engine.CRDT.PNCounter
import           GameTypes (PlayerId (..))

-- ---------------------------------------------------------------------------
-- Arbitrary instances
-- ---------------------------------------------------------------------------

-- Small player vocabulary so merges produce interesting overlaps.
newtype ArbPlayerId = ArbPlayerId PlayerId deriving (Show)

instance Arbitrary ArbPlayerId where
  arbitrary = ArbPlayerId . PlayerId <$> elements ["p1", "p2", "p3"]

-- A counter is a baseline plus a sequence of (player, delta) modifications.
newtype ArbCounter = ArbCounter (PNCounter PlayerId) deriving (Show)

instance Arbitrary ArbCounter where
  arbitrary = do
    base    <- choose (-5, 10)
    mods    <- listOf $ (,) . getPlayerId <$> arbitrary <*> choose (-5, 5)
    pure $ ArbCounter $ foldr (\(pid, d) acc -> pnModify pid d acc) (pnZero base) mods
    where
      getPlayerId (ArbPlayerId p) = p

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "PNCounter properties" $ do

  -- -------------------------------------------------------------------------
  -- CRDT merge laws
  -- -------------------------------------------------------------------------

  it "merge is commutative" $ property $
    \(ArbCounter a) (ArbCounter b) ->
      -- Both sides must share the same baseline for this to hold.
      -- Fix b's base to a's so the law is meaningful.
      let b' = b { pnBase = pnBase a }
      in pnValue (pnMerge a b') == pnValue (pnMerge b' a)

  it "merge is associative" $ property $
    \(ArbCounter a) (ArbCounter b) (ArbCounter c) ->
      let base = pnBase a
          b'   = b { pnBase = base }
          c'   = c { pnBase = base }
      in pnValue (pnMerge (pnMerge a b') c') == pnValue (pnMerge a (pnMerge b' c'))

  it "merge is idempotent" $ property $
    \(ArbCounter a) ->
      pnValue (pnMerge a a) == pnValue a

  it "merge with pnZero is identity" $ property $
    \(ArbCounter a) ->
      pnValue (pnMerge a (pnZero (pnBase a))) == pnValue a
        && pnValue (pnMerge (pnZero (pnBase a)) a) == pnValue a

  -- -------------------------------------------------------------------------
  -- pnModify / pnValue round-trip
  -- -------------------------------------------------------------------------

  it "positive delta increases value" $ property $
    \(ArbCounter c) (ArbPlayerId pid) (Positive d) ->
      pnValue (pnModify pid d c) == pnValue c + d

  it "negative delta decreases value" $ property $
    \(ArbCounter c) (ArbPlayerId pid) (Positive d) ->
      pnValue (pnModify pid (negate d) c) == pnValue c - d

  it "zero delta is a no-op" $ property $
    \(ArbCounter c) (ArbPlayerId pid) ->
      pnModify pid 0 c == c

  -- -------------------------------------------------------------------------
  -- Baseline semantics
  -- -------------------------------------------------------------------------

  it "baseline is not doubled by merging a counter with itself" $ property $
    \(ArbCounter c) ->
      pnBase (pnMerge c c) == pnBase c

  it "baseline is preserved from the left side on merge" $ property $
    \(ArbCounter a) (ArbCounter b) ->
      pnBase (pnMerge a b) == pnBase a

  -- -------------------------------------------------------------------------
  -- Concurrent update semantics
  -- -------------------------------------------------------------------------

  -- Two independent players each apply a delta from the same baseline.
  -- The merged value must reflect both deltas exactly once.
  it "concurrent deltas from distinct players both take effect" $ property $
    \(Positive baseVal) (Positive da) (Positive db) ->
      let base   = pnZero baseVal
          siteA  = pnModify (PlayerId "a") da   base
          siteB  = pnModify (PlayerId "b") (-db) base
          merged = pnMerge siteA siteB
      in pnValue merged == baseVal + da - db

  -- Applying the same delta twice from the same player (re-merge after resync)
  -- should not double-count: merge takes the max per bucket.
  it "re-merging a player's own state does not double-count their delta" $ property $
    \(ArbCounter base) (ArbPlayerId pid) (Positive d) ->
      let modified = pnModify pid d base
          merged   = pnMerge modified modified
      in pnValue merged == pnValue modified

  -- -------------------------------------------------------------------------
  -- Per-player bucket structure
  -- -------------------------------------------------------------------------

  it "positive delta appears in pnP bucket for that player" $ property $
    \(ArbPlayerId pid) (Positive d) ->
      let c = pnModify pid d (pnZero 0)
      in Map.lookup pid (pnP c) == Just d

  it "negative delta appears in pnN bucket for that player (stored positive)" $ property $
    \(ArbPlayerId pid) (Positive d) ->
      let c = pnModify pid (negate d) (pnZero 0)
      in Map.lookup pid (pnN c) == Just d
