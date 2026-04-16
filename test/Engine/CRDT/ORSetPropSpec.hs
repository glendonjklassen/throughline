module Engine.CRDT.ORSetPropSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck
import qualified Data.Set as Set
import           Data.UUID (UUID)
import qualified Data.UUID.V5 as UUID.V5
import qualified Data.UUID as UUID

import           Engine.CRDT.ORSet
import           GameTypes (Tag (..), ScenarioTagValue(..))

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- Deterministic UUID from an Int — avoids IO while still giving distinct values.
uuidFrom :: Int -> UUID
uuidFrom n = UUID.V5.generateNamed UUID.nil (map (fromIntegral . fromEnum) (show n))

-- Logical contents as a Set for easy comparison.
contents :: ORSet Tag -> Set.Set Tag
contents = orToSet

-- ---------------------------------------------------------------------------
-- Arbitrary instances
-- ---------------------------------------------------------------------------

-- Small tag vocabulary so merges are interesting (overlapping elements).
newtype ArbTag = ArbTag Tag deriving (Show)

instance Arbitrary ArbTag where
  arbitrary = ArbTag . ScenarioTag . MkScenarioTag <$> elements
    ["alpha", "beta", "gamma", "delta", "epsilon"]

-- An ORSet built from a list of labelled inserts followed by some deletes.
-- Each insert gets a deterministic UUID derived from its list index.
newtype ArbORSet = ArbORSet (ORSet Tag) deriving (Show)

instance Arbitrary ArbORSet where
  arbitrary = do
    inserts <- listOf (arbitrary :: Gen ArbTag)
    deletes <- sublistOf inserts
    let s0 = foldr (\(i, ArbTag t) acc -> orInsert (uuidFrom i) t acc)
                   orEmpty
                   (zip [0..] inserts)
        s1 = foldr (\(ArbTag t) acc -> orDelete t acc) s0 deletes
    pure (ArbORSet s1)

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "ORSet properties" $ do

  -- -------------------------------------------------------------------------
  -- CRDT merge laws
  -- -------------------------------------------------------------------------

  it "merge is commutative" $ property $
    \(ArbORSet a) (ArbORSet b) ->
      contents (orMerge a b) == contents (orMerge b a)

  it "merge is associative" $ property $
    \(ArbORSet a) (ArbORSet b) (ArbORSet c) ->
      contents (orMerge (orMerge a b) c) == contents (orMerge a (orMerge b c))

  it "merge is idempotent" $ property $
    \(ArbORSet a) ->
      contents (orMerge a a) == contents a

  it "merge with orEmpty is identity" $ property $
    \(ArbORSet a) ->
      contents (orMerge a orEmpty) == contents a
        && contents (orMerge orEmpty a) == contents a

  -- -------------------------------------------------------------------------
  -- Add-wins semantics
  -- -------------------------------------------------------------------------

  -- If site A inserts an element with a *fresh* UUID (not known to site B),
  -- then merging with site B (which may have deleted that element under old
  -- UUIDs) must keep the element live — the new UUID was never tombstoned.
  it "add-wins: fresh insert survives merge with a deleting peer" $ property $
    \(ArbORSet base) (ArbTag t) (NonNegative freshIdx) ->
      let freshUUID = uuidFrom (1000 + freshIdx)   -- outside the [0..] range used in ArbORSet
          siteA     = orInsert freshUUID t base
          siteB     = orDelete t base               -- tombstones only UUIDs base already knows
          merged    = orMerge siteA siteB
      in orMember t merged

  -- -------------------------------------------------------------------------
  -- Membership round-trips through insert/delete
  -- -------------------------------------------------------------------------

  it "inserted element is a member" $ property $
    \(ArbORSet s) (ArbTag t) (NonNegative i) ->
      let uuid = uuidFrom (2000 + i)
      in orMember t (orInsert uuid t s)

  it "element deleted from a singleton is not a member" $ property $
    \(ArbTag t) (NonNegative i) ->
      let s = orInsert (uuidFrom (3000 + i)) t orEmpty
      in not (orMember t (orDelete t s))

  -- -------------------------------------------------------------------------
  -- orToSet / orToList consistency
  -- -------------------------------------------------------------------------

  it "orToSet and orToList agree on live elements" $ property $
    \(ArbORSet s) ->
      Set.fromList (orToList s) == contents s
