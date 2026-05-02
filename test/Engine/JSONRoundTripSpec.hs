{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Engine.JSONRoundTripSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck
import           Data.Aeson         (encode, decode, object, (.=), ToJSON, FromJSON)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Word          (Word8)

import           Engine.CRDT.ORSet
import           Engine.CRDT.PNCounter
import           GameTypes
import           TestFixtures   (emptyWorld, twoCharWorld)

-- ---------------------------------------------------------------------------
-- Helper
-- ---------------------------------------------------------------------------

roundTrip :: (ToJSON a, FromJSON a, Eq a) => a -> Bool
roundTrip x = decode (encode x) == Just x

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "JSON round-trips" $ do

  -- -------------------------------------------------------------------------
  -- Primitive types (derived instances — sanity check)
  -- -------------------------------------------------------------------------

  it "CharacterId round-trips" $ property $
    \(c :: CharacterId) -> roundTrip c

  it "Tag round-trips" $ property $
    \(t :: Tag) -> roundTrip t

  it "StatType round-trips" $ property $
    \(s :: StatType) -> roundTrip s

  it "LamportClock round-trips" $ property $
    \(c :: LamportClock) -> roundTrip c

  -- -------------------------------------------------------------------------
  -- CharacterId / StatType as map keys
  -- -------------------------------------------------------------------------

  -- These use ToJSONKey / FromJSONKey (Show/Read-based), not the generic path.
  it "CharacterId survives as a JSON object key" $ property $
    \(c :: CharacterId) (t :: Tag) ->
      let m = encode [(c, t)]
      in decode m == Just [(c, t)]

  it "StatType survives as a JSON object key" $ property $
    \(s :: StatType) (v :: Int) ->
      let m = encode [(s, v)]
      in decode m == Just [(s, v)]

  -- -------------------------------------------------------------------------
  -- WorldDiff — custom instance that omits empty arrays
  -- -------------------------------------------------------------------------

  it "WorldDiff round-trips (arbitrary fields)" $ property $
    \(d :: WorldDiff) -> roundTrip d

  it "empty WorldDiff round-trips (all fields omitted in JSON)" $
    let empty = WorldDiff [] [] [] [] [] [] [] [] 0
    in decode (encode empty) `shouldBe` Just empty

  it "WorldDiff with only worldTagsAdded round-trips" $ property $
    \(tags :: [Tag]) ->
      let d = WorldDiff [] [] [] [] tags [] [] [] 0
      in roundTrip d

  -- -------------------------------------------------------------------------
  -- LogEntry — custom instance with optional Base16 signature
  -- -------------------------------------------------------------------------

  it "LogEntry without signature round-trips" $ property $
    \(e :: LogEntry) -> roundTrip (e { entrySignature = Nothing })

  it "LogEntry with signature round-trips" $ property $
    \(e :: LogEntry) (sig :: [Word8]) ->
      let e' = e { entrySignature = Just (BS.pack sig) }
      in roundTrip e'

  -- A log entry written before the schema-version field existed omits
  -- "schema" entirely.  'FromJSON' must accept it and default to 1, so
  -- existing player saves keep working across the upgrade.
  it "LogEntry without a schema field loads as version 1" $
    let legacy = BLC.pack
          "{\"action\":{\"actionIdText\":\"act\"},\
          \\"clock\":{\"lcPlayerId\":\"p\",\"lcTick\":1},\
          \\"diff\":{},\"frontier\":{},\"id\":\"1-p\",\"player\":\"p\"}"
    in fmap entrySchemaVersion (decode legacy :: Maybe LogEntry) `shouldBe` Just 1

  -- -------------------------------------------------------------------------
  -- ORSet — custom instance (entries/tombstones structure)
  -- -------------------------------------------------------------------------

  it "ORSet Tag round-trips" $ property $
    \(s :: ORSet Tag) -> roundTrip s

  it "ORSet round-trip preserves live membership" $ property $
    \(s :: ORSet Tag) (t :: Tag) ->
      let decoded = decode (encode s) :: Maybe (ORSet Tag)
      in fmap (orMember t) decoded == Just (orMember t s)

  it "ORSet round-trip preserves tombstones" $ property $
    \(s :: ORSet Tag) ->
      fmap orTombstones (decode (encode s) :: Maybe (ORSet Tag))
        == Just (orTombstones s)

  -- -------------------------------------------------------------------------
  -- PNCounter — generic instance (exercises ToJSONKey PlayerId)
  -- -------------------------------------------------------------------------

  it "PNCounter PlayerId round-trips" $ property $
    \(pid :: PlayerId) (Positive base) (Small d) ->
      roundTrip (pnModify pid d (pnZero base))

  -- -------------------------------------------------------------------------
  -- Cycle / CycleMany — these were previously infinite OnExpire chains;
  -- serialization is where that bug would silently resurface.
  -- -------------------------------------------------------------------------

  it "Cycle EffectBody round-trips" $ property $
    \(e :: Effect) ->
      let body = Cycle 2 (effectBody e) DoNothing
          e'   = e { effectBody = body }
      in roundTrip e'

  it "CycleMany EffectBody round-trips" $ property $
    \(e :: Effect) (b1 :: EffectBody) (b2 :: EffectBody) ->
      let body = CycleMany 2 (effectBody e :| [b1, b2])
          e'   = e { effectBody = body }
      in roundTrip e'

  it "nested Cycle inside CycleMany round-trips" $ property $
    \(inner1 :: EffectBody) (inner2 :: EffectBody) ->
      let outer = CycleMany 3 (Cycle 1 inner1 inner2 :| [inner1])
          e     = Effect outer Nothing (All []) Nothing
      in roundTrip e

  it "Cycle and CycleMany carry the correct bodies after decode" $ do
    let body1  = AddWorldTag (ScenarioTag (MkScenarioTag "a"))
        body2  = AddWorldTag (ScenarioTag (MkScenarioTag "b"))
        cycle2 = Cycle 2 body1 body2
        e      = Effect cycle2 (Just 3) (All []) Nothing
    decode (encode e) `shouldBe` Just e

  it "CycleMany rotation list is preserved exactly after decode" $ do
    let bodies = AddWorldTag (ScenarioTag (MkScenarioTag "x"))
                 :| [ AddWorldTag (ScenarioTag (MkScenarioTag "y"))
                    , AddWorldTag (ScenarioTag (MkScenarioTag "z"))
                    ]
        e = Effect (CycleMany 1 bodies) Nothing (All []) Nothing
    decode (encode e) `shouldBe` Just e

  -- -------------------------------------------------------------------------
  -- Serializable axiom rule types
  -- -------------------------------------------------------------------------

  it "Trigger round-trips" $ property $
    \(t :: Trigger) -> roundTrip t

  it "Target round-trips" $ property $
    \(t :: Target) -> roundTrip t

  it "AxiomRule round-trips" $ property $
    \(r :: AxiomRule) -> roundTrip r

  it "MergeTrigger round-trips" $ property $
    \(t :: MergeTrigger) -> roundTrip t

  it "MergeAxiomRule round-trips" $ property $
    \(r :: MergeAxiomRule) -> roundTrip r

  it "Narration round-trips" $ property $
    \(n :: Narration) -> roundTrip n

  -- -------------------------------------------------------------------------
  -- LocationGraph types
  -- -------------------------------------------------------------------------

  it "Region round-trips" $ property $
    \(r :: Region) -> roundTrip r

  it "LocationGraph round-trips" $ property $
    \(g :: LocationGraph) -> roundTrip g

  -- -------------------------------------------------------------------------
  -- Condition extensions (new constructors)
  -- -------------------------------------------------------------------------

  it "Condition round-trips (includes CoLocated, InRegion, Chance, HasCoLocated)" $ property $
    \(c :: Condition) -> roundTrip c

  -- -------------------------------------------------------------------------
  -- Spatial EffectBody variants
  -- -------------------------------------------------------------------------

  it "EffectBody round-trips (includes SetLocationRandom, SetLocationAdjacent)" $ property $
    \(e :: EffectBody) -> roundTrip e

  -- -------------------------------------------------------------------------
  -- AnyAction round-trips (serializable actions)
  -- -------------------------------------------------------------------------

  it "AnyAction round-trips" $ property $
    \(a :: AnyAction) -> roundTrip a

  -- -------------------------------------------------------------------------
  -- Snapshot — the serializable scenario handoff package.
  -- This is the headline guarantee of the serialization proposal: a snapshot
  -- bundles a world plus the scenario's actions, rules, and merge rules, and
  -- a recipient can reconstruct the full bundle by JSON round-trip alone.
  -- -------------------------------------------------------------------------

  it "Snapshot with empty world round-trips" $
    roundTrip (Snapshot emptyWorld 0 [] [] []) `shouldBe` True

  it "Snapshot with non-trivial world round-trips" $
    roundTrip (Snapshot twoCharWorld 17 [] [] []) `shouldBe` True

  it "Snapshot carries actions/rules/mergeRules through round-trip" $ property $
    \(acts :: [AnyAction]) (rs :: [AxiomRule]) (mrs :: [MergeAxiomRule]) (off :: Int) ->
      let s = Snapshot twoCharWorld off acts rs mrs
      in roundTrip s

  it "Snapshot round-trips at Int extremes (minBound, maxBound)" $ do
    roundTrip (Snapshot emptyWorld (minBound :: Int) [] [] []) `shouldBe` True
    roundTrip (Snapshot emptyWorld (maxBound :: Int) [] [] []) `shouldBe` True

  -- A snapshot written before snapActions/snapRules/snapMergeRules existed
  -- omits all three.  FromJSON defaults them to empty lists so legacy
  -- player saves keep loading.
  it "legacy Snapshot without scenario fields loads with empty action/rule lists" $
    let canonical = Snapshot emptyWorld 0 [] [] []
        legacy    = encode (object
          [ "snapWorld"  .= emptyWorld
          , "snapOffset" .= (0 :: Int)
          ])
    in ((decode legacy :: Maybe Snapshot) == Just canonical) `shouldBe` True
