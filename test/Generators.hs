{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
module Generators where

import qualified Data.ByteString as BS
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.UUID        (UUID)
import qualified Data.UUID.V5    as UUID.V5
import qualified Data.UUID       as UUID
import           Test.QuickCheck

import           Engine.Author.DSL      (staticInitEffect)
import           Engine.CRDT.ORSet
import           GameTypes
import           GameTypes.Types         (Action(..))

-- ---------------------------------------------------------------------------
-- UUID helper
-- ---------------------------------------------------------------------------

-- | Deterministic UUID from an Int for static test setup.
arbUUID :: Int -> UUID
arbUUID n = UUID.V5.generateNamed UUID.nil (map (fromIntegral . fromEnum) (show n))

-- ---------------------------------------------------------------------------
-- Arbitrary instances (orphans suppressed by OPTIONS_GHC above)
-- ---------------------------------------------------------------------------

instance Arbitrary CharacterId where
  arbitrary = oneof
    [ Named <$> elements ["player", "npc", "bradley", "you"]
    , pure Truth
    ]

instance Arbitrary CapacityStat where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary SocialEnergyLevel where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary StatType where
  arbitrary = oneof
    [ Capacity    <$> arbitrary
    , pure Trust
    , Perceived   <$> arbitrary
    ]

instance Arbitrary ClockTag where
  arbitrary = oneof
    [ TimeOfDay  <$> choose (0, 23)
    , DayOfWeek  <$> choose (0, 6)
    , LunarPhase <$> choose (0, 28)
    , Season     <$> choose (0, 3)
    , DayNumber  <$> choose (0, 365)
    ]

instance Arbitrary FatigueLevel where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary HungerLevel where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary EngineTag where
  arbitrary = oneof
    [ ActionTaken  <$> elements [ActionId "act1", ActionId "act2", ActionId "act3"]
    , Weather      <$> elements [WeatherDesc "sun", WeatherDesc "rain", WeatherDesc "snow"]
    , Clock        <$> arbitrary
    , pure DialogueInProgress
    , Fatigue      <$> arbitrary
    , HungerState  <$> arbitrary
    , pure Sleeping
    , SocialEnergy <$> arbitrary
    ]

instance Arbitrary Tag where
  arbitrary = oneof
    [ EngineTag   <$> arbitrary
    , ScenarioTag . MkScenarioTag <$> elements ["tag-a", "tag-b", "tag-c", "awkward", "tired"]
    ]

instance Arbitrary PlayerId where
  arbitrary = PlayerId <$> elements ["player-a", "player-b", "player-c"]

instance Arbitrary LamportClock where
  arbitrary = LamportClock <$> choose (0, 1000) <*> arbitrary

-- | Build an ORSet from a list of inserts followed by some deletes.
-- Each insert gets a deterministic UUID from its list index.
instance (Arbitrary a, Ord a, Show a) => Arbitrary (ORSet a) where
  arbitrary = do
    inserts <- listOf arbitrary
    deletes <- sublistOf inserts
    let s0 = foldr (\(i, x) acc -> orInsert (arbUUID i) x acc)
                   orEmpty
                   (zip [0..] inserts)
        s1 = foldr orDelete s0 deletes
    pure s1

-- statDeltaPlayer / relationDeltaPlayer are stripped from JSON storage and
-- patched in by LogEntry's FromJSON. Standalone round-trip tests must use the
-- same placeholder that FromJSON inserts, so encode/decode is idempotent.
instance Arbitrary StatDelta where
  arbitrary = StatDelta
    <$> arbitrary <*> arbitrary
    <*> choose (-10, 10) <*> choose (-10, 10)
    <*> pure (PlayerId "<unpatched>")

instance Arbitrary RelationDelta where
  arbitrary = RelationDelta
    <$> arbitrary <*> arbitrary <*> arbitrary
    <*> choose (-10, 10) <*> choose (-10, 10)
    <*> pure (PlayerId "<unpatched>")

instance Arbitrary LocationDelta where
  arbitrary = LocationDelta
    <$> arbitrary
    <*> elements [Location "sales-floor", Location "home", Location "break-room"]
    <*> elements [Location "sales-floor", Location "home", Location "break-room"]

instance Arbitrary WorldDiff where
  arbitrary = WorldDiff
    <$> arbitrary <*> arbitrary
    <*> arbitrary <*> arbitrary
    <*> arbitrary <*> arbitrary
    <*> arbitrary <*> arbitrary
    <*> choose (0, 2)

instance Arbitrary LogEntry where
  arbitrary = do
    eid  <- elements ["1-pa", "2-pa", "3-pb"]
    clk  <- arbitrary
    pid  <- arbitrary
    aid  <- elements [ActionId "act1", ActionId "act2", ActionId "wait"]
    diff <- arbitrary
    sig  <- oneof [pure Nothing, Just . BS.pack <$> vectorOf 64 arbitrary]
    -- Enforce invariant: *Player in deltas always equals entryPlayerId.
    let patchedDiff = diff
          { diffStats     = map (\sd -> sd { statDeltaPlayer     = pid }) (diffStats diff)
          , diffRelations = map (\rd -> rd { relationDeltaPlayer = pid }) (diffRelations diff)
          }
    frontier <- arbitrary
    pure (LogEntry eid clk pid aid patchedDiff sig frontier 1)

-- Condition, EffectBody, and Effect are mutually recursive via OnExpire.
-- Use `sized` to bound depth; at size 0 only the leaf constructors fire.

instance Arbitrary Region where
  arbitrary = Region <$> elements ["North Field", "South Field", "East Bush", "West Ridge"]

instance Arbitrary LocationGraph where
  arbitrary = pure emptyLocationGraph

instance Arbitrary Condition where
  arbitrary = sized arbCondition
    where
      arbCondition 0 = oneof leaves
      arbCondition n = oneof $ leaves ++
        [ Not <$> arbCondition (n `div` 2)
        , All <$> vectorOf 2 (arbCondition (n `div` 2))
        , Any <$> vectorOf 2 (arbCondition (n `div` 2))
        ]
      leaves =
        [ HasTag      <$> arbitrary <*> arbitrary
        , HasWorldTag <$> arbitrary
        , RelationAbove <$> arbitrary <*> arbitrary <*> arbitrary <*> choose (-5, 10)
        , AtLocation  <$> arbitrary <*> elements [Location "sales-floor", Location "home"]
        , CoLocated   <$> arbitrary <*> arbitrary
        , InRegion    <$> arbitrary <*> arbitrary
        , InSameRegion <$> arbitrary <*> arbitrary
        , Chance      <$> choose (0, 1000) <*> choose (0.0, 1.0)
        , HasCoLocated <$> arbitrary <*> resize 2 (listOf arbitrary)
        ]

instance Arbitrary EffectBody where
  arbitrary = sized arbBody
    where
      arbBody 0 = oneof leaves
      arbBody n = oneof $ leaves ++
        [ OnExpire  <$> arbBody (n `div` 2) <*> arbEffect (n `div` 2)
        , CycleMany <$> choose (1, 3) <*> ((:|) <$> arbBody (n `div` 2) <*> vectorOf 1 (arbBody (n `div` 2)))
        , Cycle     <$> choose (1, 3) <*> arbBody (n `div` 2) <*> arbBody (n `div` 2)
        ]
      leaves =
        [ AddTag         <$> arbitrary <*> arbitrary
        , AddWorldTag    <$> arbitrary
        , RemoveTag      <$> arbitrary <*> arbitrary
        , RemoveWorldTag <$> arbitrary
        , ModifyRelation <$> arbitrary <*> arbitrary <*> arbitrary <*> choose (-5, 5)
        , Say            <$> arbitrary <*> arbitrary <*> elements ["Hello.", "Goodbye.", "..."]
        , Think          <$> arbitrary <*> elements ["Hmm.", "Strange.", "OK."]
        , Narrate        <$> elements ["It happened.", "Nothing moved.", "Silence."]
        , NarratePool    <$> choose (1, 100) <*> listOf1 (elements ["Variant 1.", "Variant 2.", "Variant 3."])
        , SetLocation    <$> arbitrary <*> elements [Location "sales-floor", Location "home"]
        , SetLocationRandom <$> arbitrary <*> choose (1, 100)
            <*> listOf1 (elements [Location "sales-floor", Location "home"])
        , SetLocationAdjacent <$> arbitrary <*> choose (1, 100)
        , SetLocationAdjacentPrefer <$> arbitrary <*> choose (1, 100) <*> arbitrary
        , pure DoNothing
        ]
      arbEffect n = Effect
        <$> arbBody n
        <*> oneof [pure Nothing, Just <$> choose (1, 5)]
        <*> resize (n `div` 2) arbitrary
        <*> pure Nothing

instance Arbitrary Effect where
  arbitrary = sized $ \n -> Effect
    <$> arbitrary
    <*> oneof [pure Nothing, Just <$> choose (1, 5)]
    <*> resize (n `div` 2) arbitrary
    <*> pure Nothing

instance Arbitrary LiveEffect where
  arbitrary = staticInitEffect <$> arbitrary

instance Arbitrary AnyAction where
  arbitrary = AnyAction <$> (Action
    <$> elements [ActionId "act1", ActionId "act2", ActionId "wait"]
    <*> elements ["Do something.", "Wait.", "Go."]
    <*> pure Nothing
    <*> resize 2 arbitrary
    <*> resize 2 (listOf arbitrary)
    :: Gen (Action 'Repeatable))

instance Arbitrary AxiomId where
  arbitrary = oneof
    [ SystemAxiom   <$> elements ["fatigue", "hunger", "socialEnergy"]
    , ScenarioAxiom <$> elements ["perception", "tension", "dawn"]
    ]

instance Arbitrary Trigger where
  arbitrary = oneof
    [ WhenTagAdded <$> arbitrary
    , WhenWorldTagAdded <$> arbitrary
    , WhenStatChanged <$> arbitrary
    , WhenRelationChanged <$> arbitrary
    , pure WhenLocationChanged
    , pure EveryTick
    ]

instance Arbitrary Target where
  arbitrary = oneof
    [ pure EachCharacter
    , SpecificChar <$> arbitrary
    , pure ChangedChars
    , CoLocatedWith <$> arbitrary
    , CharsAtLocation <$> elements [Location "sales-floor", Location "home"]
    ]

instance Arbitrary AxiomRule where
  arbitrary = AxiomRule
    <$> arbitrary <*> choose (1, 10) <*> arbitrary
    <*> resize 2 arbitrary <*> arbitrary <*> resize 2 (listOf arbitrary)

instance Arbitrary MergeTrigger where
  arbitrary = elements [WhenMergeRelationChanged, WhenMergeLocationChanged, WhenMergeTagChanged, WhenMergeWorldTagChanged, OnAnyMerge]

instance Arbitrary MergeAxiomRule where
  arbitrary = MergeAxiomRule
    <$> arbitrary <*> choose (1, 10) <*> arbitrary
    <*> oneof [pure Nothing, Just <$> elements [Aware, Unaware, Stale]]
    <*> resize 2 arbitrary <*> resize 2 (listOf arbitrary)

instance Arbitrary AxiomTrace where
  arbitrary = AxiomTrace <$> arbitrary <*> choose (0, 10) <*> resize 2 (listOf arbitrary)

instance Arbitrary Narration where
  arbitrary = oneof
    [ Static <$> elements ["It happened.", "Silence.", "Nothing moved."]
    , Conditional <$> resize 2 (listOf ((,) <$> resize 2 arbitrary <*> elements ["Rain.", "Sun.", "Night."]))
                  <*> elements ["Default.", "Fallback."]
    , NarrationPool <$> choose (1, 100) <*> listOf1 (elements ["Variant 1.", "Variant 2.", "Variant 3."])
    ]
