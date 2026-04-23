{-# OPTIONS_GHC -Wno-orphans #-}
-- | Public type surface for the engine.  Re-exports every type,
-- smart constructor, and JSON instance that scenario authors, the
-- runtime, and the renderer all need, so downstream modules can
-- @import GameTypes@ once instead of chasing half a dozen internal
-- sub-modules.  Definitions live in "GameTypes.Types" and the
-- "Engine.CRDT.*" modules; this one only re-exports.
module GameTypes
  ( -- * Event log
    PlayerId(..)
  , LamportClock(..)
  , CausalFrontier
  , LogEntry(..)
    -- * Identifiers & Tags
  , CharId(..)
  , Entity(..)
  , Tag(..)
  , ScenarioTagValue(..)
  , ClockTag(..)
  , ActionId(..)
  , WeatherDesc(..)
  , FatigueLevel(..)
  , HungerLevel(..)
  , SocialEnergyLevel(..)
  , EngineTag(..)
    -- * Locations
  , Location(..)
  , Region(..)
  , LocationGraph(..)
  , emptyLocationGraph
    -- * Narration
  , Narration(..)
    -- * Stats
  , CapacityStat(..)
  , StatType(..)
    -- * Characters & Relationships
  , Relationship(..)
  , RelationshipGraph
  , Character(..)
    -- * World
  , GameWorld(..)
    -- * Conditions
  , Condition(..)
    -- * Effects
  , EffectBody( AddTag, AddWorldTag, RemoveTag, RemoveWorldTag
              , ModifyRelation, Say, Think, Narrate, NarratePool, SetLocation
              , OnExpire, CycleMany, Cycle, Dialogue
              , SetLocationRandom, SetLocationAdjacent, SetLocationAdjacentPrefer
              , JournalEntry, AdvanceDay
              , DoNothing
              )
  , Effect(..)
  , LiveEffect(..)
    -- * Actions (sealed — construct via DSL helpers)
  , Frequency(..)
  , Action
  , actionId
  , actionLabel
  , actionTarget
  , actionCondition
  , actionEffects
  , AnyAction(..)
  , anyActionId
  , anyActionLabel
  , anyActionTarget
  , anyActionCondition
  , anyActionEffects
    -- * World diff
  , StatDelta(..)
  , RelationDelta(..)
  , LocationDelta(..)
  , WorldDiff(..)
    -- * Axioms
  , AxiomId(..)
  , Axiom(..)
  , AxiomTrace(..)
    -- * Merge causality
  , Provenance(..)
  , MergeDelta(..)
  , MergeDiff(..)
  , MergeAxiom(..)
    -- * Declarative axiom rules
  , self
  , Trigger(..)
  , Target(..)
  , AxiomRule(..)
  , MergeTrigger(..)
  , MergeAxiomRule(..)
    -- * Snapshots
  , Snapshot(..)
  , mergeActions
  , mergeRules
  , mergeMergeRules
    -- * LogStore
  , LogStore(..)
    -- * Scenarios
  , DebugMode(..)
  , Scenario(..)
  , TombstoneGCRule
    -- * Smart constructors
  , scenarioTag
  , actionTaken
  , dialogueInProgress
  , weatherTag
  , timeTag
  , dayOfWeekTag
  , lunarPhaseTag
  , seasonTag
  , dayNumberTag
  , isWeather
  , isTimeTag
  , isDayOfWeekTag
  , isLunarPhaseTag
  , isSeasonTag
  , isDayNumberTag
  , isClockTag
  , tensionTag
  , isTensionTag
  , getTension
  , fatigueTag
  , isFatigueTag
  , hungerStateTag
  , isHungerStateTag
  , sleepingTag
  , isSleepingTag
  , socialEnergyTag
  , isSocialEnergyTag
  , getRelStat
  , unconditional
    -- * LocationGraph queries
  , lgAdjacentTo
  , lgInRegion
  , lgSameRegion
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import           Engine.CRDT.ORSet    (orToList)
import           Engine.CRDT.PNCounter (pnValue)
import           GameTypes.Instances ()
import           GameTypes.Types

-- ---------------------------------------------------------------------------
-- Smart constructors
-- ---------------------------------------------------------------------------

-- | Convert a scenario-defined tag ADT to a Tag via its Show instance.
-- WARNING: the Show output is used as the tag's persistent identity.
-- Changing a tag type's Show instance (including reordering constructors
-- when using derived Show) will silently break saved state compatibility.
scenarioTag :: Show a => a -> Tag
scenarioTag = ScenarioTag . MkScenarioTag . show

actionTaken :: ActionId -> Tag
actionTaken = EngineTag . ActionTaken

dialogueInProgress :: Tag
dialogueInProgress = EngineTag DialogueInProgress

weatherTag :: WeatherDesc -> Tag
weatherTag = EngineTag . Weather

tensionTag :: Int -> Tag
tensionTag = EngineTag . Tension

timeTag :: Int -> Tag
timeTag = EngineTag . Clock . TimeOfDay

dayOfWeekTag :: Int -> Tag
dayOfWeekTag = EngineTag . Clock . DayOfWeek

lunarPhaseTag :: Int -> Tag
lunarPhaseTag = EngineTag . Clock . LunarPhase

seasonTag :: Int -> Tag
seasonTag = EngineTag . Clock . Season

dayNumberTag :: Int -> Tag
dayNumberTag = EngineTag . Clock . DayNumber

-- ---------------------------------------------------------------------------
-- Predicates
-- ---------------------------------------------------------------------------

isWeather :: Tag -> Bool
isWeather (EngineTag (Weather _)) = True
isWeather _                       = False

isTimeTag :: Tag -> Bool
isTimeTag (EngineTag (Clock (TimeOfDay _))) = True
isTimeTag _                                 = False

isDayOfWeekTag :: Tag -> Bool
isDayOfWeekTag (EngineTag (Clock (DayOfWeek _))) = True
isDayOfWeekTag _                                 = False

isLunarPhaseTag :: Tag -> Bool
isLunarPhaseTag (EngineTag (Clock (LunarPhase _))) = True
isLunarPhaseTag _                                  = False

isSeasonTag :: Tag -> Bool
isSeasonTag (EngineTag (Clock (Season _))) = True
isSeasonTag _                              = False

isDayNumberTag :: Tag -> Bool
isDayNumberTag (EngineTag (Clock (DayNumber _))) = True
isDayNumberTag _                                 = False

isClockTag :: Tag -> Bool
isClockTag (EngineTag (Clock _)) = True
isClockTag _                     = False

isTensionTag :: Tag -> Bool
isTensionTag (EngineTag (Tension _)) = True
isTensionTag _                       = False

fatigueTag :: FatigueLevel -> Tag
fatigueTag = EngineTag . Fatigue

isFatigueTag :: Tag -> Bool
isFatigueTag (EngineTag (Fatigue _)) = True
isFatigueTag _                       = False

hungerStateTag :: HungerLevel -> Tag
hungerStateTag = EngineTag . HungerState

isHungerStateTag :: Tag -> Bool
isHungerStateTag (EngineTag (HungerState _)) = True
isHungerStateTag _                           = False

sleepingTag :: Tag
sleepingTag = EngineTag Sleeping

isSleepingTag :: Tag -> Bool
isSleepingTag (EngineTag Sleeping) = True
isSleepingTag _                    = False

socialEnergyTag :: SocialEnergyLevel -> Tag
socialEnergyTag = EngineTag . SocialEnergy

isSocialEnergyTag :: Tag -> Bool
isSocialEnergyTag (EngineTag (SocialEnergy _)) = True
isSocialEnergyTag _                             = False

-- ---------------------------------------------------------------------------
-- World queries
-- ---------------------------------------------------------------------------

getTension :: GameWorld -> Int
getTension w = foldr check 0 (orToList (worldTags w))
  where
    check (EngineTag (Tension n)) _ = n
    check _                      acc = acc

-- ---------------------------------------------------------------------------
-- StatType instances
-- ---------------------------------------------------------------------------

instance Bounded StatType where
  minBound = Capacity minBound
  maxBound = Perceived maxBound

instance Enum StatType where
  fromEnum (Capacity c)   = fromEnum c                              -- 0..capMax
  fromEnum Trust          = capMax + 1                              -- capMax+1
  fromEnum (Perceived c)  = capMax + 2 + fromEnum c                 -- capMax+2..2*capMax+2

  toEnum n
    | n >= 0,       n <= capMax          = Capacity  (toEnum n)
    | n == capMax + 1                    = Trust
    | n >= capMax + 2, n <= 2*capMax + 2 = Perceived (toEnum (n - capMax - 2))
    | otherwise = error ("StatType.toEnum: out of range " ++ show n)

-- | Highest 'fromEnum' value of a 'CapacityStat'.  Drives the index
-- ranges in the 'Enum StatType' instance above so new capacity stats
-- slot in without having to hand-update bounds.
capMax :: Int
capMax = fromEnum (maxBound :: CapacityStat)

-- ---------------------------------------------------------------------------
-- Relationship helpers
-- ---------------------------------------------------------------------------

getRelStat :: StatType -> Relationship -> Int
getRelStat stat (Relationship m) = maybe 0 pnValue (Map.lookup stat m)

-- ---------------------------------------------------------------------------
-- AnyAction accessors
-- ---------------------------------------------------------------------------

anyActionId :: AnyAction -> ActionId
anyActionId (AnyAction a) = actionId a

anyActionLabel :: AnyAction -> String
anyActionLabel (AnyAction a) = actionLabel a

anyActionTarget :: AnyAction -> Maybe Entity
anyActionTarget (AnyAction a) = actionTarget a

anyActionCondition :: AnyAction -> Condition
anyActionCondition (AnyAction a) = actionCondition a

anyActionEffects :: AnyAction -> [Effect]
anyActionEffects (AnyAction a) = actionEffects a

-- ---------------------------------------------------------------------------
-- Conditions
-- ---------------------------------------------------------------------------

unconditional :: Condition
unconditional = All []

-- ---------------------------------------------------------------------------
-- Snapshot merge helpers
-- ---------------------------------------------------------------------------

-- | Union actions by ActionId, keeping ours on conflict.
mergeActions :: [AnyAction] -> [AnyAction] -> [AnyAction]
mergeActions mine theirs = mine ++ filter isNew theirs
  where
    myIds = Set.fromList [anyActionId a | a <- mine]
    isNew a = anyActionId a `Set.notMember` myIds

-- | Union rules by ruleId, keeping ours on conflict.
mergeRules :: [AxiomRule] -> [AxiomRule] -> [AxiomRule]
mergeRules mine theirs = mine ++ filter (\r -> ruleId r `Set.notMember` myIds) theirs
  where myIds = Set.fromList (map ruleId mine)

-- | Union merge rules by mergeRuleId, keeping ours on conflict.
mergeMergeRules :: [MergeAxiomRule] -> [MergeAxiomRule] -> [MergeAxiomRule]
mergeMergeRules mine theirs = mine ++ filter (\r -> mergeRuleId r `Set.notMember` myIds) theirs
  where myIds = Set.fromList (map mergeRuleId mine)

-- ---------------------------------------------------------------------------
-- LocationGraph queries
-- ---------------------------------------------------------------------------

-- | All locations reachable from the given one via a single edge.
lgAdjacentTo :: Location -> LocationGraph -> [Location]
lgAdjacentTo loc lg =
  [ b | (a, b) <- Set.toList (lgEdges lg), a == loc ] ++
  [ a | (a, b) <- Set.toList (lgEdges lg), b == loc ]

-- | Which region does this location belong to?
lgInRegion :: Location -> LocationGraph -> Maybe Region
lgInRegion loc lg = Map.lookup loc (lgRegions lg)

-- | Are two locations in the same region?
lgSameRegion :: Location -> Location -> LocationGraph -> Bool
lgSameRegion a b lg = case (lgInRegion a lg, lgInRegion b lg) of
  (Just ra, Just rb) -> ra == rb
  _                  -> False
