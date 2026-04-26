{-# OPTIONS_GHC -fno-hpc     #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
-- | Core domain types: characters, effects, actions, conditions, tags, stats, relationships, and world state.
module GameTypes.Types where

import           Control.DeepSeq (NFData(rnf))
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Data.Time.Clock (UTCTime)
import           GHC.Generics    (Generic)
import qualified Data.ByteString as BS
import           Data.UUID       (UUID)

import           Engine.CRDT.ORSet
import           Engine.CRDT.PNCounter

-- ---------------------------------------------------------------------------
-- Event log
-- ---------------------------------------------------------------------------

-- | The latest entry seen from each sync partner at the time of a log entry.
-- Empty for entries created during pure local play (no syncs yet).
type CausalFrontier = Map.Map PlayerId String

newtype PlayerId = PlayerId String
  deriving (Show, Eq, Ord, Generic)

data LamportClock = LamportClock
  { lcTick     :: Int
  , lcPlayerId :: PlayerId
  } deriving (Show, Eq, Ord, Generic)

-- | One action's worth of log, written append-only to the event log.
-- 'entrySchemaVersion' stamps the on-disk format so future schema changes
-- can migrate older logs rather than rejecting them. Absent from a parsed
-- entry means "pre-versioning" and is treated as version 1.
data LogEntry = LogEntry
  { entryId            :: String
  , entryClock         :: LamportClock
  , entryPlayerId      :: PlayerId
  , entryActionId      :: ActionId
  , entryDiff          :: WorldDiff
  , entrySignature     :: Maybe BS.ByteString
  , entryFrontier      :: CausalFrontier
  , entrySchemaVersion :: Int
  } deriving (Show, Eq, Generic)

-- ---------------------------------------------------------------------------
-- Identifiers & Tags
-- ---------------------------------------------------------------------------

data CharacterId = Named String | Truth
  deriving (Eq, Ord, Generic)

-- | An entity that can be the target of an action.
-- Starts as just characters; extensible to locations, objects, etc.
newtype Entity = ECharacter CharacterId
  deriving (Show, Eq, Ord, Generic)

instance Show CharacterId where
  show (Named s) = s
  show Truth     = "Truth"

data Tag
  = EngineTag EngineTag
  | ScenarioTag ScenarioTagValue
  deriving (Show, Eq, Ord, Generic)

-- | Opaque wrapper for scenario-defined tags. Scenarios construct these
-- via the scenarioTag helper using their own ADTs.
newtype ScenarioTagValue = MkScenarioTag String
  deriving (Show, Eq, Ord, Generic)

data ClockTag
  = TimeOfDay Int
  | DayOfWeek Int
  | LunarPhase Int
  | Season Int
  | DayNumber Int
  deriving (Show, Eq, Ord, Generic)

newtype ActionId = ActionId { actionIdText :: String }
  deriving (Eq, Ord, Generic)

instance Show ActionId where
  show (ActionId s) = s

newtype WeatherDesc = WeatherDesc { weatherName :: String }
  deriving (Eq, Ord, Generic)

instance Show WeatherDesc where
  show (WeatherDesc s) = s

data FatigueLevel = Rested | Tired | Exhausted
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

data HungerLevel = Satiated | Peckish | Hungry
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

data SocialEnergyLevel = Energized | Neutral | Drained
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

data EngineTag
  = ActionTaken ActionId
  | Weather WeatherDesc
  | Clock ClockTag
  | DialogueInProgress
  | Tension Int
  | Fatigue FatigueLevel
  | HungerState HungerLevel
  | Sleeping
  | SocialEnergy SocialEnergyLevel
  | ForeignOrigin PlayerId
  deriving (Show, Eq, Ord, Generic)

-- ---------------------------------------------------------------------------
-- Locations
-- ---------------------------------------------------------------------------

newtype Location = Location { locationName :: String }
  deriving (Eq, Ord, Generic)

instance Show Location where
  show (Location s) = s

newtype Region = Region { regionName :: String }
  deriving (Eq, Ord, Show, Read, Generic)

data LocationGraph = LocationGraph
  { lgEdges   :: Set.Set (Location, Location)
  , lgRegions :: Map.Map Location Region
  , lgCoords  :: Map.Map Location (Double, Double)
  } deriving (Show, Eq, Generic)

emptyLocationGraph :: LocationGraph
emptyLocationGraph = LocationGraph
  { lgEdges   = Set.empty
  , lgRegions = Map.empty
  , lgCoords  = Map.empty
  }

-- ---------------------------------------------------------------------------
-- Narration
-- ---------------------------------------------------------------------------

data Narration
  = Static String
  | Conditional [(Condition, String)] String   -- [(guard, text)] with fallback
  | NarrationPool Int [String]                 -- salt + variants, picked by PRNG
  deriving (Show, Eq, Generic)

-- ---------------------------------------------------------------------------
-- Stats
-- ---------------------------------------------------------------------------

data CapacityStat = Intelligence | Strength | Charisma | Understanding | Hunger | SocialStamina | Stillness | Experience
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

data StatType
  = Capacity CapacityStat
  | Trust
  | Perceived CapacityStat
  deriving (Show, Read, Eq, Ord, Generic)

-- ---------------------------------------------------------------------------
-- Characters & Relationships
-- ---------------------------------------------------------------------------

newtype Relationship = Relationship
  { relStats :: Map.Map StatType (PNCounter PlayerId)
  } deriving (Show, Eq, Generic)

type RelationshipGraph = Map.Map CharacterId (Map.Map CharacterId Relationship)

data Character = Character
  { charId      :: CharacterId
  , charName    :: String
  , charEffects :: [Effect]
  , charTags    :: ORSet Tag
  } deriving (Generic)

-- ---------------------------------------------------------------------------
-- World
-- ---------------------------------------------------------------------------

data GameWorld = GameWorld
  { worldCharacters       :: Map.Map CharacterId Character
  , worldGraph            :: RelationshipGraph
  , worldLocations        :: Map.Map CharacterId Location
  , worldActiveEffects    :: [LiveEffect]
  , worldTags             :: ORSet Tag
  , worldClock            :: LamportClock
  , worldLocationGraph    :: LocationGraph
  , worldSeed             :: Int
  , worldLocationHistory  :: Map.Map CharacterId [Location]
    -- ^ Most recently departed location first, newest at head.  Bounded
    -- to a small window so downstream renderers can draw a fading trail.
  , worldLocationVisits   :: Map.Map CharacterId (Map.Map Location Int)
    -- ^ Per-character visit count for each location, incremented on
    -- arrival.  Powers familiarity cues in the spatial HUD.
  , worldJournal          :: [String]
    -- ^ Append-only list of journal entries written by the player
    -- character over the life of the scenario (across all days).
    -- Ordered oldest-first.  Entries are produced by the
    -- 'JournalEntry' effect body and carried on the event log via
    -- 'diffJournal', so they survive session close and merge.
  , worldDayNumber        :: Int
    -- ^ Which in-scenario day the player is on.  Starts at 1 on
    -- scenario open and increments each time the day rolls over.
    -- Scenarios that don't use multi-day structure can leave it at 1.
  } deriving (Generic)

-- ---------------------------------------------------------------------------
-- Conditions
-- ---------------------------------------------------------------------------

data Condition
  = HasTag CharacterId Tag
  | HasWorldTag Tag
  | RelationAbove CharacterId CharacterId StatType Int
  | AtLocation CharacterId Location
  | CoLocated CharacterId CharacterId
  | InRegion CharacterId Region
  | InSameRegion CharacterId CharacterId
  | Chance Int Double
  | HasCoLocated CharacterId [CharacterId]   -- ^ character has at least one co-located
                                   -- character, excluding those listed
  | Not Condition
  | All [Condition]
  | Any [Condition]
  deriving (Show, Eq, Generic)

-- ---------------------------------------------------------------------------
-- Effects
-- ---------------------------------------------------------------------------

-- | The body of an effect — what it /does/ each tick it is alive.
--
-- Simple constructors fire once per tick. Compound constructors ('OnExpire',
-- 'Cycle', 'CycleMany') compose bodies over time and carry runtime
-- preconditions on their numeric arguments; prefer the DSL helpers in
-- "Engine.Author.DSL" which enforce those preconditions at construction time.
data EffectBody
  = -- | Attach a 'Tag' to a character.
    AddTag CharacterId Tag
    -- | Attach a 'Tag' to the world.
  | AddWorldTag Tag
    -- | Remove a 'Tag' from a character.
  | RemoveTag CharacterId Tag
    -- | Remove a 'Tag' from the world.
  | RemoveWorldTag Tag
    -- | Adjust a directed relationship stat by a signed delta.
  | ModifyRelation CharacterId CharacterId StatType Int
    -- | Spoken dialogue attributed to a character, optionally directed at listeners.
    -- [] = said to the room (announcement, mutter). Non-empty = addressed to specific people.
  | Say CharacterId [CharacterId] String
    -- | Internal thought attributed to a character.
  | Think CharacterId String
    -- | Narrator prose with no speaker.
  | Narrate String
    -- | Narrator prose picked from a pool by deterministic PRNG.
    -- The salt + world clock tick select which variant fires.
  | NarratePool Int [String]
    -- | Move a character to a new 'Location'.
  | SetLocation CharacterId Location
    -- | Executes @inner@ for the effect's lifetime, then spawns @child@ on
    -- expiry. Use 'Engine.Author.DSL.ifItPersists' for condition-gated chains.
  | OnExpire EffectBody Effect
    -- | Rotates through @bodies@ every @interval@ ticks.
    -- @interval@ must be >= 1. Use 'Engine.Author.DSL.effectCycleMany' for
    -- convenient construction.
  | CycleMany Int (NonEmpty EffectBody)
    -- | Alternates between @body1@ and @body2@ every @interval@ ticks.
    -- @interval@ must be >= 1. Use 'Engine.Author.DSL.effectCycle' for
    -- convenient construction.
  | Cycle     Int EffectBody EffectBody
    -- | A sequence of dialogue lines rendered as a block.
    -- Each triple: (speaker, listeners, text).
  | Dialogue (NonEmpty (CharacterId, [CharacterId], String))
    -- | Move character to a random location from the given list.
    -- Salt + Lamport clock determines the choice.
  | SetLocationRandom CharacterId Int [Location]
    -- | Move character to a random adjacent location (reads worldLocationGraph).
    -- Salt + Lamport clock determines the choice.
  | SetLocationAdjacent CharacterId Int
    -- | Move character to a random adjacent location, preferring locations in
    -- the given region. Salt + Lamport clock determines the choice.
  | SetLocationAdjacentPrefer CharacterId Int Region
    -- | Append a line to the player's journal.  Renders no prose — the
    -- entry becomes visible when the player opens the journal.  Carried
    -- on the event log so it persists across sessions and replays.
  | JournalEntry String
    -- | Increment 'worldDayNumber' by one.  Scenarios emit this from
    -- a day-rollover handler after any day-ending event; the engine
    -- keeps no other notion of "day" so authors stay in control of
    -- when one day becomes the next.
  | AdvanceDay
    -- | No-op placeholder; useful as a default or in conditional branches.
  | DoNothing
  deriving (Show, Eq, Generic)

data Effect = Effect
  { effectBody      :: EffectBody
  , effectLifetime  :: Maybe Int
  , effectCondition :: Condition
  , effectNarrative :: Maybe String
  } deriving (Show, Eq, Generic)

data LiveEffect = LiveEffect
  { liveId         :: UUID
  , liveEffect     :: Effect
  , liveBirthClock :: LamportClock
  } deriving (Show, Eq, Generic)

-- ---------------------------------------------------------------------------
-- Actions
-- ---------------------------------------------------------------------------

data Frequency = Once | Repeatable
  deriving (Show, Eq, Ord)

data Action (f :: Frequency) = Action
  { actionId        :: ActionId
  , actionLabel     :: String
  , actionTarget    :: Maybe Entity
  , actionCondition :: Condition
  , actionEffects   :: [Effect]
  } deriving (Show, Eq, Generic)

data AnyAction where
  AnyAction :: Action f -> AnyAction

instance Show AnyAction where
  show (AnyAction a) = show a

instance Eq AnyAction where
  AnyAction a == AnyAction b =
    actionId a == actionId b && actionLabel a == actionLabel b
    && actionTarget a == actionTarget b
    && actionCondition a == actionCondition b && actionEffects a == actionEffects b

-- ---------------------------------------------------------------------------
-- World diff
-- ---------------------------------------------------------------------------

data StatDelta = StatDelta
  { statDeltaChar   :: CharacterId
  , statDeltaStat   :: StatType
  , statDeltaOld    :: Int
  , statDeltaNew    :: Int
  , statDeltaPlayer :: PlayerId
  } deriving (Show, Eq, Generic)

data RelationDelta = RelationDelta
  { relationDeltaFrom   :: CharacterId
  , relationDeltaTo     :: CharacterId
  , relationDeltaStat   :: StatType
  , relationDeltaOld    :: Int
  , relationDeltaNew    :: Int
  , relationDeltaPlayer :: PlayerId
  } deriving (Show, Eq, Generic)

data LocationDelta = LocationDelta
  { locationDeltaChar :: CharacterId
  , locationDeltaFrom :: Location
  , locationDeltaTo   :: Location
  } deriving (Show, Eq, Generic)

data WorldDiff = WorldDiff
  { diffStats            :: [StatDelta]
  , diffRelations        :: [RelationDelta]
  , diffTagsAdded        :: [(CharacterId, Tag)]
  , diffTagsRemoved      :: [(CharacterId, Tag)]
  , diffWorldTagsAdded   :: [Tag]
  , diffWorldTagsRemoved :: [Tag]
  , diffLocations        :: [LocationDelta]
  , diffJournal          :: [String]
    -- ^ Journal lines newly appended during this step, oldest-first.
    -- Carried so replay and session load reconstruct 'worldJournal'.
  , diffDayDelta         :: Int
    -- ^ Change in 'worldDayNumber' during this step.  Almost always
    -- 0 or 1, driven by day-rollover effects.
  } deriving (Show, Eq, Generic)

-- ---------------------------------------------------------------------------
-- Axioms
-- ---------------------------------------------------------------------------

data AxiomId
  = SystemAxiom  String    -- ^ Engine-owned axiom (runs in every scenario)
  | ScenarioAxiom String   -- ^ Scenario-authored axiom
  deriving (Show, Read, Eq, Ord, Generic)

data Axiom = Axiom
  { axiomId       :: AxiomId
  , axiomPriority :: Int
  , axiomEvaluate :: GameWorld -> [AnyAction] -> WorldDiff -> [Effect]
  }

-- ---------------------------------------------------------------------------
-- Merge causality
-- ---------------------------------------------------------------------------

-- | Whether the origin of a merged change knew about our state.
data Provenance = Aware | Unaware | Stale
  deriving (Show, Read, Eq, Ord, Generic)

-- | A single delta annotated with who caused it and whether they knew about us.
data MergeDelta a = MergeDelta
  { mdValue      :: a
  , mdOrigin     :: PlayerId
  , mdProvenance :: Provenance
  } deriving (Show, Eq, Generic)

-- | What changed as a result of absorbing foreign state, with provenance.
data MergeDiff = MergeDiff
  { mergeStats     :: [MergeDelta StatDelta]
  , mergeRelations :: [MergeDelta RelationDelta]
  , mergeTags      :: [MergeDelta (CharacterId, Tag)]
  , mergeWorldTags :: [MergeDelta Tag]
  , mergeLocations :: [MergeDelta LocationDelta]
  } deriving (Show, Eq, Generic)

-- | Axiom that fires once per merge (not per tick).
data MergeAxiom = MergeAxiom
  { mergeAxiomId       :: AxiomId
  , mergeAxiomPriority :: Int
  , mergeAxiomEvaluate :: GameWorld -> MergeDiff -> [Effect]
  }

-- ---------------------------------------------------------------------------
-- Declarative axiom rules (serializable)
-- ---------------------------------------------------------------------------

-- | Sentinel CharacterId for rules that target multiple characters.
-- The rule evaluator substitutes this with the actual CharacterId.
self :: CharacterId
self = Named "\xa7self"

data Trigger
  = WhenTagAdded Tag
  | WhenWorldTagAdded Tag
  | WhenStatChanged StatType
  | WhenRelationChanged StatType
  | WhenLocationChanged
  | EveryTick
  deriving (Show, Eq, Generic)

data Target
  = EachCharacter
  | SpecificChar CharacterId
  | ChangedChars
  | CoLocatedWith CharacterId
  | CharsAtLocation Location
  deriving (Show, Eq, Generic)

data AxiomRule = AxiomRule
  { ruleId       :: AxiomId
  , rulePriority :: Int
  , ruleTrigger  :: Trigger
  , ruleGuard    :: Condition
  , ruleTarget   :: Target
  , ruleEffects  :: [Effect]
  } deriving (Show, Eq, Generic)

data MergeTrigger
  = WhenMergeRelationChanged
  | WhenMergeLocationChanged
  | WhenMergeTagChanged
  | WhenMergeWorldTagChanged
  | OnAnyMerge
  deriving (Show, Eq, Generic)

data MergeAxiomRule = MergeAxiomRule
  { mergeRuleId         :: AxiomId
  , mergeRulePriority   :: Int
  , mergeRuleTrigger    :: MergeTrigger
  , mergeRuleProvenance :: Maybe Provenance
  , mergeRuleGuard      :: Condition
  , mergeRuleEffects    :: [Effect]
  } deriving (Show, Eq, Generic)

-- ---------------------------------------------------------------------------
-- Snapshots
-- ---------------------------------------------------------------------------

data Snapshot = Snapshot
  { snapWorld      :: GameWorld
  , snapOffset     :: Int
  , snapActions    :: [AnyAction]
  , snapRules      :: [AxiomRule]
  , snapMergeRules :: [MergeAxiomRule]
  } deriving (Generic)

-- ---------------------------------------------------------------------------
-- LogStore abstraction
-- ---------------------------------------------------------------------------

-- | Abstract log persistence. The engine writes entries through this interface;
-- the concrete implementation decides where they go (filesystem, network, memory).
data LogStore = LogStore
  { lsAppend      :: LogEntry -> IO ()                            -- ^ persist one entry
  , lsLoadOwn     :: IO [LogEntry]                                -- ^ load this player's log
  , lsForeignLogs :: IO [(PlayerId, [LogEntry], Maybe Snapshot)]  -- ^ discover and load other players' logs + snapshots
  , lsLoadSnap    :: IO (Maybe Snapshot)                          -- ^ load this player's snapshot
  , lsSaveSnap    :: Snapshot -> IO ()                             -- ^ save this player's snapshot
  , lsReset       :: IO ()                                         -- ^ delete log and snapshot (for --new-session)
  }

-- ---------------------------------------------------------------------------
-- Scenarios
-- ---------------------------------------------------------------------------

data DebugMode = Off | Before | After | Diff | Learning deriving (Eq)

data AxiomTrace = AxiomTrace
  { traceAxiomId  :: AxiomId
  , tracePriority :: Int
  , traceEffects  :: [Effect]
  } deriving (Show, Generic)

data Scenario = Scenario
  { scenarioName         :: String
  , scenarioInitial      :: GameWorld
  , scenarioActions      :: [AnyAction]
  , scenarioAxioms       :: [Axiom]
  , scenarioMergeAxioms  :: [MergeAxiom]
  , scenarioRules        :: [AxiomRule]
  , scenarioMergeRules   :: [MergeAxiomRule]
  , scenarioTerminal     :: Condition
  , scenarioDebugDefault :: DebugMode
  , scenarioPlayerCharId :: CharacterId
  , scenarioTombstoneGC  :: Maybe TombstoneGCRule
    -- ^ Optional per-scenario cleanup rule for ORSet tombstones.
    -- 'Nothing' keeps every tombstone forever (the conservative
    -- default — a scenario with no opinion on lifetime state should
    -- never surprise the player by \"forgetting\" something).  Set
    -- this to @Just (olderThanDays 365)@ (or any 'TombstoneGCRule')
    -- to let the runtime drop old tombstones at merge/snapshot
    -- boundaries.  See 'Engine.CRDT.TombstoneGC'.
  }

-- | A cleanup rule for ORSet tombstones.  First argument is the
-- current wall-clock time; second is the tombstone's minted-at
-- time.  Returning 'True' drops the tombstone on the next GC sweep.
-- Defined here (rather than in 'Engine.CRDT.TombstoneGC') so the
-- 'Scenario' record can reference it without a module cycle.
type TombstoneGCRule = UTCTime -> UTCTime -> Bool

-- ---------------------------------------------------------------------------
-- NFData instances (via Generic, for benchmarking)
-- ---------------------------------------------------------------------------

instance NFData PlayerId
instance NFData LamportClock
instance NFData CharacterId
instance NFData Entity
instance NFData ClockTag
instance NFData ActionId
instance NFData WeatherDesc
instance NFData FatigueLevel
instance NFData HungerLevel
instance NFData SocialEnergyLevel
instance NFData EngineTag
instance NFData ScenarioTagValue
instance NFData Tag
instance NFData Location
instance NFData Region
instance NFData LocationGraph
instance NFData Narration
instance NFData CapacityStat
instance NFData StatType
instance NFData Relationship
instance NFData Character
instance NFData Condition
instance NFData EffectBody
instance NFData Effect
instance NFData LiveEffect
instance NFData GameWorld
instance NFData AxiomId
instance NFData AxiomTrace
instance NFData (Action f)
instance NFData AnyAction where
  rnf (AnyAction a) = rnf a
instance NFData StatDelta
instance NFData RelationDelta
instance NFData LocationDelta
instance NFData WorldDiff
instance NFData LogEntry
instance NFData Provenance
instance NFData a => NFData (MergeDelta a)
instance NFData MergeDiff
instance NFData Trigger
instance NFData Target
instance NFData AxiomRule
instance NFData MergeTrigger
instance NFData MergeAxiomRule
instance NFData Snapshot
