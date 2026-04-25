module Scenarios.Diner.Constants where

import qualified Data.Map.Strict as Map
import           Data.List.NonEmpty (NonEmpty(..))
import           Engine.Author.DSL
import           Engine.Core.World    (addRelationship, mkRelationship, setCharacterStat)
import           GameTypes

-- ---------------------------------------------------------------------------
-- Characters
-- ---------------------------------------------------------------------------

visitor :: CharId
visitor = Named "visitor"

maya :: CharId
maya = Named "maya"

frank :: CharId
frank = Named "frank"

-- ---------------------------------------------------------------------------
-- Locations
-- ---------------------------------------------------------------------------

counter :: Location
counter = Location "Diner: Counter"

booth :: Location
booth = Location "Diner: Booth"

outside :: Location
outside = Location "Diner: Parking Lot"

-- ---------------------------------------------------------------------------
-- Tags
-- ---------------------------------------------------------------------------

data DinerTag
  = OrderedCoffee
  | MayaOpened
  | FrankOpened
  | DawnArrived
  | Restless
  | Settled
  | SmallKindness
  | WorryInTheWalls
  | LateNightConfession
  | QuietPresence
  | VisitorDawn
  | MayaDawn
  | CheckedOnKid
  | NoticedVisitor
  | FrankChatted
  deriving (Show, Eq, Ord, Enum, Bounded)

-- Progression
orderedCoffee :: Tag
orderedCoffee = scenarioTag OrderedCoffee

mayaOpened :: Tag
mayaOpened = scenarioTag MayaOpened

frankOpened :: Tag
frankOpened = scenarioTag FrankOpened

dawnArrived :: Tag
dawnArrived = scenarioTag DawnArrived

-- Mood (visitor)
restless :: Tag
restless = scenarioTag Restless

settled :: Tag
settled = scenarioTag Settled

-- Ambient (world qualities, not scenario-specific)
smallKindness :: Tag
smallKindness = scenarioTag SmallKindness

worryInTheWalls :: Tag
worryInTheWalls = scenarioTag WorryInTheWalls

lateNightConfession :: Tag
lateNightConfession = scenarioTag LateNightConfession

quietPresence :: Tag
quietPresence = scenarioTag QuietPresence

-- Per-player completion
visitorDawn :: Tag
visitorDawn = scenarioTag VisitorDawn

mayaDawn :: Tag
mayaDawn = scenarioTag MayaDawn

-- Maya-specific
checkedOnKid :: Tag
checkedOnKid = scenarioTag CheckedOnKid

noticedVisitor :: Tag
noticedVisitor = scenarioTag NoticedVisitor

frankChatted :: Tag
frankChatted = scenarioTag FrankChatted

-- ---------------------------------------------------------------------------
-- Clock & weather
-- ---------------------------------------------------------------------------

ticksPerHour :: Int
ticksPerHour = 2

ticksPerDay :: Int
ticksPerDay = ticksPerHour * 24

-- | Time cycle starting at 2 AM — the middle of the night.
timeCycle :: Effect
timeCycle = effectCycleMany ticksPerHour
  (AddWorldTag (timeTag 2) :| [ AddWorldTag (timeTag h) | h <- [3..23] ++ [0..1] ])

-- | Weather: rainy night that gradually clears toward dawn.
weatherCycle :: Effect
weatherCycle = effectCycleMany (ticksPerHour * 2)
  (AddWorldTag (weatherTag (WeatherDesc "Rainy")) :|
   [ AddWorldTag (weatherTag w) | w <- drop 1 weatherSequence ])

weatherSequence :: [WeatherDesc]
weatherSequence =
  [ WeatherDesc "Rainy"
  , WeatherDesc "Drizzle"
  , WeatherDesc "Overcast"
  , WeatherDesc "Clearing"
  ]

-- ---------------------------------------------------------------------------
-- Initial world (shared between visitor and Maya scenarios)
-- ---------------------------------------------------------------------------

initialGraph :: RelationshipGraph
initialGraph
  = addRelationship visitor maya  (mkRelationship Trust 0) (mkRelationship Trust 2)
  . addRelationship visitor frank (mkRelationship Trust 0) (mkRelationship Trust 0)
  . addRelationship maya   frank  (mkRelationship Trust 4) (mkRelationship Trust 3)
  . setCharacterStat visitor (Capacity Intelligence)  5
  . setCharacterStat visitor (Capacity Strength)      3   -- already tired, it's 2 AM
  . setCharacterStat visitor (Capacity Charisma)      5
  . setCharacterStat visitor (Capacity Understanding) 5
  . setCharacterStat visitor (Capacity Hunger)        4
  . setCharacterStat maya    (Capacity Intelligence)  5
  . setCharacterStat maya    (Capacity Strength)      3   -- end of a long shift
  . setCharacterStat maya    (Capacity Charisma)      7   -- people person
  . setCharacterStat maya    (Capacity Understanding) 6
  . setCharacterStat frank   (Capacity Intelligence)  6
  . setCharacterStat frank   (Capacity Strength)      4
  . setCharacterStat frank   (Capacity Charisma)      3   -- withdrawn
  . setCharacterStat frank   (Capacity Understanding) 7   -- seen a lot
  $ Map.empty

initialWorld :: Int -> GameWorld
initialWorld seed = GameWorld
  { worldCharacters = Map.fromList
      [ (visitor, Character visitor "You"   [] emptyTags)
      , (maya,    Character maya    "Maya"  [] emptyTags)
      , (frank,   Character frank   "Frank" [] emptyTags)
      ]
  , worldGraph         = initialGraph
  , worldLocations     = Map.fromList
      [ (visitor, booth)
      , (maya,    counter)
      , (frank,   counter)
      ]
  , worldActiveEffects = map staticLive [timeCycle, weatherCycle]
  , worldClock         = LamportClock 0 (PlayerId "init")
  , worldTags          = tagsFromList
      [ weatherTag (WeatherDesc "Rainy")
      , seasonTag 2              -- Autumn
      , dayOfWeekTag 3           -- Wednesday
      , lunarPhaseTag 20         -- Waning gibbous
      , dayNumberTag 0
      , timeTag 2                -- 2 AM
      , restless                 -- visitor is restless
      ]
  , worldLocationGraph = emptyLocationGraph
  , worldSeed          = seed
  , worldLocationHistory = Map.empty
  , worldLocationVisits  = Map.empty
  , worldJournal         = []
  , worldDayNumber       = 1
  }
