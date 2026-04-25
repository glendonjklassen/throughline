module Scenarios.TopBuy.Constants where

import qualified Data.Map.Strict as Map
import           Data.List.NonEmpty (NonEmpty(..))
import           Engine.Author.DSL
import           Engine.Core.World    (addRelationship, mkRelationship, setCharacterStat)
import           GameTypes
import           Scenarios.TopBuy.Locations

-- ---------------------------------------------------------------------------
-- Characters
-- ---------------------------------------------------------------------------

bradley :: CharId
bradley = Named "bradley"

kyle :: CharId
kyle = Named "kyle"

-- ---------------------------------------------------------------------------
-- Tags
-- ---------------------------------------------------------------------------

data TopBuyTag
  = BradleySmallAsk
  | BradleyBigAsk
  | KyleInvestigating
  | BradleyAsking
  | PlayerSuspecting
  | InventoryDiscrepancy
  | ReportedToKyle
  | LoggedReturnForBradley
  | CoveredForBradley
  | PhoneOut
  | ScrollingPhone
  | CustomerAssisted
  | BradleySucceeded
  | PlayerSuspended
  | PlayerCleared
  deriving (Show, Eq, Ord, Enum, Bounded)

-- Progression flags
bradleySmallAsk :: Tag
bradleySmallAsk = scenarioTag BradleySmallAsk

bradleyBigAsk :: Tag
bradleyBigAsk = scenarioTag BradleyBigAsk

kyleInvestigating :: Tag
kyleInvestigating = scenarioTag KyleInvestigating

-- Intermediate state flags (set before dialogue fires; gate player responses)
bradleyAsking :: Tag
bradleyAsking = scenarioTag BradleyAsking

-- Player-choice flags
playerSuspecting :: Tag
playerSuspecting = scenarioTag PlayerSuspecting

inventoryDiscrepancy :: Tag
inventoryDiscrepancy = scenarioTag InventoryDiscrepancy

reportedToKyle :: Tag
reportedToKyle = scenarioTag ReportedToKyle

loggedReturnForBradley :: Tag
loggedReturnForBradley = scenarioTag LoggedReturnForBradley

coveredForBradley :: Tag
coveredForBradley = scenarioTag CoveredForBradley

phoneOut :: Tag
phoneOut = scenarioTag PhoneOut

scrollingPhone :: Tag
scrollingPhone = scenarioTag ScrollingPhone

-- ---------------------------------------------------------------------------
-- Locations
-- ---------------------------------------------------------------------------

home :: Location
home = Location "Home"

-- Co-location interaction flags
customerAssisted :: Tag
customerAssisted = scenarioTag CustomerAssisted

-- Outcome flags
bradleySucceeded :: Tag
bradleySucceeded = scenarioTag BradleySucceeded

playerSuspended :: Tag
playerSuspended = scenarioTag PlayerSuspended

playerCleared :: Tag
playerCleared = scenarioTag PlayerCleared

-- ---------------------------------------------------------------------------
-- Clock & calendar
-- ---------------------------------------------------------------------------

ticksPerHour :: Int
ticksPerHour = 2

ticksPerDay :: Int
ticksPerDay = ticksPerHour * 24

-- | Cycles the TimeOfDay tag through a full day starting at 9 AM.
-- Each hour persists for ticksPerHour ticks; deduplication ensures only
-- one TimeOfDay tag is active at a time.
timeCycle :: Effect
timeCycle = effectCycleMany ticksPerHour
  (AddWorldTag (timeTag 9) :| [ AddWorldTag (timeTag h) | h <- [10..23] ++ [0..8] ])

-- | Cycles through a sequence of weather states, one per in-game day.
-- Deduplication ensures only one Weather tag is active at a time.
-- The dayAdvanceAxiom narrates changes when the active tag in the diff changes.
weatherCycle :: Effect
weatherCycle = effectCycleMany ticksPerDay
  (case [ AddWorldTag (weatherTag w) | w <- weatherSequence ] of
     (x:xs) -> x :| xs
     []     -> AddWorldTag (weatherTag (WeatherDesc "Clear")) :| [])

weatherSequence :: [WeatherDesc]
weatherSequence =
  [ WeatherDesc "Clear"
  , WeatherDesc "Clear"
  , WeatherDesc "Partly Cloudy"
  , WeatherDesc "Overcast"
  , WeatherDesc "Light Rain"
  , WeatherDesc "Overcast"
  , WeatherDesc "Partly Cloudy"
  , WeatherDesc "Clear"
  , WeatherDesc "Windy"
  , WeatherDesc "Clear"
  , WeatherDesc "Clear"
  , WeatherDesc "Partly Cloudy"
  , WeatherDesc "Stormy"
  , WeatherDesc "Overcast"
  , WeatherDesc "Clear"
  ]

-- ---------------------------------------------------------------------------
-- Hunger
-- ---------------------------------------------------------------------------


-- ---------------------------------------------------------------------------
-- Initial world
-- ---------------------------------------------------------------------------

-- | Builds the initial relationship graph including:
-- * Bidirectional you↔bradley trust (starting value 3)
-- * Ground truth stats for all three characters (Truth → char → stat)
initialGraph :: CharId -> RelationshipGraph
initialGraph you
  = addRelationship you bradley (mkRelationship Trust 3) (mkRelationship Trust 3)
  . setCharacterStat you     (Capacity Intelligence)  5
  . setCharacterStat you     (Capacity Strength)      5
  . setCharacterStat you     (Capacity Charisma)      5
  . setCharacterStat you     (Capacity Understanding) 4  -- below perception threshold; must be earned
  . setCharacterStat you     (Capacity Hunger)        8  -- starts full; depletes hourly on shift
  . setCharacterStat bradley (Capacity Intelligence)  5
  . setCharacterStat bradley (Capacity Strength)      5
  . setCharacterStat bradley (Capacity Charisma)      7  -- likeable; that's how he gets away with things
  . setCharacterStat bradley (Capacity Understanding) 4
  . setCharacterStat kyle    (Capacity Intelligence)  7
  . setCharacterStat kyle    (Capacity Strength)      5
  . setCharacterStat kyle    (Capacity Charisma)      6
  . setCharacterStat kyle    (Capacity Understanding) 8  -- he's seen this before
  $ Map.empty

initialWorld :: Int -> CharId -> GameWorld
initialWorld seed you = GameWorld
  { worldCharacters = Map.fromList
      [ (you,     Character you     "You"     [] emptyTags)
      , (bradley, Character bradley "Bradley" [] emptyTags)
      , (kyle,    Character kyle    "Kyle"    [] emptyTags)
      ]
  , worldGraph         = initialGraph you
  , worldLocations     = Map.fromList
      [ (you,     salesFloor)
      , (bradley, salesFloor)
      , (kyle,    backOffice)
      ]
  , worldActiveEffects = map staticLive [timeCycle, weatherCycle]
  , worldClock         = LamportClock 0 (PlayerId "init")
  , worldTags          = tagsFromList
      [ weatherTag  (WeatherDesc "Clear")   -- initial weather
      , seasonTag   0         -- Spring
      , dayOfWeekTag 0        -- Monday
      , lunarPhaseTag 0       -- New Moon
      , dayNumberTag  0       -- day zero
      ]
  , worldLocationGraph = emptyLocationGraph
  , worldSeed          = seed
  , worldLocationHistory = Map.empty
  , worldLocationVisits  = Map.empty
  , worldJournal         = []
  , worldDayNumber       = 1
  }
