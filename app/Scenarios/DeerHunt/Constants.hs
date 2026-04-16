module Scenarios.DeerHunt.Constants where

import           Data.Maybe          (fromMaybe)
import qualified Data.Map.Strict as Map
import           Data.List.NonEmpty (NonEmpty(..))
import           Engine.Author.DSL
import           Engine.Author.Random   (rollChoice)
import           System.Random          (mkStdGen, randomR)
import           Engine.CRDT.ORSet
import           Engine.Core.World      (setCharacterStat)
import           GameTypes
import           Scenarios.DeerHunt.Locations

-- ---------------------------------------------------------------------------
-- Characters
-- ---------------------------------------------------------------------------

deer :: CharId
deer = Named "deer"

-- ---------------------------------------------------------------------------
-- Tags
-- ---------------------------------------------------------------------------

data DeerHuntTag
  = DeerKilled
  | HunterShot
  | DeerGone
  | DeerSpooked
  | DeerSpotted
  | FreshSign
  | MovingFast
  | ShotTaken
  | NightFall
  | BackAtTruck
  | DayTwo
  | DayThree
  | WindAngle Int        -- ^ Wind direction in hundredths of degrees (0–36000)
  | WindStrength Int     -- ^ Wind strength in hundredths (0–100, maps to 0.0–1.0)
  | PlayerSitting        -- ^ Player is sitting (toggle state)
  | SignTracks           -- ^ Fresh tracks at a location
  | SignBed              -- ^ Bedding site found
  | SignRub              -- ^ Antler rub found (repeated visits)
  | SignScrape           -- ^ Ground scrape (very recent activity)
  | FoundSignTracks      -- ^ First time finding tracks (experience gate)
  | FoundSignBed         -- ^ First time finding a bed
  | FoundSignRub         -- ^ First time finding a rub
  | FoundSignScrape      -- ^ First time finding a scrape
  deriving (Show, Eq, Ord)

deerKilled :: Tag
deerKilled = scenarioTag DeerKilled

hunterShot :: Tag
hunterShot = scenarioTag HunterShot

deerGone :: Tag
deerGone = scenarioTag DeerGone

deerSpooked :: Tag
deerSpooked = scenarioTag DeerSpooked

deerSpotted :: Tag
deerSpotted = scenarioTag DeerSpotted

freshSign :: Tag
freshSign = scenarioTag FreshSign

movingFast :: Tag
movingFast = scenarioTag MovingFast

shotTaken :: Tag
shotTaken = scenarioTag ShotTaken

nightFall :: Tag
nightFall = scenarioTag NightFall

backAtTruck :: Tag
backAtTruck = scenarioTag BackAtTruck

dayTwo :: Tag
dayTwo = scenarioTag DayTwo

dayThree :: Tag
dayThree = scenarioTag DayThree

playerSitting :: Tag
playerSitting = scenarioTag PlayerSitting

signTracks :: Tag
signTracks = scenarioTag SignTracks

signBed :: Tag
signBed = scenarioTag SignBed

signRub :: Tag
signRub = scenarioTag SignRub

signScrape :: Tag
signScrape = scenarioTag SignScrape

foundSignTracks :: Tag
foundSignTracks = scenarioTag FoundSignTracks

foundSignBed :: Tag
foundSignBed = scenarioTag FoundSignBed

foundSignRub :: Tag
foundSignRub = scenarioTag FoundSignRub

foundSignScrape :: Tag
foundSignScrape = scenarioTag FoundSignScrape

-- | Encode a wind angle (degrees) as a world tag. Stores hundredths.
windAngleTag :: Double -> Tag
windAngleTag deg = scenarioTag (WindAngle (round (deg * 100)))

-- | Encode a wind strength (0.0–1.0) as a world tag. Stores hundredths.
windStrengthTag :: Double -> Tag
windStrengthTag s = scenarioTag (WindStrength (round (s * 100)))

-- | Check if a tag is a wind angle tag.
isWindAngleTag :: Tag -> Bool
isWindAngleTag (ScenarioTag (MkScenarioTag s)) = take 10 s == "WindAngle "
isWindAngleTag _ = False

-- | Check if a tag is a wind strength tag.
isWindStrengthTag :: Tag -> Bool
isWindStrengthTag (ScenarioTag (MkScenarioTag s)) = take 13 s == "WindStrength "
isWindStrengthTag _ = False

-- | Read the current wind angle (degrees) from world tags.
getWindAngle :: GameWorld -> Double
getWindAngle world =
  let tags = orToList (worldTags world)
  in case [ n | ScenarioTag (MkScenarioTag s) <- tags
              , take 10 s == "WindAngle "
              , (n, _) <- reads (drop 10 s) :: [(Int, String)] ] of
       (n:_) -> fromIntegral n / 100.0
       []    -> 270.0  -- default: wind from west (blowing east)

-- | Read the current wind strength (0.0–1.0) from world tags.
getWindStrength :: GameWorld -> Double
getWindStrength world =
  let tags = orToList (worldTags world)
  in case [ n | ScenarioTag (MkScenarioTag s) <- tags
              , take 13 s == "WindStrength "
              , (n, _) <- reads (drop 13 s) :: [(Int, String)] ] of
       (n:_) -> fromIntegral n / 100.0
       []    -> 0.2    -- default: light breeze

-- ---------------------------------------------------------------------------
-- Clock
-- ---------------------------------------------------------------------------

-- | Each tick = 5 minutes of game time.
-- Legal shooting: 7 AM to 7 PM = 144 ticks per day.
ticksPerHour :: Int
ticksPerHour = 12

ticksPerDay :: Int
ticksPerDay = ticksPerHour * 24

timeCycle :: Effect
timeCycle = effectCycleMany ticksPerHour
  (AddWorldTag (timeTag 7) :| [ AddWorldTag (timeTag h) | h <- [8..23] ++ [0..6] ])

weatherCycle :: Effect
weatherCycle = effectCycleMany ticksPerDay
  (case [ AddWorldTag (weatherTag w) | w <- weatherSequence ] of
     (x:xs) -> x :| xs
     []     -> AddWorldTag (weatherTag (WeatherDesc "Clear")) :| [])

weatherSequence :: [WeatherDesc]
weatherSequence =
  [ WeatherDesc "Clear and Cold"
  , WeatherDesc "Overcast"
  , WeatherDesc "Light Snow"
  , WeatherDesc "Clear and Cold"
  , WeatherDesc "Windy"
  , WeatherDesc "Overcast"
  , WeatherDesc "Clear and Cold"
  ]

-- ---------------------------------------------------------------------------
-- Random salts — each use-site gets a unique salt for independent rolls
-- ---------------------------------------------------------------------------

saltDeerMove :: Int
saltDeerMove = 1

saltSpook :: Int
saltSpook = 2

saltShot :: Int
saltShot = 3

saltFriendlyFire :: Int
saltFriendlyFire = 4

saltDeerZone :: Int
saltDeerZone = 5

saltWindDrift :: Int
saltWindDrift = 6

saltWindGust :: Int
saltWindGust = 7

saltSignPlacement :: Int
saltSignPlacement = 8

-- ---------------------------------------------------------------------------
-- World queries
-- ---------------------------------------------------------------------------

charLocation :: CharId -> GameWorld -> Maybe Location
charLocation cid world = Map.lookup cid (worldLocations world)

coLocated :: CharId -> CharId -> GameWorld -> Bool
coLocated a b world = case (charLocation a world, charLocation b world) of
  (Just la, Just lb) -> la == lb
  _                  -> False

coLocatedHunters :: CharId -> GameWorld -> [CharId]
coLocatedHunters you world =
  [ cid | (cid, _) <- Map.toList (worldCharacters world)
        , cid /= you
        , cid /= deer
        , cid /= Truth
        , coLocated you cid world
        ]

currentHour :: GameWorld -> Maybe Int
currentHour world =
  let tags = orToList (worldTags world)
  in case [ h | EngineTag (Clock (TimeOfDay h)) <- tags ] of
       (h:_) -> Just h
       []    -> Nothing

-- | Preferred zone for the deer based on time of day.
deerPreferredZone :: GameWorld -> Zone
deerPreferredZone world = case currentHour world of
  Just h
    | h >= 7  && h < 9  -> NorthField     -- early morning: feeding
    | h >= 9  && h < 11 -> BushEdge       -- mid-morning: heading for cover
    | h >= 11 && h < 14 -> OakRidge       -- midday: bedded down
    | h >= 14 && h < 17 -> PoplarStand    -- afternoon: moving again
    | h >= 17 && h < 19 -> SouthField     -- evening: feeding
    | otherwise          -> OakRidge       -- night: bedded
  Nothing -> NorthField

-- | Pick a destination node for the deer's next move.
deerNextLocation :: GameWorld -> Location
deerNextLocation world =
  let current   = fromMaybe stubbleRows (charLocation deer world)
      preferred = deerPreferredZone world
      prefLocs  = zoneLocations preferred
      neighbors = adjacentTo current
      -- Bias toward preferred zone: if any neighbor is in preferred zone, pick from those
      prefNeighbors = filter (\l -> locationZone l == preferred) neighbors
      candidates | not (null prefNeighbors) = prefNeighbors
                 | not (null neighbors)     = neighbors
                 | otherwise                = prefLocs  -- shouldn't happen
  in rollChoice world saltDeerMove candidates

-- ---------------------------------------------------------------------------
-- Initial world
-- ---------------------------------------------------------------------------

initialGraph :: CharId -> RelationshipGraph
initialGraph you
  = setCharacterStat you  (Capacity Intelligence)  5
  . setCharacterStat you  (Capacity Strength)      6
  . setCharacterStat you  (Capacity Charisma)      4
  . setCharacterStat you  (Capacity Understanding) 2
  . setCharacterStat you  (Capacity Hunger)        8
  . setCharacterStat deer (Capacity Intelligence)  3
  . setCharacterStat deer (Capacity Strength)      6
  $ Map.empty

-- | Pick the deer's starting location from the session seed.
-- Deep bush only — oak ridge, willow bottom, poplar stand.
deerStartFromSeed :: Int -> Location
deerStartFromSeed seed =
  let candidates = zoneLocations OakRidge ++ zoneLocations WillowBottom ++ zoneLocations PoplarStand
      (idx, _) = randomR (0, length candidates - 1) (mkStdGen seed)
  in candidates !! idx

initialWorld :: Int -> CharId -> Location -> GameWorld
initialWorld seed you startTruck = GameWorld
  { worldCharacters = Map.fromList
      [ (you,  Character you  "You"      [] orEmpty)
      , (deer, Character deer "The Deer" [] orEmpty)
      ]
  , worldGraph         = initialGraph you
  , worldLocations     = Map.fromList
      [ (you,  startTruck)
      , (deer, deerStartFromSeed seed)
      ]
  , worldActiveEffects = map staticLive [timeCycle, weatherCycle]
  , worldClock         = LamportClock 0 (PlayerId "init")
  , worldTags          = orFromList
      [ weatherTag  (WeatherDesc "Clear and Cold")
      , seasonTag   3
      , dayOfWeekTag 5
      , lunarPhaseTag 0
      , dayNumberTag  0
      , timeTag 7
      , windAngleTag 270.0     -- initial wind from the west
      , windStrengthTag 0.2    -- light morning breeze
      ]
  , worldLocationGraph = huntLocationGraph
  , worldSeed          = seed
  }

