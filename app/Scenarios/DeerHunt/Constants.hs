module Scenarios.DeerHunt.Constants where

import           Data.List            (isPrefixOf, nub)
import           Data.Maybe           (fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Time.Calendar (Day, DayOfWeek(..), addDays, dayOfWeek,
                                     fromGregorian, toGregorian)
import           Engine.Author.DSL
import           Engine.Author.Random   (rollChoice)
import           System.Random          (mkStdGen, randomR)
import           Engine.CRDT.ORSet
import           Engine.Core.World      (setCharacterStat)
import           GameTypes
import           Scenarios.DeerHunt.Generation (GeneratedMap(..), TerrainClass(..))
import           Scenarios.DeerHunt.World      (HuntWorld(..), hwClass, hwLocsOfClass, hwStart, hwDeerStart)

-- ---------------------------------------------------------------------------
-- Calendar
-- ---------------------------------------------------------------------------

-- | Opening day of rifle season on the Saskatchewan prairie — the
-- hunt's in-world anchor date.  Changing this shifts every day
-- marker in the journal so the season still reads as contiguous.
-- The year is chosen so the weekday order reads right; nothing in
-- the game depends on it being the current year.
huntStartDate :: Day
huntStartDate = fromGregorian 2024 11 7

-- | Calendar date for the @n@'th day of the hunt (1-indexed).
huntDayDate :: Int -> Day
huntDayDate n = addDays (fromIntegral (max 0 (n - 1))) huntStartDate

-- | Short journal-style date label for the @n@'th day — e.g.
-- \"Thu, Nov 7\".  Used for notebook day headers, the day-end
-- transition overlay, and each discovery's first-seen stamp in the
-- index.  The weekday makes the passage of time feel lived-in; the
-- terse month abbreviation keeps headers from crowding a narrow
-- viewport.
formatHuntDate :: Int -> String
formatHuntDate n =
  let d = huntDayDate n
      (_y, m, dom) = toGregorian d
  in dowShort (dayOfWeek d) <> ", " <> monthShort m <> " " <> show dom

dowShort :: DayOfWeek -> String
dowShort dow = case dow of
  Monday    -> "Mon"
  Tuesday   -> "Tue"
  Wednesday -> "Wed"
  Thursday  -> "Thu"
  Friday    -> "Fri"
  Saturday  -> "Sat"
  Sunday    -> "Sun"

monthShort :: Int -> String
monthShort m = case m of
  1  -> "Jan"; 2  -> "Feb"; 3  -> "Mar"; 4  -> "Apr"
  5  -> "May"; 6  -> "Jun"; 7  -> "Jul"; 8  -> "Aug"
  9  -> "Sep"; 10 -> "Oct"; 11 -> "Nov"; 12 -> "Dec"
  _  -> "?"

-- ---------------------------------------------------------------------------
-- Characters
-- ---------------------------------------------------------------------------

deer :: CharId
deer = Named "deer"

-- ---------------------------------------------------------------------------
-- Tags
-- ---------------------------------------------------------------------------

-- | Day-scoped tags — cleared by the day-rollover axiom.  If a state
-- belongs here, 'allDayScopedTags' automatically picks it up; no more
-- hand-listing in the rollover.
data DeerHuntDayTag
  = DeerKilled
  | DeerGone
  | DeerSpooked
  | DeerSpotted
  | FreshSign
  | MovingFast
  | ShotTaken
  | NightFall
  | BackAtTruck
  | PlayerSitting
  | SignTracks           -- ^ Fresh tracks at a location
  | SignBed              -- ^ Bedding site found
  | SignRub              -- ^ Antler rub found (repeated visits)
  | SignScrape           -- ^ Ground scrape (very recent activity)
  | DayOver              -- ^ Current day is ending; triggers rollover axiom
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Season-scoped tags — persist across day rollovers.  Things the
-- hunter has learned or accumulated over the run, plus terminal
-- states that end the scenario entirely.
data DeerHuntSeasonTag
  = HunterShot
  | SeasonOver
  | DayTwo
  | DayThree
  | FoundSignTracks      -- ^ First time finding tracks (experience gate)
  | FoundSignBed         -- ^ First time finding a bed
  | FoundSignRub         -- ^ First time finding a rub
  | FoundSignScrape      -- ^ First time finding a scrape
  | WindAngle Int        -- ^ Wind direction in hundredths of degrees (0–36000)
  | WindStrength Int     -- ^ Wind strength in hundredths (0–100, maps to 0.0–1.0)
  deriving (Show, Eq, Ord)

-- | Every day-scoped tag, as engine 'Tag' values.  Consumed by the
-- day-rollover axiom to clear per-day state in one sweep.  New
-- day-scoped tags picked up automatically by adding to
-- 'DeerHuntDayTag'.
allDayScopedTags :: [Tag]
allDayScopedTags = map scenarioTag [minBound .. maxBound :: DeerHuntDayTag]

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

dayOver :: Tag
dayOver = scenarioTag DayOver

seasonOver :: Tag
seasonOver = scenarioTag SeasonOver

-- ---------------------------------------------------------------------------
-- Per-location sign tags (string-encoded)
--
-- The legacy signTracks/signScrape/etc. tags above are coarse: they
-- indicate "there is some sign somewhere in the player's zone".  The
-- shiny-sense UI needs to know *where*, so we encode per-location
-- signs as opaque string tags of the form:
--
--   "sign|<type>|<LocationName>"
--
-- and the player's discovery (i.e. "I have looked here and noticed it")
-- as:
--
--   "found|<type>|<LocationName>"
--
-- Multiple sign types can coexist at a location.  Tags decay via the
-- timed-effect TTL mechanism.
-- ---------------------------------------------------------------------------

-- | All sign types the engine tracks per-location.  Each type has a
-- different tell, decay profile, and narrative flavour.
data SignType
  = STracks      -- ^ recent passage, decays fastest
  | SScrape      -- ^ ground torn up, medium decay
  | SBed         -- ^ oval matted in the grass, persists
  | SRub         -- ^ velvet shredded off saplings, persists longest
  | SDroppings   -- ^ droppings, short decay
  | SHair        -- ^ caught on barbed wire or a branch, long-lived
  deriving (Eq, Ord, Show)

-- | Short string id used in tag encoding.
signTypeCode :: SignType -> String
signTypeCode STracks    = "tracks"
signTypeCode SScrape    = "scrape"
signTypeCode SBed       = "bed"
signTypeCode SRub       = "rub"
signTypeCode SDroppings = "droppings"
signTypeCode SHair      = "hair"

-- | Every type in a stable order; useful for iteration.
allSignTypes :: [SignType]
allSignTypes = [STracks, SScrape, SBed, SRub, SDroppings, SHair]

-- | Default decay in ticks for a sign type.  Tweaked at call-sites
-- based on weather (snow accelerates the tracks/droppings fade).
signDuration :: SignType -> Int
signDuration STracks    = 24
signDuration SScrape    = 18
signDuration SBed       = 48
signDuration SRub       = 72
signDuration SDroppings = 12
signDuration SHair      = 96

-- | Encode a per-location sign tag.
signAt :: SignType -> Location -> Tag
signAt t (Location name) =
  ScenarioTag (MkScenarioTag ("sign|" <> signTypeCode t <> "|" <> name))

-- | Encode a player-discovered-sign tag.
foundSignAt :: SignType -> Location -> Tag
foundSignAt t (Location name) =
  ScenarioTag (MkScenarioTag ("found|" <> signTypeCode t <> "|" <> name))

-- | Parse a "sign|type|location" tag into (SignType, Location) if it matches.
parseSignTag :: Tag -> Maybe (SignType, Location)
parseSignTag (ScenarioTag (MkScenarioTag s))
  | "sign|" `isPrefixOf` s = parseAfter (drop 5 s)
  | otherwise              = Nothing
  where
    parseAfter rest = case break (== '|') rest of
      (code, '|':name) -> (,) <$> codeToType code <*> Just (Location name)
      _                -> Nothing
parseSignTag _ = Nothing

-- | Parse a "found|type|location" discovery tag.
parseFoundTag :: Tag -> Maybe (SignType, Location)
parseFoundTag (ScenarioTag (MkScenarioTag s))
  | "found|" `isPrefixOf` s = parseAfter (drop 6 s)
  | otherwise               = Nothing
  where
    parseAfter rest = case break (== '|') rest of
      (code, '|':name) -> (,) <$> codeToType code <*> Just (Location name)
      _                -> Nothing
parseFoundTag _ = Nothing

codeToType :: String -> Maybe SignType
codeToType "tracks"    = Just STracks
codeToType "scrape"    = Just SScrape
codeToType "bed"       = Just SBed
codeToType "rub"       = Just SRub
codeToType "droppings" = Just SDroppings
codeToType "hair"      = Just SHair
codeToType _           = Nothing

-- | All sign types present at a location in the world right now.
signsAt :: GameWorld -> Location -> [SignType]
signsAt world loc =
  nub [ t | tag <- orToList (worldTags world)
          , Just (t, l) <- [parseSignTag tag]
          , l == loc ]

-- | All sign types the player has *discovered* at a location.
foundSignsAt :: GameWorld -> Location -> [SignType]
foundSignsAt world loc =
  nub [ t | tag <- orToList (worldTags world)
          , Just (t, l) <- [parseFoundTag tag]
          , l == loc ]

-- | Has the player discovered any sign anywhere?
hasDiscoveredAnySign :: GameWorld -> Bool
hasDiscoveredAnySign world =
  any (\tag -> case parseFoundTag tag of
         Just _ -> True
         Nothing -> False)
      (orToList (worldTags world))

-- | How strong is the evidence the player *has noticed* at a location?
-- 0 = none; higher with more distinct sign types and rarer types.
discoveredEvidence :: GameWorld -> Location -> Int
discoveredEvidence world loc =
  let found = foundSignsAt world loc
      weight SRub       = 3
      weight SBed       = 3
      weight SHair      = 2
      weight SScrape    = 2
      weight STracks    = 1
      weight SDroppings = 1
  in sum (map weight found)

-- ---------------------------------------------------------------------------
-- Sign hotspots — the "treasure" placement
--
-- Signs are not sprayed wherever the deer walks.  Instead, the world
-- has a small number of hotspot locations decided at scenario init
-- from the session seed.  Each hotspot holds 1-3 sign types, biased
-- by the location's character (the "Rub Line" location is very likely
-- to carry SRub, a moss hollow is likely to carry SBed, etc.).
--
-- Hotspots are persistent for the whole run — they don't decay — and
-- the player discovers them by searching.  This turns the map into a
-- foraging puzzle layered on top of the direct deer-chasing loop.
-- ---------------------------------------------------------------------------

-- | How many hotspot locations to place per run.  With 57 locations,
-- 9 hotspots means the player must search ~15% of the map to hit one.
hotspotCount :: Int
hotspotCount = 9

-- | Locations eligible to carry signs.  Roads and empty cells are
-- excluded (nothing credible happens on a gravel road).  Everything
-- in a cover class or a field is fair game.
hotspotCandidates :: HuntWorld -> [Location]
hotspotCandidates hw =
  [ loc | loc <- gmLocations (hwMap hw)
        , let cls = hwClass hw loc
        , cls /= CRoad && cls /= CEmpty ]

-- | Bias: per terrain class, which sign types are plausible and with
-- what probability weight.  Higher weight = more likely to be chosen.
-- Ridge and bush are prime bedding/rubbing ground; fields are feeding
-- ground (droppings, tracks); creeks are tracks-heavy (mud holds them).
-- Returns a non-empty list of (SignType, weight).
locationSignBias :: HuntWorld -> Location -> [(SignType, Int)]
locationSignBias hw loc = case hwClass hw loc of
  CRidge -> [(SRub, 4), (SScrape, 3), (SBed, 2), (STracks, 2)]
  CBush  -> [(SBed, 4), (SHair, 3), (STracks, 2), (SDroppings, 1)]
  CCreek -> [(STracks, 5), (SDroppings, 2), (SHair, 1)]
  CField -> [(SDroppings, 4), (STracks, 3), (SBed, 1)]
  _      -> [(STracks, 2), (SDroppings, 1)]

-- | Pick N hotspot locations from the candidates using the session seed.
-- Deterministic: same HuntWorld + same seed -> same hotspots.  Uses a
-- linear congruential shuffle to keep the draw independent of any
-- runtime state.
hotspotLocations :: HuntWorld -> [Location]
hotspotLocations hw =
  let seed = hwSeed hw
      gen0 = mkStdGen (seed * 7919 + 113)
      pool = hotspotCandidates hw
      go 0 _ _    = []
      go _ [] _   = []
      go n xs gen =
        let (idx, gen') = randomR (0, length xs - 1) gen
        in case splitAt idx xs of
             (pre, picked : post) -> picked : go (n - 1) (pre ++ post) gen'
             _                    -> []   -- unreachable: idx is in-range
  in go (min hotspotCount (length pool)) pool gen0

-- | For one hotspot, pick 1-3 sign types using the class-keyed bias
-- and a seed derived from the session seed + the location name.
hotspotSigns :: HuntWorld -> Location -> [SignType]
hotspotSigns hw loc =
  let seed    = hwSeed hw
      bias    = locationSignBias hw loc
      salt    = foldl (\acc c -> acc * 33 + fromEnum c) 1 (locationName loc)
      gen0    = mkStdGen (seed * 31 + salt)
      (nSigns, gen1) = randomR (1, 3 :: Int) gen0
      pick remaining g
        | remaining <= 0 || null bias = []
        | otherwise =
            let totalW = sum (map snd bias)
                (r, g') = randomR (0, totalW - 1) g
                chosen  = choose r bias
            in chosen : pick (remaining - 1) g'
      choose _ []           = STracks   -- should not happen, bias is non-empty
      choose r ((t, w):rest)
        | r < w     = t
        | otherwise = choose (r - w) rest
  in nub (pick nSigns gen1)

-- | All initial world tags for sign hotspots for the given HuntWorld.
initialSignTags :: HuntWorld -> [Tag]
initialSignTags hw =
  [ signAt t loc
  | loc <- hotspotLocations hw
  , t   <- hotspotSigns hw loc
  ]

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

-- | Preferred terrain class for the deer based on time of day.  The
-- generator no longer gives us a pinned Zone ADT, so we work in
-- 'TerrainClass' instead.  Behaviour is the same: feed in fields
-- morning and evening, retreat to bush/ridge during the day.
deerPreferredClass :: GameWorld -> TerrainClass
deerPreferredClass world = case currentHour world of
  Just h
    | h >= 7  && h < 9  -> CField      -- early morning: feeding
    | h >= 9  && h < 11 -> CBush       -- mid-morning: heading for cover
    | h >= 11 && h < 14 -> CRidge      -- midday: bedded down
    | h >= 14 && h < 17 -> CBush       -- afternoon: moving again
    | h >= 17 && h < 19 -> CField      -- evening: feeding
    | otherwise         -> CRidge      -- night: bedded
  Nothing -> CField

-- | Pick a destination node for the deer's next move.  Biased toward
-- the preferred terrain class: if any neighbor sits in that class the
-- deer picks from those, otherwise from all neighbors, otherwise from
-- the class itself (a teleport fallback when the deer is stuck).
deerNextLocation :: HuntWorld -> GameWorld -> Location
deerNextLocation hw world =
  let current   = fromMaybe (hwDeerStart hw) (charLocation deer world)
      preferred = deerPreferredClass world
      neighbors = neighborsOf (worldLocationGraph world) current
      prefNeighbors = filter (\l -> hwClass hw l == preferred) neighbors
      prefLocs      = hwLocsOfClass hw preferred
      candidates | not (null prefNeighbors) = prefNeighbors
                 | not (null neighbors)     = neighbors
                 | otherwise                = prefLocs
  in rollChoice world saltDeerMove candidates

-- | All locations adjacent to the given one in the scenario's
-- 'LocationGraph'.  Edges are undirected; we walk both directions.
neighborsOf :: LocationGraph -> Location -> [Location]
neighborsOf lg loc =
  let pairs = Set.toList (lgEdges lg)
  in nub $  [ b | (a, b) <- pairs, a == loc ]
         ++ [ a | (a, b) <- pairs, b == loc ]

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

-- | Build the initial world from a pre-constructed 'HuntWorld'.  The
-- map, start, and deer-start are all already baked into the
-- 'HuntWorld'; this function composes them with characters, tags, and
-- engine-level effects.
initialWorld :: HuntWorld -> CharId -> GameWorld
initialWorld hw you = GameWorld
  { worldCharacters = Map.fromList
      [ (you,  Character you  "You"      [] orEmpty)
      , (deer, Character deer "The Deer" [] orEmpty)
      ]
  , worldGraph         = initialGraph you
  , worldLocations     = Map.fromList
      [ (you,  hwStart hw)
      , (deer, hwDeerStart hw)
      ]
  , worldActiveEffects = map staticLive [timeCycle, weatherCycle]
  , worldClock         = LamportClock 0 (PlayerId "init")
  , worldTags          = orFromList
      (
        [ weatherTag  (WeatherDesc "Clear and Cold")
        , seasonTag   3
        , dayOfWeekTag 5
        , lunarPhaseTag 0
        , dayNumberTag  0
        , timeTag 7
        , windAngleTag 270.0     -- initial wind from the west
        , windStrengthTag 0.2    -- light morning breeze
        ]
        ++ initialSignTags hw  -- seeded sign-hotspot "treasure"
      )
  , worldLocationGraph = gmGraph (hwMap hw)
  , worldSeed          = hwSeed hw
  , worldLocationHistory = Map.empty
  , worldLocationVisits  = Map.empty
  , worldJournal         = []
  , worldDayNumber       = 1
  }

