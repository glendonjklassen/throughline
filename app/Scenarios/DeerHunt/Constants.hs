module Scenarios.DeerHunt.Constants where

import           Data.List            (isPrefixOf, nub)
import           Data.Maybe           (fromMaybe)
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

-- | Locations eligible to carry signs.  Roads are excluded (nothing
-- credible happens on a gravel road).  Trucks are excluded too.
hotspotCandidates :: [Location]
hotspotCandidates =
  filter (not . isRoadZone . locationZone) allLocations

-- | Bias: per location, which sign types are plausible and with what
-- probability weight.  Higher weight = more likely to be chosen.
-- Returns a non-empty list of (SignType, weight).  Defaults to a mix
-- of tracks and droppings for unremarkable spots.
locationSignBias :: Location -> [(SignType, Int)]
locationSignBias loc
  -- Named sign-specific locations heavily favour their namesake
  | loc == scrapeLine =
      [(SScrape, 5), (STracks, 2), (SDroppings, 1)]
  | loc == rubLine =
      [(SRub, 5), (STracks, 2), (SHair, 2)]
  | loc == deerTrail || loc == gameTrailEntrance || loc == gameTrailFork =
      [(STracks, 5), (SDroppings, 2), (SHair, 1)]
  -- Bedding spots: sheltered, soft
  | loc `elem` [mossyHollow, dryHummock, oakThicket, willowTangle, windbreak] =
      [(SBed, 4), (STracks, 2), (SDroppings, 1)]
  -- Water crossings and mud: tracks last
  | loc `elem` [creekCrossing, mudFlat, drainageDitch, beaverDam] =
      [(STracks, 5), (SDroppings, 2), (SHair, 1)]
  -- Field feeding grounds
  | loc `elem` [stubbleRows, stubbleFlat, hayBale, cornStubbleStrip, sunflowerStubble] =
      [(SDroppings, 4), (STracks, 3), (SBed, 1)]
  -- Old fences, brush: hair catches
  | loc `elem` [oldFence, fenceLine, brushPile, deadfall, blowdown, fenceCorner] =
      [(SHair, 4), (STracks, 2), (SDroppings, 1)]
  -- Generic bush cover
  | otherwise =
      [(STracks, 2), (SDroppings, 1), (SBed, 1)]

-- | Pick N hotspot locations from the candidates using the session seed.
-- Deterministic: same seed -> same hotspots.  Uses a linear congruential
-- shuffle via mkStdGen to keep the draw independent of any runtime state.
hotspotLocations :: Int -> [Location]
hotspotLocations seed =
  let gen0 = mkStdGen (seed * 7919 + 113)
      pool = hotspotCandidates
      go 0 _ _    = []
      go _ [] _   = []
      go n xs gen =
        let (idx, gen') = randomR (0, length xs - 1) gen
        in case splitAt idx xs of
             (pre, picked : post) -> picked : go (n - 1) (pre ++ post) gen'
             _                    -> []   -- unreachable: idx is in-range
  in go (min hotspotCount (length pool)) pool gen0

-- | For one hotspot, pick 1-3 sign types using the location bias
-- and a seed derived from the session seed + the location name.
hotspotSigns :: Int -> Location -> [SignType]
hotspotSigns seed loc =
  let bias    = locationSignBias loc
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

-- | All initial world tags for sign hotspots at the given seed.
initialSignTags :: Int -> [Tag]
initialSignTags seed =
  [ signAt t loc
  | loc <- hotspotLocations seed
  , t   <- hotspotSigns seed loc
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
        ++ initialSignTags seed  -- seeded sign-hotspot "treasure"
      )
  , worldLocationGraph = huntLocationGraph
  , worldSeed          = seed
  , worldLocationHistory = Map.empty
  , worldLocationVisits  = Map.empty
  }

