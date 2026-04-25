-- | World helpers used by the engine runtime, the renderer, and
-- internal axioms.  Not part of the scenario-author API — the public
-- 'Engine.Core.World' exposes the narrow set of queries and setup
-- helpers a scenario typically needs.
module Engine.Core.World.Internal
  ( -- * App-monad logging and mutation
    narrate
  , logEffect
  , modifyCharacter
    -- * Relationship graph internals
  , setRelStat
  , modifyRelStat
  , updateEdge
  , updateRel
  , addEdge
    -- * Hour and calendar formatters
  , formatHour
  , dayOfWeekName
  , seasonName
  , lunarPhaseName
  , lunarPhaseLabel
    -- * Status-line display helpers
  , engineStatusLine
  , playerLocationName
  , engineTimeStatus
    -- * Spatial helpers (compass bearings)
  , bearing
  , snapToCardinal
  , exitBearings
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.IORef
import           Data.Maybe             (fromMaybe)
import qualified Data.Map.Strict        as Map

import           Engine.Core.NarrativeMessage
import           Engine.CRDT.ORSet
import           Engine.CRDT.PNCounter
import           GameTypes
import           MonadStack

-- ---------------------------------------------------------------------------
-- Logging
-- ---------------------------------------------------------------------------

narrate :: NarrativeMessage -> App ()
narrate msg = do
  logRef <- asks envMessageLog
  w      <- get
  let label   = maybe "" formatHour (getHour w)
      tension = getTension w
  liftIO $ do
    existing <- readIORef logRef
    -- Compare against the most recent *non-empty* label rather than
    -- the literal previous entry's label.  Previously an empty label
    -- (suppressed as a duplicate of the hour before it) would reset
    -- the comparison so the same "3:00 PM" would re-appear a few
    -- entries later, giving the illusion of stopped time.
    let prevLabel = case dropWhile (null . neTimeLabel) existing of
          (e:_) -> neTimeLabel e
          _     -> ""
        shownLabel = if label == prevLabel then "" else label
    modifyIORef' logRef (NarrativeEntry msg tension shownLabel :)

formatHour :: Int -> String
formatHour h
  | h == 0    = "12:00 AM"
  | h < 12    = show h <> ":00 AM"
  | h == 12   = "12:00 PM"
  | otherwise = show (h - 12) <> ":00 PM"

logEffect :: String -> App ()
logEffect msg = do
  log' <- asks envLog
  liftIO $ log' msg

-- ---------------------------------------------------------------------------
-- Character mutation
-- ---------------------------------------------------------------------------

modifyCharacter :: CharId -> (Character -> Character) -> App ()
modifyCharacter cid f =
  modify (\w -> w { worldCharacters = Map.adjust f cid (worldCharacters w) })

-- ---------------------------------------------------------------------------
-- Relationship mutation (low-level building blocks)
-- ---------------------------------------------------------------------------

setRelStat :: CharId -> CharId -> StatType -> Int -> RelationshipGraph -> RelationshipGraph
setRelStat from to stat val = Map.alter (updateEdge to stat val) from

modifyRelStat :: PlayerId -> CharId -> CharId -> StatType -> Int -> RelationshipGraph -> RelationshipGraph
modifyRelStat pid from to stat delta = Map.alter (Just . applyToEdges) from
  where
    applyToEdges Nothing      = Map.singleton to newRel
    applyToEdges (Just edges) = Map.alter (Just . applyToRel) to edges
    applyToRel Nothing                 = newRel
    applyToRel (Just (Relationship m)) =
      Relationship (Map.alter (Just . pnModify pid delta . fromMaybe (pnZero 0)) stat m)
    newRel = Relationship (Map.singleton stat (pnModify pid delta (pnZero 0)))

updateEdge :: CharId -> StatType -> Int -> Maybe (Map.Map CharId Relationship) -> Maybe (Map.Map CharId Relationship)
updateEdge target stat val Nothing      = Just (Map.singleton target (mkRel stat val))
updateEdge target stat val (Just edges) = Just (Map.alter (updateRel stat val) target edges)

updateRel :: StatType -> Int -> Maybe Relationship -> Maybe Relationship
updateRel stat val Nothing                 = Just (mkRel stat val)
updateRel stat val (Just (Relationship m)) = Just (Relationship (Map.insert stat (pnZero val) m))

addEdge :: CharId -> Relationship -> Maybe (Map.Map CharId Relationship) -> Maybe (Map.Map CharId Relationship)
addEdge target rel Nothing      = Just (Map.singleton target rel)
addEdge target rel (Just edges) = Just (Map.insert target rel edges)

-- | Single-stat 'Relationship' constructor.  Local copy used by
-- 'updateEdge' / 'updateRel' so the public 'mkRelationship' in
-- 'Engine.Core.World' can re-export this exact shape.
mkRel :: StatType -> Int -> Relationship
mkRel stat val = Relationship (Map.singleton stat (pnZero val))

-- ---------------------------------------------------------------------------
-- World query helpers used internally by display formatters
-- ---------------------------------------------------------------------------

getHour :: GameWorld -> Maybe Int
getHour w = foldr check Nothing (orToList (worldTags w))
  where
    check (EngineTag (Clock (TimeOfDay h))) _ = Just h
    check _                                acc = acc

getDayOfWeek :: GameWorld -> Maybe Int
getDayOfWeek w = foldr check Nothing (orToList (worldTags w))
  where
    check (EngineTag (Clock (DayOfWeek d))) _ = Just d
    check _                                acc = acc

getLunarPhase :: GameWorld -> Maybe Int
getLunarPhase w = foldr check Nothing (orToList (worldTags w))
  where
    check (EngineTag (Clock (LunarPhase p))) _ = Just p
    check _                                 acc = acc

getSeason :: GameWorld -> Maybe Int
getSeason w = foldr check Nothing (orToList (worldTags w))
  where
    check (EngineTag (Clock (Season s))) _ = Just s
    check _                             acc = acc

-- ---------------------------------------------------------------------------
-- Display formatters
-- ---------------------------------------------------------------------------

dayOfWeekName :: Int -> String
dayOfWeekName 0 = "Monday"
dayOfWeekName 1 = "Tuesday"
dayOfWeekName 2 = "Wednesday"
dayOfWeekName 3 = "Thursday"
dayOfWeekName 4 = "Friday"
dayOfWeekName 5 = "Saturday"
dayOfWeekName 6 = "Sunday"
dayOfWeekName n = "Day " <> show n

seasonName :: Int -> String
seasonName 0 = "Spring"
seasonName 1 = "Summer"
seasonName 2 = "Autumn"
seasonName 3 = "Winter"
seasonName n = "Season " <> show n

lunarPhaseName :: Int -> Maybe String
lunarPhaseName 0  = Just "New Moon"
lunarPhaseName 4  = Just "Waxing Crescent"
lunarPhaseName 8  = Just "First Quarter"
lunarPhaseName 11 = Just "Waxing Gibbous"
lunarPhaseName 15 = Just "Full Moon"
lunarPhaseName 19 = Just "Waning Gibbous"
lunarPhaseName 22 = Just "Last Quarter"
lunarPhaseName 26 = Just "Waning Crescent"
lunarPhaseName _  = Nothing

-- | Range-based label for any day 0–28, suitable for status line display.
lunarPhaseLabel :: Int -> String
lunarPhaseLabel n
  | n <= 3    = "New Moon"
  | n <= 7    = "Waxing Crescent"
  | n <= 10   = "First Quarter"
  | n <= 14   = "Waxing Gibbous"
  | n <= 18   = "Full Moon"
  | n <= 21   = "Waning Gibbous"
  | n <= 25   = "Last Quarter"
  | otherwise = "Waning Crescent"

-- | Default engine status line: location, day of week, season, and moon phase.
-- Returns Nothing only if the player has no location recorded.
engineStatusLine :: CharId -> GameWorld -> Maybe String
engineStatusLine you world =
  let loc   = Map.lookup you (worldLocations world)
      dow   = dayOfWeekName  <$> getDayOfWeek  world
      ssn   = seasonName     <$> getSeason     world
      lunar = lunarPhaseLabel <$> getLunarPhase world
  in case (loc, dow, ssn, lunar) of
       (Just l, Just d, Just s, Just p) -> Just (locationName l <> " — " <> d <> " — " <> s <> " — " <> p)
       _                                -> locationName <$> loc

-- | Just the player's current location name.
playerLocationName :: CharId -> GameWorld -> Maybe String
playerLocationName you world = locationName <$> Map.lookup you (worldLocations world)

-- | Calendar status (day of week, season, moon phase) without location.
engineTimeStatus :: GameWorld -> Maybe String
engineTimeStatus world =
  let dow   = dayOfWeekName  <$> getDayOfWeek  world
      ssn   = seasonName     <$> getSeason     world
      lunar = lunarPhaseLabel <$> getLunarPhase world
  in case (dow, ssn, lunar) of
       (Just d, Just s, Just p) -> Just (d <> "  ·  " <> s <> "  ·  " <> p)
       _                        -> Nothing

-- ---------------------------------------------------------------------------
-- Spatial helpers (coordinate-based)
-- ---------------------------------------------------------------------------

-- | Bearing in degrees (0° = north, 90° = east) from one coordinate to another.
bearing :: (Double, Double) -> (Double, Double) -> Double
bearing (x1, y1) (x2, y2) =
  let raw = atan2 (x2 - x1) (y2 - y1) * 180 / pi
  in if raw < 0 then raw + 360 else raw

-- | Snap a bearing in degrees to the nearest cardinal/intercardinal label.
snapToCardinal :: Double -> String
snapToCardinal deg =
  let labels = ["N","NE","E","SE","S","SW","W","NW"]
      idx :: Int
      idx = round (deg / 45) `mod` 8
  in fromMaybe "N" (lookup idx (zip [0..] labels))

-- | Given the player's location, return adjacent locations with their cardinal
-- label and bearing in degrees. Returns [] if coordinates are not populated.
exitBearings :: CharId -> GameWorld -> [(Location, String, Double)]
exitBearings cid world =
  case Map.lookup cid (worldLocations world) of
    Nothing  -> []
    Just loc ->
      let lg = worldLocationGraph world
          coords = lgCoords lg
      in case Map.lookup loc coords of
           Nothing -> []
           Just here ->
             [ (adj, snapToCardinal b, b)
             | adj <- lgAdjacentTo loc lg
             , Just there <- [Map.lookup adj coords]
             , let b = bearing here there
             ]
