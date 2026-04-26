-- | The scenario-facing world API: queries against 'GameWorld' and
-- the small set of relationship-graph constructors a scenario uses
-- to wire up its initial state.  Internal helpers (App-monad
-- mutators, status-line formatters, compass math) live in
-- "Engine.Core.World.Internal".
module Engine.Core.World
  ( -- * Character location
    characterLocation
    -- * Environment queries
  , getWeather
  , getHour
  , getDayOfWeek
  , getLunarPhase
  , getSeason
  , getDayNumber
    -- * Relationship graph setup
  , setCharacterStat
  , mkRelationship
  , addRelationship
  ) where

import qualified Data.Map.Strict as Map

import           Engine.Core.World.Internal (addEdge, setRelStat)
import           Engine.CRDT.ORSet           (orToList)
import           Engine.CRDT.PNCounter       (pnZero)
import           GameTypes

-- ---------------------------------------------------------------------------
-- Character queries
-- ---------------------------------------------------------------------------

-- | Look up a character's current location.  Returns 'Nothing' if
-- the character has not been placed.
characterLocation :: CharacterId -> GameWorld -> Maybe Location
characterLocation cid world = Map.lookup cid (worldLocations world)

-- ---------------------------------------------------------------------------
-- Environment queries
-- ---------------------------------------------------------------------------

-- | Current weather descriptor, if any 'Weather' tag is on the world.
getWeather :: GameWorld -> Maybe WeatherDesc
getWeather w = foldr check Nothing (orToList (worldTags w))
  where
    check (EngineTag (Weather desc)) _   = Just desc
    check _                          acc = acc

-- | Current hour of day (0–23), if a 'TimeOfDay' tag is present.
getHour :: GameWorld -> Maybe Int
getHour w = foldr check Nothing (orToList (worldTags w))
  where
    check (EngineTag (Clock (TimeOfDay h))) _ = Just h
    check _                                acc = acc

-- | Current day of the week (0–6, Monday=0), if tagged.
getDayOfWeek :: GameWorld -> Maybe Int
getDayOfWeek w = foldr check Nothing (orToList (worldTags w))
  where
    check (EngineTag (Clock (DayOfWeek d))) _ = Just d
    check _                                acc = acc

-- | Current lunar phase (0–28, 0=new), if tagged.
getLunarPhase :: GameWorld -> Maybe Int
getLunarPhase w = foldr check Nothing (orToList (worldTags w))
  where
    check (EngineTag (Clock (LunarPhase p))) _ = Just p
    check _                                 acc = acc

-- | Current season (0=spring through 3=winter), if tagged.
getSeason :: GameWorld -> Maybe Int
getSeason w = foldr check Nothing (orToList (worldTags w))
  where
    check (EngineTag (Clock (Season s))) _ = Just s
    check _                             acc = acc

-- | Current scenario day number, if tagged.
getDayNumber :: GameWorld -> Maybe Int
getDayNumber w = foldr check Nothing (orToList (worldTags w))
  where
    check (EngineTag (Clock (DayNumber n))) _ = Just n
    check _                                acc = acc

-- ---------------------------------------------------------------------------
-- Relationship setup (scenario init)
-- ---------------------------------------------------------------------------

-- | Set a character's ground-truth stat value in the relationship
-- graph.  Truth is the canonical stat owner; per-character
-- perceptions branch off from there.
setCharacterStat :: CharacterId -> StatType -> Int -> RelationshipGraph -> RelationshipGraph
setCharacterStat = setRelStat Truth

-- | Build a 'Relationship' carrying a single stat value; all other
-- stats default to 0.
mkRelationship :: StatType -> Int -> Relationship
mkRelationship stat val = Relationship (Map.singleton stat (pnZero val))

-- | Insert a bidirectional pair of relationship edges between two
-- characters.  Use at scenario init to seed initial trust /
-- familiarity / etc.
addRelationship :: CharacterId -> CharacterId -> Relationship -> Relationship -> RelationshipGraph -> RelationshipGraph
addRelationship a b relAtoB relBtoA graph =
  Map.alter (addEdge b relAtoB) a (Map.alter (addEdge a relBtoA) b graph)
