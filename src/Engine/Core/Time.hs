-- | Engine-level time-of-day phases.  Every hour 0-23 maps to one
-- 'TimePhase', and 'Engine.Author.CommonAxioms.timeOfDayNarrationAxiom'
-- fires scenario prose when the hour crosses a phase boundary.  Kept
-- here rather than in a scenario because diurnal rhythm isn't a
-- hunting concept — every narrative has a morning and a dusk.
module Engine.Core.Time
  ( TimePhase (..)
  , timeOfDayPhase
  , currentHour
  , currentTimePhase
  ) where

import           Engine.CRDT.ORSet (orToList)
import           GameTypes

-- | A coarse bucket for hour-of-day.  Finer than "day/night", coarse
-- enough that scenarios only need a handful of prose lines.  Ordered
-- in the natural way a day unfolds.
data TimePhase
  = DeepNight     -- 23-02
  | PreDawn       -- 03-05
  | Dawn          -- 06-07
  | Morning       -- 08-10
  | Midday        -- 11-13
  | Afternoon     -- 14-16
  | GoldenHour    -- 17-18
  | Dusk          -- 19-20
  | Night         -- 21-22
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Bucket an hour into a 'TimePhase'.  Hours outside @[0,23]@ wrap
-- modulo 24, so callers can hand in raw lcTick-derived numbers.
timeOfDayPhase :: Int -> TimePhase
timeOfDayPhase h = case h `mod` 24 of
  n | n >= 23 || n <= 2 -> DeepNight
    | n <= 5            -> PreDawn
    | n <= 7            -> Dawn
    | n <= 10           -> Morning
    | n <= 13           -> Midday
    | n <= 16           -> Afternoon
    | n <= 18           -> GoldenHour
    | n <= 20           -> Dusk
    | otherwise         -> Night

-- | The current hour on the world clock, if the world carries a
-- TimeOfDay tag.  Returns 'Nothing' for worlds that don't track
-- hour-of-day.
currentHour :: GameWorld -> Maybe Int
currentHour world =
  case [ h | EngineTag (Clock (TimeOfDay h)) <- orToList (worldTags world) ] of
    (h:_) -> Just h
    []    -> Nothing

-- | The phase of the current hour on the world clock, if the world
-- carries a TimeOfDay tag.  Returns 'Nothing' for worlds that don't
-- track hour-of-day — scenarios without a diurnal cycle quietly opt
-- out of phase-based narration that way.
currentTimePhase :: GameWorld -> Maybe TimePhase
currentTimePhase = fmap timeOfDayPhase . currentHour
