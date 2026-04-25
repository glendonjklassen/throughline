-- | Date formatting helpers shared across scenarios.  Scenarios pick
-- their own anchor 'Day' and offset; this module just renders.
module Engine.Author.Calendar
  ( formatShortDate
  ) where

import Data.Time.Calendar (Day, DayOfWeek(..), dayOfWeek, toGregorian)

-- | Short journal-style date label for a 'Day' — e.g. \"Thu, Nov 7\".
-- Used for notebook day headers and per-day stamps.  The weekday
-- makes the passage of time feel lived-in; the terse month
-- abbreviation keeps headers from crowding a narrow viewport.
formatShortDate :: Day -> String
formatShortDate d =
  let (_y, m, dom) = toGregorian d
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
