-- | Tombstone GC: let scenarios drop old ORSet tombstones so the
-- CRDT state doesn't grow forever.
--
-- Every scenario opts in (or out) by supplying a 'TombstoneGCRule'
-- on its 'Scenario' record — the default is 'Nothing', which keeps
-- tombstones forever.  Deer Hunt uses 'olderThanDays' 365 so
-- anything that died more than a year ago drops out of the log.
-- That matches the narrative vibe: things that happened last season
-- are real; things from a decade back are lore, and lore doesn't
-- need to re-enact itself at merge time.
--
-- GC only drops tombstones whose age was recorded (via 'orDeleteAt').
-- Untimestamped tombstones have unknown ages and are always kept —
-- it would be wrong to drop an unknowably-old tombstone because the
-- value it's suppressing might get re-added and \"come back from the
-- dead\".
module Engine.CRDT.TombstoneGC
  ( TombstoneGCRule
  , olderThan
  , olderThanDays
  , gcWorld
  , gcORSetWith
  ) where

import           Data.Time.Clock  (NominalDiffTime, UTCTime, diffUTCTime)

import           Engine.CRDT.ORSet (orGCTombstones)
import           Engine.CRDT.ORSet.Types
import           GameTypes         (GameWorld (..), TombstoneGCRule)

-- | Drop tombstones older than the given interval.  E.g.
-- @olderThan (60 * 60 * 24 * 30)@ drops anything older than a month.
olderThan :: NominalDiffTime -> TombstoneGCRule
olderThan age now minted = diffUTCTime now minted > age

-- | Convenience: drop tombstones older than the given number of days.
-- 'olderThanDays' 365 is the Deer Hunt default.
olderThanDays :: Integer -> TombstoneGCRule
olderThanDays days = olderThan (fromInteger days * 60 * 60 * 24)

-- | Apply a GC rule to a single 'ORSet'.  Pure and self-contained;
-- callers that want to GC a whole world use 'gcWorld'.
gcORSetWith :: TombstoneGCRule -> UTCTime -> ORSet a -> ORSet a
gcORSetWith rule now = orGCTombstones (rule now)

-- | Apply a GC rule to every ORSet-bearing field on 'GameWorld'.
-- Current tombstone-bearing state lives on 'worldTags' only; if
-- more ORSets appear on 'GameWorld' later, extend this function
-- rather than scattering @gcORSetWith@ calls across the runtime.
gcWorld :: TombstoneGCRule -> UTCTime -> GameWorld -> GameWorld
gcWorld rule now w = w { worldTags = gcORSetWith rule now (worldTags w) }
