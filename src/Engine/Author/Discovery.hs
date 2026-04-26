-- | Engine-level first-find tracking.  A 'Discovery' is a
-- scenario-tagged "I saw this for the first time" entry — trees,
-- animals, sign, rare finds, whatever the scenario considers
-- catalog-worthy.  The 'Show' instance of the parameterized
-- 'Discovery' is what 'scenarioTag' writes to the world tag set, so
-- merge and replay reuse the existing tag infrastructure.
module Engine.Author.Discovery
  ( Discovery (..)
  , discoveryTag
  , firstFind
  , arrivalDiscoveryAxiom
  ) where

import           Engine.Author.DSL
import           Engine.Core.Conditions  (checkCondition)
import           GameTypes

-- | A discovery: its scenario-defined kind plus the in-fiction name.
-- @k@ is whatever 'DiscoveryKind' enum the scenario uses; the
-- @(Show k, Read k, Ord k)@ constraints are paid by callers via
-- standard 'deriving' clauses.
data Discovery k = Discovery k String
  deriving (Show, Read, Eq, Ord)

-- | The scenario tag that marks a 'Discovery' as seen.  Round-trips
-- through the world tag set via 'Show'/'Read' on the parameterized
-- 'Discovery' value.
discoveryTag :: Show k => Discovery k -> Tag
discoveryTag = scenarioTag

-- | One-time beat for a 'Discovery'.  Each effect is guarded by the
-- discovery tag not yet being set, so repeat encounters collapse to
-- nothing.  Caller supplies the prose voice via the opening-line
-- function; the journal line is fixed-format ("First Kind: Name.")
-- so the engine catalog parser can recover entries.
firstFind
  :: Show k
  => (k -> String -> String)   -- ^ in-the-moment line
  -> Discovery k
  -> [Effect]
firstFind opener d@(Discovery kind name) =
  let tag   = discoveryTag d
      guard = Not (HasWorldTag tag)
  in [ immediateWhen guard (Narrate (opener kind name))
     , immediateWhen guard (JournalEntry ("First " <> show kind <> ": " <> name <> "."))
     , immediateWhen guard (AddWorldTag tag)
     ]

-- | When the player arrives at a new location, roll once to notice
-- something.  If the roll lands, the first still-undiscovered entry
-- in the location's pool produces the 'firstFind' beat.  Triggers
-- off the player's arrival deltas, not a point-in-time read.
arrivalDiscoveryAxiom
  :: Show k
  => AxiomId
  -> CharacterId
  -> (Location -> [Discovery k])   -- ^ pool at a location
  -> (k -> String -> String)       -- ^ opening-line voice
  -> (Location -> Int)             -- ^ per-location salt for 'Chance'
  -> Double                        -- ^ per-arrival probability
  -> Axiom
arrivalDiscoveryAxiom aid you poolOf opener salt chance = Axiom
  { axiomId       = aid
  , axiomPriority = 4
  , axiomEvaluate = \world _avail diff ->
      concatMap (atArrival world) (characterArrivals you diff)
  }
  where
    atArrival world loc =
      let undiscovered = filter (not . hasTag world . discoveryTag) (poolOf loc)
      in case undiscovered of
           []    -> []
           (d:_)
             | checkCondition world (Chance (salt loc) chance) -> firstFind opener d
             | otherwise                                       -> []
