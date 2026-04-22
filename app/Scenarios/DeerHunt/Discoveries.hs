{-# LANGUAGE DeriveGeneric #-}
-- | First-find tracking for DeerHunt: the hunter's field catalog of
-- trees, animals, sign, and rare finds.  A 'Discovery' is a scenario
-- tag in the existing 'scenarioTag' shape — no new engine machinery.
-- 'firstFind' emits the one-time beat (terse narration, journal line,
-- tag).  Subsequent encounters collapse to nothing via the tag guard.
--
-- Pools live here too: what trees and animals belong to each terrain
-- class, and what the arrival axiom considers eligible to reveal.
module Scenarios.DeerHunt.Discoveries
  ( DiscoveryKind (..)
  , Discovery (..)
  , discoveryTag
  , firstFind
  , arrivalDiscoveryAxiom
  ) where

import           Engine.Author.DSL
import           Engine.Core.Conditions (checkCondition)
import           GameTypes
import           Scenarios.DeerHunt.Generation (TerrainClass(..))
import           Scenarios.DeerHunt.World      (HuntWorld, hwClass)

-- | High-level categorization for a discoverable entry.  Matches the
-- groupings the journal's catalog view will display.
data DiscoveryKind
  = Tree
  | Animal
  | Sign
  | Find
  deriving (Show, Eq, Ord)

-- | A discoverable entry: its category plus its name as it appears
-- in-fiction.  The derived 'Show' instance doubles as the scenario-
-- tag key, so 'scenarioTag' handles persistence and merge via the
-- existing tag infrastructure.
data Discovery = Discovery DiscoveryKind String
  deriving (Show, Eq, Ord)

-- | The scenario tag that marks a 'Discovery' as seen.
discoveryTag :: Discovery -> Tag
discoveryTag = scenarioTag

-- | One-time beat for a 'Discovery'.  Each effect is guarded by the
-- discovery tag not yet being set, so repeat encounters collapse.
firstFind :: Discovery -> [Effect]
firstFind d@(Discovery kind name) =
  let tag   = discoveryTag d
      guard = Not (HasWorldTag tag)
  in [ immediateWhen guard (Narrate (openingLine kind name))
     , immediateWhen guard (JournalEntry ("First " <> show kind <> ": " <> name <> "."))
     , immediateWhen guard (AddWorldTag tag)
     ]

-- | Category-aware one-liner for a first-find.  Keeps the voice of
-- the rest of DeerHunt's prose — short, observational, prairie-dry.
openingLine :: DiscoveryKind -> String -> String
openingLine Tree   name = "A " <> name <> ". Old trunk, new to your map."
openingLine Animal name = "A " <> name <> ". You stand still a moment."
openingLine Sign   name = name <> ". Recent."
openingLine Find   name = name <> ". Not something you expected to see out here."

-- ---------------------------------------------------------------------------
-- Discovery pools
-- ---------------------------------------------------------------------------
--
-- Each terrain class has a list of trees and a list of animals that
-- the hunter might plausibly notice there.  Adding a new species is
-- just a line in the relevant list; it becomes part of the catalog
-- automatically once wired through 'arrivalDiscoveryAxiom'.

treesOf :: TerrainClass -> [Discovery]
treesOf CBush  = [ Discovery Tree "trembling aspen"
                 , Discovery Tree "bur oak"
                 , Discovery Tree "chokecherry"
                 , Discovery Tree "box elder"
                 ]
treesOf CRidge = [ Discovery Tree "bur oak"
                 , Discovery Tree "hazel"
                 , Discovery Tree "green ash"
                 ]
treesOf CCreek = [ Discovery Tree "willow"
                 , Discovery Tree "red osier dogwood"
                 ]
treesOf _      = []

animalsOf :: TerrainClass -> [Discovery]
animalsOf CBush  = [ Discovery Animal "raven"
                   , Discovery Animal "ruffed grouse"
                   , Discovery Animal "snowshoe hare"
                   ]
animalsOf CRidge = [ Discovery Animal "raven"
                   , Discovery Animal "red-tailed hawk"
                   ]
animalsOf CField = [ Discovery Animal "raven"
                   , Discovery Animal "jackrabbit"
                   ]
animalsOf CCreek = [ Discovery Animal "great horned owl"
                   ]
animalsOf _      = []

-- ---------------------------------------------------------------------------
-- Arrival axiom
-- ---------------------------------------------------------------------------

-- | When the player arrives at a new location, roll once to notice
-- something.  If the roll lands, the first still-undiscovered entry
-- in that terrain's pool becomes the beat for this tick.  Triggers
-- off 'diffLocations' — the actual arrival event — not a point-in-
-- time read of 'worldLocations'.
arrivalDiscoveryAxiom :: HuntWorld -> CharId -> Axiom
arrivalDiscoveryAxiom hw you = Axiom
  { axiomId       = ScenarioAxiom "arrivalDiscovery"
  , axiomPriority = 4
  , axiomEvaluate = \world _actions diff ->
      concatMap (discoveriesOnArrival hw world) (playerArrivals you diff)
  }

-- | Locations the player newly arrived at this tick.  A no-op move
-- (from == to) is dropped: a "resettle" shouldn't surface new finds.
playerArrivals :: CharId -> WorldDiff -> [Location]
playerArrivals you diff =
  [ locationDeltaTo ld
  | ld <- diffLocations diff
  , locationDeltaChar ld == you
  , locationDeltaFrom ld /= locationDeltaTo ld
  ]

-- | Decide what — if anything — the hunter notices on arrival here.
-- At most one discovery per tick: the first undiscovered entry from
-- the combined tree+animal pool for this terrain class, gated by a
-- per-location Chance roll.
discoveriesOnArrival :: HuntWorld -> GameWorld -> Location -> [Effect]
discoveriesOnArrival hw world loc =
  let cls          = hwClass hw loc
      pool         = treesOf cls ++ animalsOf cls
      undiscovered = filter (not . hasDiscovered world) pool
      locSalt      = locHash loc
  in case undiscovered of
       []    -> []
       (d:_)
         | checkCondition world (Chance locSalt arrivalNoticeChance) -> firstFind d
         | otherwise                                                 -> []

-- | Has the player already catalogued this entry?
hasDiscovered :: GameWorld -> Discovery -> Bool
hasDiscovered world d = hasTag world (discoveryTag d)

-- | Probability the hunter notices anything new when entering a cell.
-- Low enough that a walk doesn't turn into a stream of beats; high
-- enough that seasoned play fills the catalog out.  Tunable.
arrivalNoticeChance :: Double
arrivalNoticeChance = 0.35

-- | Stable-per-location salt for 'Chance'.  Same shape as the hash
-- used by the SDL runner's sensory selection.
locHash :: Location -> Int
locHash (Location s) = foldl (\acc c -> acc * 131 + fromEnum c) 7 s
