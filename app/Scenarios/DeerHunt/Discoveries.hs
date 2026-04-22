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
  , findDiscoveryAxiom
  , discoveryCatalog
  ) where

import           Data.List       (sort)
import qualified Data.Map.Strict as Map
import           Text.Read       (readMaybe)

import           Engine.Author.DSL
import           Engine.CRDT.ORSet       (orToList)
import           Engine.Core.Conditions  (checkCondition)
import           GameTypes
import           Scenarios.DeerHunt.Generation (TerrainClass(..))
import           Scenarios.DeerHunt.World      (HuntWorld, hwClass, hwFinds)

-- | High-level categorization for a discoverable entry.  Matches the
-- groupings the journal's catalog view will display.
data DiscoveryKind
  = Tree
  | Animal
  | Sign
  | Find
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | A discoverable entry: its category plus its name as it appears
-- in-fiction.  The derived 'Show' instance doubles as the scenario-
-- tag key, so 'scenarioTag' handles persistence and merge via the
-- existing tag infrastructure.  'Read' is derived so the catalog
-- view can recover entries from the world-tag strings without an
-- additional registry.
data Discovery = Discovery DiscoveryKind String
  deriving (Show, Read, Eq, Ord)

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

-- ---------------------------------------------------------------------------
-- Location-bound rare finds
-- ---------------------------------------------------------------------------

-- | When the player arrives at a location that holds a rare find
-- (seeded at worldgen via 'placeFinds'), emit the first-find beat.
-- The discovery tag carried on 'Discovery' dedupes repeat visits, so
-- this axiom is safe to re-run on every arrival.  Triggers off
-- 'diffLocations' — the arrival event — not a point-in-time read.
findDiscoveryAxiom :: HuntWorld -> CharId -> Axiom
findDiscoveryAxiom hw you = Axiom
  { axiomId       = ScenarioAxiom "findDiscovery"
  , axiomPriority = 4
  , axiomEvaluate = \_world _actions diff ->
      concatMap (handleFindArrival hw) (playerArrivals you diff)
  }

handleFindArrival :: HuntWorld -> Location -> [Effect]
handleFindArrival hw loc =
  case Map.lookup loc (hwFinds hw) of
    Nothing   -> []
    Just name -> firstFind (Discovery Find name)

-- ---------------------------------------------------------------------------
-- Catalog view
-- ---------------------------------------------------------------------------

-- | Recover every discovery the player has catalogued from the set of
-- scenario tags on the world.  Used by the journal overlay's catalog
-- tab.  Since 'scenarioTag' writes the 'Show' of a 'Discovery' into
-- the tag string, 'Read' recovers it — no separate registry to keep
-- in sync with the tag set.
discoveredEntries :: GameWorld -> [Discovery]
discoveredEntries world =
  [ d
  | ScenarioTag (MkScenarioTag s) <- orToList (worldTags world)
  , Just d <- [readMaybe s :: Maybe Discovery]
  ]

-- | Group discoveries by kind for the catalog overlay.  Every kind
-- gets a row — empty kinds render as a quiet footer in the overlay
-- so the player sees what's still out there to find.  Names are
-- sorted alphabetically within each group for stable reading.
discoveryCatalog :: GameWorld -> [(String, [String])]
discoveryCatalog world =
  let entries = discoveredEntries world
      grouped k = sort [ name | Discovery k' name <- entries, k' == k ]
  in [ (show k, grouped k) | k <- [minBound .. maxBound :: DiscoveryKind] ]
