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
  , HuntDiscovery
  , arrivalDiscoveryAxiom
  , findDiscoveryAxiom
  , discoveryCatalog
  ) where

import           Data.List       (isPrefixOf, sortOn)
import qualified Data.Map.Strict as Map
import           Text.Read       (readMaybe)

import           Engine.Author.DSL
import qualified Engine.Author.Discovery       as ED
import           Engine.Author.Discovery       (Discovery (..), discoveryTag, firstFind)
import           GameTypes
import           Scenarios.DeerHunt.Constants  (formatHuntDate)
import           Scenarios.DeerHunt.Generation (TerrainClass(..))
import           Scenarios.DeerHunt.Signature  (SignatureArchetype(..),
                                                SignatureFind(..),
                                                signatureDiaryLines,
                                                signatureFactoid,
                                                signatureFoundTag)
import           Scenarios.DeerHunt.World      (HuntWorld, hwClass, hwFinds,
                                                hwSignature, hwSignatureLoc)

-- | High-level categorization for a discoverable entry.  Matches the
-- groupings the journal's catalog view will display.  'Signature' is
-- the per-hunt unique find from 'Scenarios.DeerHunt.Signature'; every
-- hunt seeds exactly one, and the catalog shows them separately from
-- the generic rare 'Find' pool.
data DiscoveryKind
  = Tree
  | Animal
  | Sign
  | Find
  | Signature
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | The hunt's specialization of the engine 'Discovery' type.  Show
-- output of @Discovery Tree "raven"@ matches the prior unparameterized
-- shape, so existing world-tag round-trips stay stable.
type HuntDiscovery = Discovery DiscoveryKind

-- | First-sighting beat for a 'SignatureFind'.  Lays down multiple
-- journal lines so the signature reads as a paragraph — the whole
-- point of the Tier 1 slot is that it matters more than a one-line
-- catalog entry.  Every effect shares the same guard so repeat
-- arrivals collapse as usual.
firstSignature :: SignatureFind -> [Effect]
firstSignature sig =
  let d      = Discovery Signature (sigName sig) :: HuntDiscovery
      tag    = discoveryTag d
      guard  = Not (HasWorldTag tag)
      detail = signatureDiaryLines sig
      header = "First Signature: " <> sigName sig <> "."
  in [ immediateWhen guard (Narrate (openingLine Signature (sigName sig))) ]
     <> [ immediateWhen guard (Narrate line) | line <- detail ]
     <> [ immediateWhen guard (JournalEntry header) ]
     <> [ immediateWhen guard (JournalEntry ("  " <> line)) | line <- detail ]
     <> [ immediateWhen guard (AddWorldTag tag)
        , immediateWhen guard (AddWorldTag signatureFoundTag)
        ]

-- | Category-aware one-liner for a first-find.  Keeps the voice of
-- the rest of DeerHunt's prose — short, observational, prairie-dry.
openingLine :: DiscoveryKind -> String -> String
openingLine Tree      name = "A " <> name <> ". Old trunk, new to your map."
openingLine Animal    name = "A " <> name <> ". You stand still a moment."
openingLine Sign      name = name <> ". Recent."
openingLine Find      name = name <> ". Not something you expected to see out here."
openingLine Signature name = name <> ". You stop."

-- | One-time beat for a hunt discovery, in this scenario's voice.
firstHuntFind :: HuntDiscovery -> [Effect]
firstHuntFind = firstFind openingLine

-- ---------------------------------------------------------------------------
-- Discovery pools
-- ---------------------------------------------------------------------------
--
-- Each terrain class has a list of trees and a list of animals that
-- the hunter might plausibly notice there.  Adding a new species is
-- just a line in the relevant list; it becomes part of the catalog
-- automatically once wired through 'arrivalDiscoveryAxiom'.

treesOf :: TerrainClass -> [HuntDiscovery]
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

animalsOf :: TerrainClass -> [HuntDiscovery]
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
-- something.  Wraps the engine helper with hunt-specific pool and
-- voice.
arrivalDiscoveryAxiom :: HuntWorld -> CharId -> Axiom
arrivalDiscoveryAxiom hw you = ED.arrivalDiscoveryAxiom
  (ScenarioAxiom "arrivalDiscovery")
  you
  (\loc -> let cls = hwClass hw loc in treesOf cls ++ animalsOf cls)
  openingLine
  locHash
  arrivalNoticeChance

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

-- | Handle arrival at a find-bearing location.  Signature-slot hits
-- take precedence over generic rare finds at the same cell, since
-- the signature's richer beat subsumes the plain one.  Both tags can
-- co-exist on the world, so a later rare-find rework won't lose a
-- stacked discovery.
handleFindArrival :: HuntWorld -> Location -> [Effect]
handleFindArrival hw loc
  | hwSignatureLoc hw == Just loc =
      let sig = hwSignature hw
      in firstSignature sig
         <> case Map.lookup loc (hwFinds hw) of
              Just name | name /= sigName sig -> firstHuntFind (Discovery Find name)
              _                               -> []
  | otherwise =
      case Map.lookup loc (hwFinds hw) of
        Nothing   -> []
        Just name -> firstHuntFind (Discovery Find name)

-- ---------------------------------------------------------------------------
-- Catalog view
-- ---------------------------------------------------------------------------

-- | Recover every discovery the player has catalogued from the set of
-- scenario tags on the world.  Used by the journal overlay's catalog
-- tab.  Since 'scenarioTag' writes the 'Show' of a 'Discovery' into
-- the tag string, 'Read' recovers it — no separate registry to keep
-- in sync with the tag set.
discoveredEntries :: GameWorld -> [HuntDiscovery]
discoveredEntries world =
  [ d
  | ScenarioTag (MkScenarioTag s) <- worldTagList world
  , Just d <- [readMaybe s :: Maybe HuntDiscovery]
  ]

-- | Render each catalogued discovery as a one-paragraph diary entry
-- in the hunter's voice: the day it was first seen, a terse phrasing
-- of the sighting, and a short factoid.  Order follows the journal
-- (chronological) so the index reads as a running log instead of an
-- alphabetised list — 'discoveredEntries' hands us Map-key order
-- (alphabetical by 'Show Discovery'), so we re-sort by first-seen
-- day and tiebreak on the discovery key to keep same-day entries
-- stable across redraws.
discoveryCatalog :: GameWorld -> [String]
discoveryCatalog world =
  let dayMap    = discoveryDays (worldJournal world)
      dayOf d   = Map.findWithDefault 1 (discoveryKey d) dayMap
      entries   = sortOn (\d -> (dayOf d, discoveryKey d)) (discoveredEntries world)
      line d    = diaryLine (dayOf d) d
  in map line entries

-- | Key used to look up a discovery's first-seen day in the journal
-- scan.  Kind + name is enough: names are unique within a kind.
discoveryKey :: HuntDiscovery -> (DiscoveryKind, String)
discoveryKey (Discovery k n) = (k, n)

-- | Scan the journal once and record the day each discovery was first
-- written down.  Each \"— ... —\" line is a day boundary (the text
-- between the dashes is scenario-specific — dates, day numbers,
-- whatever the scenario emits); the counter simply increments on
-- each one, starting at day 1 before any marker.  \"First Kind:
-- Name.\" lines record an entry at the current day.
discoveryDays :: [String] -> Map.Map (DiscoveryKind, String) Int
discoveryDays = go 1 Map.empty
  where
    go _   acc []           = acc
    go day acc (line:rest)
      | isBoundaryMarker line        = go (day + 1) acc rest
      | Just k <- firstFindKey line  = go day (Map.insert k day acc) rest
      | otherwise                    = go day acc rest

    isBoundaryMarker s =
      length s >= 4
      && take 2 s == "\x2014 "
      && drop (length s - 2) s == " \x2014"

    firstFindKey s
      | take 6 s == "First " =
          let (kindWord, afterKind) = break (== ':') (drop 6 s)
          in case afterKind of
               (':':' ':nameDot) ->
                 let name = reverse (dropWhile (== '.') (reverse nameDot))
                 in case (readMaybe kindWord :: Maybe DiscoveryKind) of
                      Just k  -> Just (k, name)
                      Nothing -> Nothing
               _ -> Nothing
      | otherwise = Nothing

-- | Format one discovery as a diary paragraph: "<date> — sighting.
-- Factoid."  The sighting phrase reads naturally in the hunter's
-- voice (article, verb, species), and the factoid is a short
-- authored fact that grounds the entry in something specific rather
-- than a Pokédex-style stat line.  Missing factoids fall through
-- to bare sighting prose so unknown species still read cleanly.
diaryLine :: Int -> HuntDiscovery -> String
diaryLine day d@(Discovery kind name) =
  let sighting = sightingPhrase kind name
      fact     = factoidFor d
      header   = formatHuntDate day <> " \x2014 " <> sighting <> "."
  in case fact of
       "" -> header
       f  -> header <> " " <> f

-- | A short verbal phrase like "a raven", "trembling aspen", "a shed
-- antler" — whatever reads natural in a diary clause after "I saw".
-- Trees and finds drop the article or use "a patch of" where it
-- helps the prose land; animals get an indefinite article.
sightingPhrase :: DiscoveryKind -> String -> String
sightingPhrase Animal    name = "a " <> name <> " went through"
sightingPhrase Tree      name = "stopped by a " <> name
sightingPhrase Sign      name = "picked up " <> name
sightingPhrase Find      name = "found " <> name
sightingPhrase Signature name = "found " <> name

-- | Short, authored factoid per species.  Written in the same terse
-- prairie voice as the rest of the game; each one grounds the entry
-- in a specific habit, range note, or appearance detail rather than
-- reading as a trivia card.  Empty string = no factoid (the diary
-- line will still render with just the sighting header).
factoidFor :: HuntDiscovery -> String
factoidFor (Discovery Animal name) = case name of
  "raven"            -> "Clever bird. Pairs stay on a territory for years."
  "ruffed grouse"    -> "Drums from a log in spring — a heartbeat louder than you'd believe."
  "snowshoe hare"    -> "Turns white for winter. Moves at the edges of things."
  "red-tailed hawk"  -> "Rides the ridge thermals. Always hunting."
  "jackrabbit"       -> "Not really a rabbit. Runs flat out; stops dead."
  "great horned owl" -> "Nests in old hawk stick nests. Calls a lot before dawn."
  _                  -> ""
factoidFor (Discovery Tree name) = case name of
  "trembling aspen"    -> "Leaves rattle in any breeze. Whole groves grow from one root."
  "bur oak"            -> "Slow wood. Takes fire and comes back."
  "chokecherry"        -> "Berries bitter on the tongue, black when ripe."
  "box elder"          -> "Soft wood, quick growth. Breaks in ice storms."
  "hazel"              -> "Short and dense. Brush deer push through head-low."
  "green ash"          -> "Straight grain; splits clean."
  "willow"             -> "Bent toward water. Roots holding the bank together."
  "red osier dogwood"  -> "Bright red bark under the snow."
  _                    -> ""
factoidFor (Discovery Find name) = case name of
  "rusty 50s car"    -> "No plates, no glass. Someone left it here long before your time."
  "shed antler"      -> "Dropped in winter. You tuck it into your pack."
  "abandoned stand"  -> "Rotten ladder, still nailed up. Not yours to use."
  "survey stake"     -> "Orange plastic, bleached. Cut lines mean plans."
  "beaver stump"     -> "Clean bevels. They work by the hour."
  "skull"            -> "White as paper. You leave it where it lay."
  _                  -> ""
factoidFor (Discovery Signature name) = signatureFactoid (archetypeOfName name)
factoidFor _ = ""

-- | Reconstruct a signature's archetype from the name prefix.  Names
-- all begin with the archetype's common noun (\"shed\", \"cairn\",
-- \"carving\", \"skull\") so a prefix scan is enough — the seeded
-- descriptor can't collide because it's always joined by \" — \".
archetypeOfName :: String -> SignatureArchetype
archetypeOfName name
  | "shed "    `isPrefixOf` name = SigAntler
  | "cairn "   `isPrefixOf` name = SigCairn
  | "carving " `isPrefixOf` name = SigCarving
  | "skull "   `isPrefixOf` name = SigSkull
  | otherwise                    = SigAntler
