-- | Lore drops: terse, third-hand rumors the hunter is "carrying"
-- this season.  One is seeded per hunt from 'hwSeed' and surfaces at
-- most once (on a random arrival, gated by a scenario tag) — a quiet
-- acknowledgement that unusual things happen out here without
-- promising the player any particular one.
--
-- The fragment pool deliberately alludes to all three tiers of the
-- unique-finds proposal:
--
-- * Tier 1 — signature-style: specific, concrete things someone saw.
-- * Tier 2 — lifetime-find: white-buck whispers, old-timer swore.
-- * Tier 3 — relic-style: keys, jars, rings, things that opened
--   nothing the finder could name.
--
-- Plus a few generic rural-folklore lines that don't tier-map, so the
-- pool doesn't read as a three-column menu.
module Scenarios.DeerHunt.Lore
  ( LoreFragment (..)
  , loreFragments
  , pickLore
  , loreDropAxiom
  , loreDroppedTag
  ) where

import           Engine.Author.DSL
import           Engine.Core.Conditions  (checkCondition)
import           GameTypes
import           Scenarios.DeerHunt.World (HuntWorld, hwSeed)

-- | One thing the hunter has "heard."  'loreText' is the narration
-- line (voice: something a co-worker or a coffee-counter regular said
-- in passing); 'loreJournal' is the diary shorthand.
data LoreFragment = LoreFragment
  { loreText    :: !String
  , loreJournal :: !String
  } deriving (Show, Eq)

-- | The pool.  Additions are cheap — one entry is a line in this
-- list.  Fragment text stays terse: the player heard a rumor, not a
-- paragraph.
loreFragments :: [LoreFragment]
loreFragments =
  -- Tier 1 — signature-style
  [ LoreFragment
      "somebody told you about a shed with a drop tine long as a thumb"
      "Heard: a drop-tine shed, thumb-long. Never saw it myself."
  , LoreFragment
      "heard about a cairn on a ridge with a cartridge inside — brass, green with age"
      "Heard: a cairn on a ridge with brass inside. People do strange things."
  , LoreFragment
      "a carving in a bur oak somewhere, E.L. and a year in the sixties"
      "Heard: old carving, E.L. and a date. Sixty Novembers now."

  -- Tier 2 — lifetime-find
  , LoreFragment
      "your grandfather swore he saw a white buck near the creek at dusk, once"
      "Heard: grandfather saw a white buck at the creek. He wasn't a liar."
  , LoreFragment
      "neighbour kid claims a pale deer crossed his line three Novembers running"
      "Heard: a pale deer, three Novembers in a row. Neighbour's a kid though."
  , LoreFragment
      "old-timer at the coffee shop says every hunter gets one white buck if they wait"
      "Heard: one white buck, if you wait. Old-timer talk."

  -- Tier 3 — relic-style
  , LoreFragment
      "Harold had a brass key in his glovebox for years. said the woods gave it to him. opened nothing he ever tried"
      "Heard: Harold's brass key. Opened nothing he'd ever seen."
  , LoreFragment
      "there's a story about a sealed jar buried under a trembling aspen out past the creek"
      "Heard: a sealed jar under an aspen. Nobody has ever dug it up."
  , LoreFragment
      "someone found a silver ring in a rotten log once. hasn't turned up since"
      "Heard: a silver ring in a log. It won't be in the same log now."

  -- Generic rural folklore — breaks up the tier cadence
  , LoreFragment
      "lights over the section last March. nobody's got a story that fits"
      "Heard: lights last March. No story fits."
  , LoreFragment
      "old trapper went missing on this ground in '87. rifle was leaned against a tree when they found it"
      "Heard: trapper gone in '87. Rifle left leaning on a tree."
  , LoreFragment
      "the wolves came back south two winters ago. nobody says so out loud"
      "Heard: wolves are back south. People don't say it out loud."
  ]

-- | Deterministically pick the hunt's lore fragment from the seed.
-- Simple modulus — the pool is small enough that collisions across
-- hunts are fine, and players will hear different fragments across
-- different seeds.
pickLore :: Int -> LoreFragment
pickLore seed = loreFragments !! (abs seed `mod` length loreFragments)

-- | Marker tag: the hunt's lore has already dropped.  Used both as a
-- gate in the axiom and as a journal-side check.
loreDroppedTag :: Tag
loreDroppedTag = scenarioTag ("lore-dropped" :: String)

-- | Axiom that surfaces the hunt's lore fragment on a location
-- arrival.  Fires at most once — the 'loreDroppedTag' guard makes
-- subsequent arrivals collapse.  Probability per arrival is low
-- enough that most hunts don't hear it on the very first move; high
-- enough that a typical hunt does surface it before the day is out.
--
-- Voice-wise the beat is italic-grey ambient: the hunter is thinking
-- about something they heard, not encountering it firsthand.
loreDropAxiom :: HuntWorld -> CharId -> Axiom
loreDropAxiom hw you = Axiom
  { axiomId       = ScenarioAxiom "loreDrop"
  , axiomPriority = 4
  , axiomEvaluate = \world _avail diff ->
      concatMap (dropOnArrival hw world) (playerArrivals you diff)
  }
  where
    dropOnArrival hw' world loc
      | checkCondition world (HasWorldTag loreDroppedTag) = []
      | checkCondition world (Chance (locSalt loc) loreDropChance) =
          let frag = pickLore (hwSeed hw')
              guard = Not (HasWorldTag loreDroppedTag)
          in [ immediateWhen guard (Think you (loreText frag))
             , immediateWhen guard (JournalEntry (loreJournal frag))
             , immediateWhen guard (AddWorldTag loreDroppedTag)
             ]
      | otherwise = []

    -- Locations the player newly arrived at this tick.  Dropped
    -- no-op moves to avoid a "resettle" firing lore.
    playerArrivals cid d =
      [ locationDeltaTo ld
      | ld <- diffLocations d
      , locationDeltaChar ld == cid
      , locationDeltaFrom ld /= locationDeltaTo ld
      ]

    -- Stable-per-location salt for the Chance roll.  Matches the
    -- shape used elsewhere in the scenario (sensory picks etc.).
    locSalt (Location s) = foldl (\acc c -> acc * 131 + fromEnum c) 11 s

-- | Probability of the lore fragment firing on any one arrival.  Low
-- enough that the beat doesn't land on turn one most hunts; high
-- enough that a full day of walking usually surfaces it.  Tunable.
loreDropChance :: Double
loreDropChance = 0.08
