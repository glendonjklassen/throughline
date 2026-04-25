-- | Per-hunt unique "signature find": a deterministic, hash-derived
-- discovery seeded once from 'hwSeed'.  Every hunt has exactly one,
-- placed on a good terrain class; whether the player walks to its
-- cell before hunt-end is the question.
--
-- The Tier 1 slot from the @unique-finds@ proposal.  The signature's
-- archetype, descriptor, and location are all functions of the seed,
-- so replaying the same seed produces the same signature in the same
-- place — no within-hunt reroll farming.
module Scenarios.DeerHunt.Signature
  ( SignatureArchetype (..)
  , SignatureFind (..)
  , buildSignature
  , placeSignature
  , signatureDiscovery
  , signatureDiaryLines
  , signatureSpriteName
  , signatureFactoid
    -- * World-tag wire format
  , signatureLocTag
  , signatureArchetypeTag
  , signatureFoundTag
  , parseSignatureLocTag
  , parseSignatureArchetypeTag
  , archetypeHint
  ) where

import           Data.List       (isPrefixOf)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           System.Random   (mkStdGen, randomR)
import           Text.Read       (readMaybe)

import           GameTypes                      (Location(..), Tag(..),
                                                 ScenarioTagValue(..))
import           Scenarios.DeerHunt.Generation (TerrainClass(..))

-- | Archetypes the hash can land on.  Each carries its own palette of
-- seeded descriptors, terrain preference, and sprite.
data SignatureArchetype
  = SigAntler     -- ^ An unusually shaped shed antler
  | SigCairn      -- ^ A stone cairn with something inside
  | SigCarving    -- ^ A name and date carved into a trunk
  | SigSkull      -- ^ A skull with a specific story to it
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | One fully-specified signature find for a hunt.  All fields are
-- derived from the hunt seed, so the same seed always yields the same
-- find.  'sigName' is what lands in the 'Discovery' tag; 'sigDetail'
-- carries the multi-line journal entry.
data SignatureFind = SignatureFind
  { sigArchetype :: !SignatureArchetype
  , sigName      :: !String
    -- ^ In-fiction short name.  Used as the 'Discovery' name, so it
    -- must be unique enough not to collide with the rare-find catalog.
  , sigDetail    :: ![String]
    -- ^ 2–3 short lines for the journal.  Rendered one per entry so
    -- the discovery reads as a paragraph rather than a one-liner.
  , sigTerrain   :: !TerrainClass
    -- ^ Preferred terrain class for placement.
  } deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Seed-driven construction
-- ---------------------------------------------------------------------------

-- | Build the hunt's signature from the seed.  Draws an archetype
-- bucket first, then a descriptor within that bucket.  Pure — same
-- seed always yields the same find.
buildSignature :: Int -> SignatureFind
buildSignature seed =
  let g0          = mkStdGen (seed * 51133 + 17)
      archetypes  = [minBound .. maxBound :: SignatureArchetype]
      (ai,    g1) = randomR (0, length archetypes - 1) g0
      arch        = archetypes !! ai
      variants    = descriptorsFor arch
      (vi,    _)  = randomR (0, length variants - 1) g1
      (name, detail) = variants !! vi
  in SignatureFind
       { sigArchetype = arch
       , sigName      = name
       , sigDetail    = detail
       , sigTerrain   = preferredTerrain arch
       }

-- | Deterministically pick a location for the signature from the
-- seed.  Prefers the archetype's terrain class; falls back to bush,
-- then ridge, then any class with candidates.  Returns 'Nothing' only
-- if the map has no non-road locations at all — shouldn't happen.
placeSignature :: Int -> SignatureFind -> Map TerrainClass [Location] -> Maybe Location
placeSignature seed sig byCls =
  let preferred = sigTerrain sig
      fallbacks = [preferred, CBush, CRidge, CCreek, CField]
      candidates = firstNonEmpty [ Map.findWithDefault [] c byCls | c <- fallbacks ]
      g         = mkStdGen (seed * 7919 + 401)
  in case candidates of
       []  -> Nothing
       xs  -> let (idx, _) = randomR (0, length xs - 1) g
              in Just (xs !! idx)
  where
    firstNonEmpty []       = []
    firstNonEmpty ([]:rest) = firstNonEmpty rest
    firstNonEmpty (xs:_)    = xs

-- ---------------------------------------------------------------------------
-- Archetype tables
-- ---------------------------------------------------------------------------

preferredTerrain :: SignatureArchetype -> TerrainClass
preferredTerrain SigAntler  = CBush
preferredTerrain SigCairn   = CRidge
preferredTerrain SigCarving = CBush
preferredTerrain SigSkull   = CRidge

-- | For each archetype, a pool of (name, journal detail) pairs.  The
-- name must be unique within the signature catalog so the discovery
-- tag disambiguates; the detail reads as a 2–3 line diary fragment.
descriptorsFor :: SignatureArchetype -> [(String, [String])]
descriptorsFor SigAntler =
  [ ( "shed — seven-point non-typical"
    , [ "Seven points, off the right beam."
      , "Old drop tine like a hook. You turn it over twice before setting it down."
      ]
    )
  , ( "shed — heavy palmated beam"
    , [ "Palmated like a moose's, but whitetail."
      , "Heavier than it should be. Grey with age."
      ]
    )
  , ( "shed — drop tine, long as your thumb"
    , [ "A drop tine off the main — longer than your thumb."
      , "You sit with it a minute. He's around here, or he was."
      ]
    )
  , ( "shed — shed velvet still clinging"
    , [ "Dropped so recent the velvet's still on in strips."
      , "Brown and papery. You leave it in the grass where you found it."
      ]
    )
  ]

descriptorsFor SigCairn =
  [ ( "cairn — brass cartridge inside"
    , [ "Stacked stones, three high, the way someone marked something."
      , "A brass .30-30 in the gap. Corroded ansiGreen. You leave it."
      ]
    )
  , ( "cairn — tin of matches, dry"
    , [ "Small cairn at the edge of the ridge."
      , "A flat tin of strike-anywhere matches under the top stone. Still dry."
      ]
    )
  , ( "cairn — coin, face worn smooth"
    , [ "A cairn someone built to last. Lichen on the east face."
      , "A coin wedged at the base — face worn smooth. Canadian, pre-war maybe."
      ]
    )
  , ( "cairn — stacked seven high"
    , [ "Seven stones, stacked the way kids stack them but heavier."
      , "Someone's count of something. You don't touch it."
      ]
    )
  ]

descriptorsFor SigCarving =
  [ ( "carving — E.L. 1962"
    , [ "Old carving cut deep into a bur oak — \"E.L. 1962\"."
      , "The scar's blackened. Sixty-three Novembers of rain."
      ]
    )
  , ( "carving — J+M 74"
    , [ "\"J+M 74\" carved into a box elder. Boxed in a heart."
      , "The wood has grown around the cuts but hasn't swallowed them."
      ]
    )
  , ( "carving — a single word, \"here\""
    , [ "One word cut into an aspen, at shoulder height: \"here\"."
      , "You stand where they stood. Nothing else to say."
      ]
    )
  , ( "carving — cross marked into chokecherry"
    , [ "A plain cross, hand-sized, cut into chokecherry bark."
      , "Old enough the bark has scarred shut around it. Memorial, probably."
      ]
    )
  ]

descriptorsFor SigSkull =
  [ ( "skull — clean hole between the eyes"
    , [ "Whitetail skull, bleached, clean .243 between the eyes."
      , "Somebody else's story. You leave it on the ridge where it lay."
      ]
    )
  , ( "skull — heavy rack still attached"
    , [ "Skull with a heavy nine-point rack still on it."
      , "No bullet hole. Maybe age, maybe a fight, maybe wolves."
      ]
    )
  , ( "skull — jaw broken, wired back"
    , [ "Skull someone wired the jaw back onto. Baling wire, rusted."
      , "Trophy mount gone to the weather. Strange to find out here."
      ]
    )
  , ( "skull — coyote skull in a badger hole"
    , [ "Narrow skull in the mouth of a badger hole. Coyote."
      , "Long enough dead it came back up on its own."
      ]
    )
  ]

-- ---------------------------------------------------------------------------
-- Rendering / catalog helpers
-- ---------------------------------------------------------------------------

-- | The Discovery-shaped representation of a signature.  Kept here so
-- the discovery module can import it without circular references.
-- Returns the 'sigName' — the 'Discovery' kind is always 'Signature'.
signatureDiscovery :: SignatureFind -> String
signatureDiscovery = sigName

-- | Multi-line diary entry for a signature find, returned as one
-- string per line.  Each line lands as its own journal entry so the
-- notebook renders a paragraph rather than a blob.
signatureDiaryLines :: SignatureFind -> [String]
signatureDiaryLines = sigDetail

-- | Sprite key for a signature find.  Different archetypes render
-- differently so the world map shows archetypes at a glance.
signatureSpriteName :: SignatureArchetype -> String
signatureSpriteName SigAntler  = "signature-antler"
signatureSpriteName SigCairn   = "signature-cairn"
signatureSpriteName SigCarving = "signature-carving"
signatureSpriteName SigSkull   = "signature-skull"

-- | Short factoid shown under the signature's sighting line in the
-- catalog view.  Gives each archetype its own closing sentence.
signatureFactoid :: SignatureArchetype -> String
signatureFactoid SigAntler  = "You'll remember where you found it."
signatureFactoid SigCairn   = "Somebody marked this. Long before you got here."
signatureFactoid SigCarving = "Old hands, long gone, but the hand still shows."
signatureFactoid SigSkull   = "Bone keeps the story longer than memory does."

-- ---------------------------------------------------------------------------
-- World-tag wire format
-- ---------------------------------------------------------------------------
--
-- The signature's location and archetype are placed on 'worldTags' at
-- scenario init so display hooks (which can't close over a 'HuntWorld'
-- — see the ScenarioDisplay contract) can recover them from world
-- state alone.  The discovery axiom emits a third boolean marker tag
-- — 'signatureFoundTag' — when the player finds the signature, so the
-- end-screen can branch on "got the signature, with or without the
-- deer" without having to recover the signature's specific name.

-- | Encode the signature's location as a world tag.  Form:
-- @"sig-loc|<LocationName>"@.  One per hunt.
signatureLocTag :: Location -> Tag
signatureLocTag (Location name) =
  ScenarioTag (MkScenarioTag ("sig-loc|" <> name))

-- | Encode the signature's archetype as a world tag.  Form:
-- @"sig-arch|<SigAntler|SigCairn|...>"@.  One per hunt.  Read back
-- via 'parseSignatureArchetypeTag' (which relies on 'Read' for the
-- enum).
signatureArchetypeTag :: SignatureArchetype -> Tag
signatureArchetypeTag a =
  ScenarioTag (MkScenarioTag ("sig-arch|" <> show a))

-- | Plain marker that the player has found their signature this
-- hunt.  Used by the end-screen branching and by prose hooks that
-- need a yes/no read without parsing the full Discovery tag.
signatureFoundTag :: Tag
signatureFoundTag = ScenarioTag (MkScenarioTag "sig-found")

-- | Parse a 'signatureLocTag' back to its 'Location', or 'Nothing'
-- if the tag isn't a signature-location tag.
parseSignatureLocTag :: Tag -> Maybe Location
parseSignatureLocTag (ScenarioTag (MkScenarioTag s))
  | "sig-loc|" `isPrefixOf` s = Just (Location (drop 8 s))
parseSignatureLocTag _ = Nothing

-- | Parse a 'signatureArchetypeTag' back to its 'SignatureArchetype',
-- or 'Nothing' if the tag isn't an archetype tag or is malformed.
parseSignatureArchetypeTag :: Tag -> Maybe SignatureArchetype
parseSignatureArchetypeTag (ScenarioTag (MkScenarioTag s))
  | "sig-arch|" `isPrefixOf` s = readMaybe (drop 9 s)
parseSignatureArchetypeTag _ = Nothing

-- | Short, archetype-flavored sensory fragment — used by the display
-- hook to nudge ambient narration toward the signature when the
-- hunter is on or adjacent to its cell.  Tuned to drift past without
-- naming the object: the beat should read like peripheral perception,
-- not a spoiler.
archetypeHint :: SignatureArchetype -> [String]
archetypeHint SigAntler =
  [ "bone-pale in the grass, catches the corner of your eye"
  , "something the colour of dropped antler, off the path"
  , "a gleam low in the brush that isn't frost"
  ]
archetypeHint SigCairn =
  [ "stone where stone shouldn't be"
  , "a stack of stones against a ridge, too deliberate"
  , "stones set by hands, a long time back"
  ]
archetypeHint SigCarving =
  [ "a wound on a trunk ahead, old and grown-around"
  , "a mark the bark won't close over, deep in the oak"
  , "a blackened scar on an old trunk, not lightning-shaped"
  ]
archetypeHint SigSkull =
  [ "something white in the leaves, not snow"
  , "a shape at the base of a tree that doesn't fit the terrain"
  , "pale bone against the brown"
  ]
