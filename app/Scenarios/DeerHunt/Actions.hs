{-# LANGUAGE DataKinds #-}
module Scenarios.DeerHunt.Actions (allActions, huntGraph) where

import qualified Data.Set        as Set
import           Engine.Author.DSL
import           Engine.Author.Scene
import           GameTypes
import           Scenarios.DeerHunt.Constants
import           Scenarios.DeerHunt.Generation (GeneratedMap(..))
import           Scenarios.DeerHunt.Narration  (intraNarration, crossNarration)
import           Scenarios.DeerHunt.World      (HuntWorld, hwClass, hwMap, hwPositionHint)

-- ---------------------------------------------------------------------------
-- Scene graph — built from the generated LocationGraph at scenario init
-- ---------------------------------------------------------------------------

-- | The scene graph for this hunt.  Every generated location becomes a
-- 'Scene'; every generated edge becomes a bidirectional pair of
-- 'SceneEdge's with class-keyed narration pools.
huntGraph :: HuntWorld -> SceneGraph
huntGraph hw = SceneGraph
  { sgScenes = [ Scene loc (const []) | loc <- gmLocations (hwMap hw) ]
  , sgEdges  = concatMap (mkEdgePair hw) (Set.toList (lgEdges (gmGraph (hwMap hw))))
  }

-- | Generate bidirectional edges with class-appropriate prose.
mkEdgePair :: HuntWorld -> (Location, Location) -> [SceneEdge]
mkEdgePair hw (a, b) =
  [ SceneEdge (edgeActionId a b) a b (moveLabel b) (moveNarr hw a b) unconditional
  , SceneEdge (edgeActionId b a) b a (moveLabel a) (moveNarr hw b a) unconditional
  ]

-- | Movement-action label.  Just the destination name — the spatial
-- HUD conveys the "go somewhere" intent via the compass layout, and
-- the zone-tint underline cues the destination biome, so this stays
-- terse on purpose.
moveLabel :: Location -> String
moveLabel (Location name) = name

-- | Build narration for movement between two locations.  If the move
-- stays within the same terrain class we consult the intra-zone pool
-- keyed by (class, position hint); otherwise the cross-zone pool keyed
-- by (from class, to class).  Each edge gets a unique salt derived
-- from its location pair so adjacent edges produce independent PRNG
-- sequences under 'NarrationPool'.
moveNarr :: HuntWorld -> Location -> Location -> Narration
moveNarr hw from to =
  let clsFrom = hwClass hw from
      clsTo   = hwClass hw to
      salt    = edgeSalt from to
      variants
        | clsFrom == clsTo = intraNarration clsTo (hwPositionHint hw to)
        | otherwise        = crossNarration clsFrom clsTo
  in NarrationPool salt variants

-- | Deterministic salt from a location pair for NarrationPool.
edgeSalt :: Location -> Location -> Int
edgeSalt (Location a) (Location b) = sum (map fromEnum a) + sum (map fromEnum b) * 31

-- ---------------------------------------------------------------------------
-- Universal actions (not location-gated)
-- ---------------------------------------------------------------------------

allActions :: HuntWorld -> CharId -> [AnyAction]
allActions hw you =
  [ anyAction (sitDown you)
  , anyAction (standUp you)
  , anyAction (lookForDeer hw you)
  , anyAction (waveToHunter you)
  , anyAction pickUpPace
  , anyAction slowDown
  , anyAction (takeTheShot you)
  , anyAction continueAction
  ] ++ buildActions you (huntGraph hw)

-- ---------------------------------------------------------------------------
-- Core actions
-- ---------------------------------------------------------------------------

huntNotOver :: Condition
huntNotOver = All [ Not (HasWorldTag deerKilled)
                  , Not (HasWorldTag hunterShot)
                  , Not (HasWorldTag deerGone)
                  , Not (HasWorldTag backAtTruck) ]

-- | Sitting is a toggle: sit down / stand up. While sitting, the stillness
-- axiom increments each tick. Movement actions automatically clear PlayerSitting
-- via the stillness axiom (it resets when the player moves).
sitDown :: CharId -> Action 'Repeatable
sitDown _you = repeatableAction (ActionId "sit:on")
  "Sit down and wait."
  (All [huntNotOver, Not (HasWorldTag playerSitting)])
  [ immediate (AddWorldTag playerSitting)
  , immediate (Narrate "You find a spot and settle in. Wind in the trees. Nothing else.")
  ]

standUp :: CharId -> Action 'Repeatable
standUp _you = repeatableAction (ActionId "sit:off")
  "Stand up and move."
  (All [huntNotOver, HasWorldTag playerSitting])
  [ immediate (RemoveWorldTag playerSitting)
  , immediate (Narrate "You push yourself up. Knees stiff. Time to move.")
  ]

-- | Look around — narrates what you can see from here.
-- Five tiers: co-located (spotted), same class in field (distant movement),
-- same class in bush with sign, same class generic, different class.
-- Sign types (tracks, scrapes) provide richer information gated by
-- Understanding level.  Wind information also reported when deer is
-- nearby.
lookForDeer :: HuntWorld -> CharId -> Action 'Repeatable
lookForDeer _hw you = repeatableAction (ActionId "look")
  "Look for deer."
  huntNotOver
  [ -- Tier 1: Co-located — deer spotted
    immediateWhen sameLoc
      (Narrate "You raise your rifle scope. There it is — a buck, maybe eighty yards out. Broadside. Your hands are shaking.")
  , immediateWhen sameLoc
      (AddWorldTag deerSpotted)

    -- Tier 2: Same class region, field — distant movement.  The
    -- scenario's regions are named with cardinal prefixes like
    -- "North Field" / "South Field"; we key on a match by suffix
    -- rather than exact string.  Same for the other field tier below.
  , immediateWhen (All [sameRegion, inFieldRegion, Not sameLoc])
      (Narrate "Movement at the far edge of the field. Could be a deer. Hard to tell at this distance.")

    -- Tier 3: Same region, sign present — scrape (very recent)
  , immediateWhen (All [sameRegion, Not sameLoc, hasScrape, lowExp])
      (Narrate "Something's been here. Ground torn up. Recent.")
  , immediateWhen (All [sameRegion, Not sameLoc, hasScrape, midExp])
      (Narrate "Scrape in the dirt. Fresh — hasn't dried yet. He was here less than an hour ago.")
  , immediateWhen (All [sameRegion, Not sameLoc, hasScrape, highExp])
      (Narrate "Ground torn up here. Dirt's still dark. He was here less than an hour ago. Heading out of this spot and moving.")

    -- Tier 3b: Same region, tracks present
  , immediateWhen (All [sameRegion, Not sameLoc, hasTracks, Not hasScrape, lowExp])
      (Narrate "Something's been through here. Marks in the ground.")
  , immediateWhen (All [sameRegion, Not sameLoc, hasTracks, Not hasScrape, midExp])
      (Narrate "Tracks. Edges still sharp — recent. A deer passed through.")
  , immediateWhen (All [sameRegion, Not sameLoc, hasTracks, Not hasScrape, highExp])
      (Narrate "Tracks. Pointed and fresh. He came through here in the last couple hours and kept moving.")

    -- Tier 3c: Same region, fresh sign but no specific sign tags
  , immediateWhen (All [sameRegion, Not inFieldRegion, Not sameLoc, Not hasTracks, Not hasScrape])
      (Narrate "Fresh tracks in the mud. Droppings still warm. It's close.")

    -- Tier 4: Different region — nothing
  , immediateWhen (All [Not sameRegion, Not sameLoc])
      (Narrate "Nothing moving. Just wind and empty bush.")
  ]
  where
    sameLoc    = CoLocated you deer
    sameRegion = InSameRegion you deer
    -- Any region whose name ends with "Field" counts as a field.  The
    -- region names generated by the section generator are like
    -- "North Field", "South Field"; we match any of them.
    inFieldRegion = Any (map (InRegion you . Region) fieldRegionNames)
    hasTracks = HasWorldTag signTracks
    hasScrape = HasWorldTag signScrape
    lowExp    = Not (statAbove you (Capacity Understanding) 2)
    midExp    = All [ statAbove you (Capacity Understanding) 2
                    , Not (statAbove you (Capacity Understanding) 5) ]
    highExp   = statAbove you (Capacity Understanding) 4

-- | Plausible generated field region names.  The generator always
-- emits one of these five with a cardinal prefix; listing them all
-- keeps the field-tier @lookForDeer@ branch working without having
-- to enumerate regions dynamically.
fieldRegionNames :: [String]
fieldRegionNames =
  [ "North Field", "South Field", "East Field", "West Field", "Central Field" ]

-- | Wave to another hunter when co-located. Only appears after merge
-- brings another player to the same node.
waveToHunter :: CharId -> Action 'Repeatable
waveToHunter you = repeatableAction (ActionId "wave")
  "Wave to the other hunter."
  (All [ huntNotOver
       , HasCoLocated you [deer] ])
  [ immediate (Narrate "You raise a hand. The other hunter nods back.") ]

-- | Toggle pair: moving fast vs. slow. Fast closes the distance before the
-- deer drifts, but you're loud. Slow is quieter but gives it time to wander.
pickUpPace, slowDown :: Action 'Repeatable
(pickUpPace, slowDown) = togglePair (ActionId "pace")
  movingFast
  (Not (HasWorldTag backAtTruck))
  "Pick up the pace."
  [ immediate (Narrate "You push forward. More noise, but you'll close the distance before it moves on.") ]
  "Slow down. Pick your steps."
  [ immediate (Narrate "You ease off. Each step deliberate. Quieter, but the deer won't wait for you.") ]

-- ---------------------------------------------------------------------------
-- The shot
-- ---------------------------------------------------------------------------

-- | Take the shot — available only when DeerSpotted is active.
-- Outcome determined by clock-seeded PRNG via Chance conditions.
takeTheShot :: CharId -> Action 'Once
takeTheShot you = targetedOnceAction (ActionId "takeTheShot")
  "Take the shot."
  (ECharacter deer)
  (HasWorldTag deerSpotted)
  (friendlyFireEffects ++ hitEffects ++ missEffects ++ [immediate (AddWorldTag shotTaken)])
  where
    saltS  = 3   -- saltShot
    saltFF = 4   -- saltFriendlyFire

    tier1Cond = Not (statAbove you (Capacity Understanding) 2)
    tier2Cond = All [statAbove you (Capacity Understanding) 2, Not (statAbove you (Capacity Understanding) 4)]
    tier3Cond = All [statAbove you (Capacity Understanding) 4, Not (statAbove you (Capacity Understanding) 6)]
    tier4Cond = statAbove you (Capacity Understanding) 6

    hitCond = Any
      [ All [tier1Cond, Chance saltS 0.49]
      , All [tier2Cond, Chance saltS 0.63]
      , All [tier3Cond, Chance saltS 0.77]
      , All [tier4Cond, Chance saltS 0.84]
      ]

    hasOther = HasCoLocated you [deer]

    friendlyFireCond = All [hitCond, Chance saltFF 0.10, hasOther]

    friendlyFireEffects =
      [ immediateWhen friendlyFireCond
          (Narrate "You squeeze the trigger. The crack rolls across the section.")
      , immediateWhen friendlyFireCond
          (Narrate "Something is wrong. The shape that drops isn't the deer.")
      , immediateWhen friendlyFireCond
          (AddWorldTag hunterShot)
      ]

    hitWithOther = All [hitCond, Not (Chance saltFF 0.10), hasOther]
    hitSolo      = All [hitCond, Not hasOther]

    hitEffects =
      [ immediateWhen hitWithOther
          (Narrate "You squeeze the trigger. The buck drops where it stands.")
      , immediateWhen hitWithOther
          (Narrate "Clean kill. But you fired with someone in your line. They know it.")
      , immediateWhen hitWithOther
          (AddWorldTag deerKilled)
      , immediateWhen hitSolo
          (Narrate "You squeeze the trigger. The crack splits the cold air. The buck staggers, takes two steps, and folds.")
      , immediateWhen hitSolo
          (Narrate "Clean kill.")
      , immediateWhen hitSolo
          (AddWorldTag deerKilled)
      ]

    missEffects =
      [ immediateWhen (Not hitCond)
          (Narrate "You squeeze the trigger. The crack echoes off the ridge.")
      , immediateWhen (Not hitCond)
          (Narrate "The buck explodes into motion. By the time you cycle the bolt it's gone. Off the section. You can hear it crashing through bush a quarter mile away.")
      , immediateWhen (Not hitCond)
          (AddWorldTag deerGone)
      ]
