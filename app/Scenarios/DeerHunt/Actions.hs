{-# LANGUAGE DataKinds #-}
module Scenarios.DeerHunt.Actions (allActions, huntGraph) where

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
-- 'Scene' with no per-scene actions (hunt actions are universal);
-- every generated edge becomes a bidirectional pair with class-keyed
-- narration pools.
huntGraph :: HuntWorld -> SceneGraph
huntGraph hw = sceneGraphFromLocations
  (gmLocations (hwMap hw))
  (gmGraph (hwMap hw))
  (\_ _ -> [])
  (biEdgeWith (moveNarr hw))

-- | Pick a class-aware narration pool for an edge.  Same-class moves
-- consult the intra-zone pool keyed by (class, position hint);
-- cross-class moves consult the (from class, to class) pool.
moveNarr :: HuntWorld -> Location -> Location -> Narration
moveNarr hw = poolNarration $ \from to ->
  let clsFrom = hwClass hw from
      clsTo   = hwClass hw to
  in if clsFrom == clsTo
       then intraNarration clsTo (hwPositionHint hw to)
       else crossNarration clsFrom clsTo

-- ---------------------------------------------------------------------------
-- Universal actions (not location-gated)
-- ---------------------------------------------------------------------------

allActions :: HuntWorld -> CharacterId -> [AnyAction]
allActions hw you =
  [ anyAction (sitDown you)
  , anyAction (standUp you)
  , anyAction (lookForDeer hw you)
  , anyAction (waveToHunter you)
  , anyAction pickUpPace
  , anyAction slowDown
  , anyAction (takeTheShot you)
  , anyAction continueAction
  , anyAction (callItForTheDay hw you)
  ] ++ compileSceneGraph you (huntGraph hw)

-- ---------------------------------------------------------------------------
-- Core actions
-- ---------------------------------------------------------------------------

huntNotOver :: Condition
huntNotOver = All [ Not (HasWorldTag deerKilled)
                  , Not (HasWorldTag hunterShot)
                  , Not (HasWorldTag deerGone)
                  , Not (HasWorldTag backAtTruck)
                  , Not (HasWorldTag dayOver)
                  , Not (HasWorldTag seasonOver) ]

-- | End the day on your own terms — wind shifted, you're cold, the
-- spot's cooked, whatever.  Triggers the same day-rollover montage as
-- a kill or a miss.  Gated on "not at the truck already" so this is a
-- choice you make out in the bush, not a button at the start line.
callItForTheDay :: HuntWorld -> CharacterId -> Action 'Repeatable
callItForTheDay _hw _you = repeatableAction (ActionId "hunt:callItForTheDay")
  "Call it for the day. Head back to the truck."
  huntNotOver
  [ immediate (Narrate "You stand, shoulder the rifle, and start the walk back. The light has the afternoon slant to it. Good enough for today.")
  , immediate (JournalEntry "Called it. Packed it in. Tomorrow.")
  , immediate (AddWorldTag dayOver)
  ]

-- | Sitting is a toggle: sit down / stand up. While sitting, the stillness
-- axiom increments each tick. Movement actions automatically clear PlayerSitting
-- via the stillness axiom (it resets when the player moves).
sitDown :: CharacterId -> Action 'Repeatable
sitDown _you = repeatableAction (ActionId "sit:on")
  "Sit down and wait."
  (All [huntNotOver, Not (HasWorldTag playerSitting)])
  [ immediate (AddWorldTag playerSitting)
  , immediate (Narrate "You find a spot and settle in. Wind in the trees. Nothing else.")
  ]

standUp :: CharacterId -> Action 'Repeatable
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
lookForDeer :: HuntWorld -> CharacterId -> Action 'Repeatable
lookForDeer _hw you = repeatableAction (ActionId "look")
  "Look for deer."
  huntNotOver
  [ -- Tier 1: Co-located — deer spotted
    immediateWhen sameLoc
      (Narrate "You raise your rifle scope. There it is — a buck, maybe eighty yards out. Broadside. Your hands are shaking.")
  , immediateWhen sameLoc
      (AddWorldTag deerSpotted)
  , immediateWhen (All [sameLoc, Not (HasWorldTag deerSpotted)])
      (JournalEntry "Saw him. Broadside, maybe eighty yards. Hands shaking.")

    -- Tier 2: Same named field zone, distant movement.  Three
    -- phrasings bucketed over a single Chance roll (seeded by tick)
    -- so exactly one lands per look — keeps the beat fresh across
    -- repeated looks in the same zone instead of reading the same
    -- sentence over and over.
  , immediateWhen (All [sameRegion, inFieldRegion, Not sameLoc, Chance lookFieldSalt 0.34])
      (Narrate "Something moved at the far treeline. Gone by the time you lift the scope. Maybe a deer. Maybe wind.")
  , immediateWhen (All [sameRegion, inFieldRegion, Not sameLoc, Not (Chance lookFieldSalt 0.34), Chance lookFieldSalt 0.67])
      (Narrate "Stubble shifts out past the middle of the field. A shape, then nothing. You wait. It doesn't come back.")
  , immediateWhen (All [sameRegion, inFieldRegion, Not sameLoc, Not (Chance lookFieldSalt 0.67)])
      (Narrate "Brown against the ansiGrey, a long way out. You hold still. Whatever it was, it doesn't show itself again.")

    -- Tier 3: Same region, sign present — scrape (very recent)
  , immediateWhen (All [sameRegion, Not sameLoc, hasScrape, lowExp])
      (Narrate "Something's been here. Ground torn up. Recent.")
  , immediateWhen (All [sameRegion, Not sameLoc, hasScrape, midExp])
      (Narrate "Scrape in the dirt. Fresh — hasn't dried yet. He was here less than an hour ago.")
  , immediateWhen (All [sameRegion, Not sameLoc, hasScrape, highExp])
      (Narrate "Ground torn up here. Dirt's still dark. He was here less than an hour ago. Heading out of this spot and moving.")
  , immediateWhen (All [sameRegion, Not sameLoc, hasScrape])
      (JournalEntry "Scrape in the dirt. Fresh. He's close.")

    -- Tier 3b: Same region, tracks present
  , immediateWhen (All [sameRegion, Not sameLoc, hasTracks, Not hasScrape, lowExp])
      (Narrate "Something's been through here. Marks in the ground.")
  , immediateWhen (All [sameRegion, Not sameLoc, hasTracks, Not hasScrape, midExp])
      (Narrate "Tracks. Edges still sharp — recent. A deer passed through.")
  , immediateWhen (All [sameRegion, Not sameLoc, hasTracks, Not hasScrape, highExp])
      (Narrate "Tracks. Pointed and fresh. He came through here in the last couple hours and kept moving.")
  , immediateWhen (All [sameRegion, Not sameLoc, hasTracks, Not hasScrape])
      (JournalEntry "Fresh tracks. He came through here recently.")

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
    -- Dedicated salt for the Chance buckets that pick between the
    -- three field-tier phrasings.  Unique to this action so the roll
    -- isn't shared with any other Chance gate.
    lookFieldSalt = 91731 :: Int

-- | Plausible generated field region names.  The generator always
-- emits one of these five with a cardinal prefix; listing them all
-- keeps the field-tier @lookForDeer@ branch working without having
-- to enumerate regions dynamically.
fieldRegionNames :: [String]
fieldRegionNames =
  [ "North Field", "South Field", "East Field", "West Field", "Central Field" ]

-- | Wave to another hunter when co-located. Only appears after merge
-- brings another player to the same node.
waveToHunter :: CharacterId -> Action 'Repeatable
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
takeTheShot :: CharacterId -> Action 'Once
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
      , immediateWhen hitWithOther
          (AddWorldTag dayOver)
      , immediateWhen hitWithOther
          (JournalEntry "Took him. Someone else was in the line \x2014 they saw. Not proud.")
      , immediateWhen hitSolo
          (Narrate "You squeeze the trigger. The crack splits the cold air. The buck staggers, takes two steps, and folds.")
      , immediateWhen hitSolo
          (Narrate "Clean kill.")
      , immediateWhen hitSolo
          (AddWorldTag deerKilled)
      , immediateWhen hitSolo
          (AddWorldTag dayOver)
      , immediateWhen hitSolo
          (JournalEntry "Took him. Clean shot, he went two steps and folded.")
      ]

    missEffects =
      [ immediateWhen (Not hitCond)
          (Narrate "You squeeze the trigger. The crack echoes off the ridge.")
      , immediateWhen (Not hitCond)
          (Narrate "The buck explodes into motion. By the time you cycle the bolt it's gone. Off the section. You can hear it crashing through bush a quarter mile away.")
      , immediateWhen (Not hitCond)
          (AddWorldTag deerGone)
      , immediateWhen (Not hitCond)
          (AddWorldTag dayOver)
      , immediateWhen (Not hitCond)
          (JournalEntry "Fired. Missed. He crashed off the section before I could cycle the bolt.")
      ]
