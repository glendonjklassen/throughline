{-# LANGUAGE DataKinds #-}
module Scenarios.DeerHunt.Actions (allActions, huntGraph) where

import           Data.List   (nub)
import           Data.Maybe  (mapMaybe)
import           Engine.Author.DSL
import           Engine.Author.Scene
import           GameTypes
import           Scenarios.DeerHunt.Constants
import           Scenarios.DeerHunt.Locations

-- ---------------------------------------------------------------------------
-- Scene graph — 38 locations, programmatic edge generation
-- ---------------------------------------------------------------------------

huntGraph :: SceneGraph
huntGraph = SceneGraph
  { sgScenes = map (\loc -> Scene loc (const [])) allLocations
  , sgEdges  = concatMap mkEdgePair adjacency
  }

-- | Generate bidirectional edges with zone-appropriate prose.
mkEdgePair :: (Location, Location) -> [SceneEdge]
mkEdgePair (a, b) =
  [ SceneEdge (edgeActionId a b) a b (moveLabel b) (moveNarr a b) unconditional
  , SceneEdge (edgeActionId b a) b a (moveLabel a) (moveNarr b a) unconditional
  ]

moveLabel :: Location -> String
moveLabel (Location name) = "Head to " <> name <> "."

-- | Build narration for movement between two locations.
-- Each zone has multiple narration variants picked by NarrationPool.
-- Each edge gets a unique salt derived from its location pair so
-- adjacent edges produce independent sequences.
moveNarr :: Location -> Location -> Narration
moveNarr from to =
  let zFrom = locationZone from
      zTo   = locationZone to
      salt  = edgeSalt from to
      variants = if zFrom /= zTo
                 then crossZoneNarr zFrom zTo to
                 else withinZoneNarr zTo to
      orient = orientation to
  in NarrationPool salt
       [ if null orient then v else v <> " " <> orient | v <- variants ]

-- | Deterministic salt from a location pair for NarrationPool.
edgeSalt :: Location -> Location -> Int
edgeSalt (Location a) (Location b) = sum (map fromEnum a) + sum (map fromEnum b) * 31

withinZoneNarr :: Zone -> Location -> [String]
withinZoneNarr z (Location name) = case z of
  NorthRoad ->
    [ "Gravel crunches under your boots. " <> name <> "."
    , "Loose rock on the shoulder. You keep to the edge. " <> name <> "."
    , "The road stretches ahead, empty both ways. " <> name <> "."
    ]
  SouthRoad ->
    [ "You walk the shoulder. " <> name <> "."
    , "Tire ruts frozen in the mud. " <> name <> "."
    , "Dust kicked up behind you settles slow. " <> name <> "."
    ]
  WestRoad ->
    [ "You follow the ditch line. " <> name <> "."
    , "Cattails in the ditch, ice on the puddles. " <> name <> "."
    , "The road bends ahead. Fence posts leaning. " <> name <> "."
    ]
  NorthField ->
    [ "Wheat stubble cracks underfoot. " <> name <> "."
    , "Stubble rows stretch to the treeline. " <> name <> "."
    , "Frost on the stubble catches the light. " <> name <> "."
    ]
  SouthField ->
    [ "Canola stubble stretches flat ahead. " <> name <> "."
    , "Short stubble, wide open. Nowhere to hide. " <> name <> "."
    , "The field is quiet. Wind moves through the stubble. " <> name <> "."
    ]
  BushEdge ->
    [ "Thin branches scratch your jacket. " <> name <> "."
    , "Deadfall underfoot. You step over a downed birch. " <> name <> "."
    , "The bush thickens here. Slower going. " <> name <> "."
    ]
  OakRidge ->
    [ "You work through thick oaks. " <> name <> "."
    , "Oak leaves underfoot, still damp. " <> name <> "."
    , "Heavy timber. You duck under a low branch. " <> name <> "."
    ]
  WillowBottom ->
    [ "Your boots sink in. " <> name <> "."
    , "Soft ground. Water seeping into your tracks. " <> name <> "."
    , "Willow branches brush your shoulders. Wet underfoot. " <> name <> "."
    ]
  PoplarStand ->
    [ "Open poplar, light through bare branches. " <> name <> "."
    , "Poplar trunks, pale and straight. Easy walking. " <> name <> "."
    , "Leaves long gone. The poplars stand bare. " <> name <> "."
    ]

crossZoneNarr :: Zone -> Zone -> Location -> [String]
crossZoneNarr from to (Location name) = case (from, to) of
  (_, z) | isRoadZone z ->
    [ "You step back out onto the road. " <> name <> "."
    , "Gravel again. You're back on the road. " <> name <> "."
    ]
  (z, _) | isRoadZone z ->
    [ "You leave the road and head into the section. " <> name <> "."
    , "Off the road. Rougher ground now. " <> name <> "."
    ]
  (_, z) | isFieldZone z ->
    [ "The bush opens up into stubble field. " <> name <> "."
    , "Trees thin out. Open field ahead. " <> name <> "."
    ]
  (z, _) | isFieldZone z ->
    [ "You leave the open field and push into cover. " <> name <> "."
    , "Out of the open. Brush closes in around you. " <> name <> "."
    ]
  _ ->
    [ "The terrain changes. " <> name <> "."
    , "Different ground now. " <> name <> "."
    ]

-- ---------------------------------------------------------------------------
-- Orientation — directional hints about adjacent zones
-- ---------------------------------------------------------------------------

-- | One-line orientation: what zones are reachable from here, with directions.
orientation :: Location -> String
orientation loc =
  let zone = locationZone loc
      neighbors = adjacentTo loc
      neighborZones = nub [ locationZone n | n <- neighbors, locationZone n /= zone ]
      dirHints = mapMaybe (zoneDirection zone) neighborZones
  in unwords dirHints

-- | Cardinal direction from one zone to another, based on the section layout.
zoneDirection :: Zone -> Zone -> Maybe String
zoneDirection from to = case (from, to) of
  -- From North Road
  (NorthRoad, NorthField)     -> Just "Fields to the south."
  -- From North Field
  (NorthField, NorthRoad)     -> Just "Road to the north."
  (NorthField, BushEdge)      -> Just "Bush to the south."
  -- From South Road
  (SouthRoad, SouthField)     -> Just "Fields to the north."
  -- From South Field
  (SouthField, SouthRoad)     -> Just "Road to the south."
  (SouthField, WestRoad)      -> Just "Road to the west."
  (SouthField, PoplarStand)   -> Just "Poplar stand to the east."
  -- From West Road
  (WestRoad, SouthField)      -> Just "Fields to the east."
  -- From Bush Edge
  (BushEdge, NorthField)      -> Just "Open field to the north."
  (BushEdge, OakRidge)        -> Just "Oak ridge to the east."
  (BushEdge, PoplarStand)     -> Just "Poplar stand to the south."
  -- From Oak Ridge
  (OakRidge, BushEdge)        -> Just "Bush edge to the west."
  (OakRidge, WillowBottom)    -> Just "Willow bottom to the south."
  -- From Willow Bottom
  (WillowBottom, OakRidge)    -> Just "Ridge to the north."
  (WillowBottom, PoplarStand) -> Just "Poplar stand to the west."
  -- From Poplar Stand
  (PoplarStand, BushEdge)     -> Just "Bush edge to the north."
  (PoplarStand, SouthField)   -> Just "Open field to the west."
  (PoplarStand, WillowBottom) -> Just "Willow bottom to the east."
  _ -> Nothing

-- ---------------------------------------------------------------------------
-- Universal actions (not location-gated)
-- ---------------------------------------------------------------------------

allActions :: CharId -> [AnyAction]
allActions you =
  [ anyAction (sitDown you)
  , anyAction (standUp you)
  , anyAction (lookForDeer you)
  , anyAction (waveToHunter you)
  , anyAction pickUpPace
  , anyAction slowDown
  , anyAction (takeTheShot you)
  , anyAction continueAction
  ] ++ buildActions you huntGraph

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
-- Five tiers: co-located (spotted), same zone in field (distant movement),
-- same zone in bush with sign, same zone generic, different zone.
-- Sign types (tracks, scrapes) provide richer information gated by Understanding level.
-- Wind information also reported when deer is nearby.
lookForDeer :: CharId -> Action 'Repeatable
lookForDeer you = repeatableAction (ActionId "look")
  "Look for deer."
  huntNotOver
  [ -- Tier 1: Co-located — deer spotted
    immediateWhen sameLoc
      (Narrate "You raise your rifle scope. There it is — a buck, maybe eighty yards out. Broadside. Your hands are shaking.")
  , immediateWhen sameLoc
      (AddWorldTag deerSpotted)

    -- Tier 2: Same zone, field — distant movement
  , immediateWhen (All [sameZone, inField, Not sameLoc])
      (Narrate "Movement at the far edge of the field. Could be a deer. Hard to tell at this distance.")

    -- Tier 3: Same zone, sign present — scrape (very recent)
    -- Low Understanding: vague
  , immediateWhen (All [sameZone, Not sameLoc, hasScrape, lowExp])
      (Narrate "Something's been here. Ground torn up. Recent.")
    -- Mid Understanding: type + timing
  , immediateWhen (All [sameZone, Not sameLoc, hasScrape, midExp])
      (Narrate "Scrape in the dirt. Fresh — hasn't dried yet. He was here less than an hour ago.")
    -- High Understanding: full read
  , immediateWhen (All [sameZone, Not sameLoc, hasScrape, highExp])
      (Narrate "Ground torn up here. Dirt's still dark. He was here less than an hour ago. Heading out of this spot and moving.")

    -- Tier 3b: Same zone, tracks present
  , immediateWhen (All [sameZone, Not sameLoc, hasTracks, Not hasScrape, lowExp])
      (Narrate "Something's been through here. Marks in the ground.")
  , immediateWhen (All [sameZone, Not sameLoc, hasTracks, Not hasScrape, midExp])
      (Narrate "Tracks. Edges still sharp — recent. A deer passed through.")
  , immediateWhen (All [sameZone, Not sameLoc, hasTracks, Not hasScrape, highExp])
      (Narrate "Tracks. Pointed and fresh. He came through here in the last couple hours and kept moving.")

    -- Tier 3c: Same zone, fresh sign but no specific sign tags
  , immediateWhen (All [sameZone, Not inField, Not sameLoc, Not hasTracks, Not hasScrape])
      (Narrate "Fresh tracks in the mud. Droppings still warm. It's close.")

    -- Tier 4: Different zone — nothing
  , immediateWhen (All [Not sameZone, Not sameLoc])
      (Narrate "Nothing moving. Just wind and empty bush.")
  ]
  where
    sameLoc   = CoLocated you deer
    sameZone  = InSameRegion you deer
    inField   = Any [ InRegion you (Region "NorthField")
                    , InRegion you (Region "SouthField") ]
    hasTracks = HasWorldTag signTracks
    hasScrape = HasWorldTag signScrape
    -- Experience tiers for sign reading
    lowExp    = Not (statAbove you (Capacity Understanding) 2)    -- 0-2
    midExp    = All [ statAbove you (Capacity Understanding) 2
                    , Not (statAbove you (Capacity Understanding) 5) ]  -- 3-4
    highExp   = statAbove you (Capacity Understanding) 4           -- 5+

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
-- Shot accuracy is tiered by Understanding stat:
--   Understanding 0-2: 0.49 (base: 0.35 + 2*0.07)
--   Understanding 3-4: 0.63
--   Understanding 5-6: 0.77
--   Understanding 7+:  0.84 (capped at 0.85)
-- Friendly fire chance: 10%, only when another hunter is co-located.
-- Since we cannot check for "any co-located non-deer character" as a
-- static condition, the friendly fire path is currently unreachable
-- without merge. This matches the pre-refactor behavior where friendly
-- fire required coLocatedHunters to be non-empty (which only happens
-- after merge).
takeTheShot :: CharId -> Action 'Once
takeTheShot you = targetedOnceAction (ActionId "takeTheShot")
  "Take the shot."
  (ECharacter deer)
  (HasWorldTag deerSpotted)
  (friendlyFireEffects ++ hitEffects ++ missEffects ++ [immediate (AddWorldTag shotTaken)])
  where
    saltS  = 3   -- saltShot
    saltFF = 4   -- saltFriendlyFire

    -- Tiered accuracy by Understanding stat
    tier1Cond = Not (statAbove you (Capacity Understanding) 2)     -- Understanding <= 2
    tier2Cond = All [statAbove you (Capacity Understanding) 2, Not (statAbove you (Capacity Understanding) 4)]
    tier3Cond = All [statAbove you (Capacity Understanding) 4, Not (statAbove you (Capacity Understanding) 6)]
    tier4Cond = statAbove you (Capacity Understanding) 6           -- Understanding >= 7

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
