module Scenarios.DeerHunt.Axioms (allAxioms, dawnRule, hunterArrivalMergeAxiom) where

import           Data.Maybe        (fromMaybe)
import qualified Data.Map.Strict as Map
import           Engine.Author.CommonAxioms  (weatherNarrationAxiom)
import           Engine.Author.DSL
import           Engine.Author.Random        (rollCheck, rollChoice, rollD)
import           Engine.CRDT.ORSet           (orToList)
import           GameTypes
import           Scenarios.DeerHunt.Constants
import           Scenarios.DeerHunt.Locations
import           Scenarios.DeerHunt.Probability

-- ---------------------------------------------------------------------------
-- Shared predicate
-- ---------------------------------------------------------------------------

-- | True when the hunt has ended: deer killed, deer gone, or hunter shot.
huntOver :: GameWorld -> Bool
huntOver world = hasTag world deerKilled
              || hasTag world deerGone
              || hasTag world hunterShot

-- ---------------------------------------------------------------------------
-- Axiom list
-- ---------------------------------------------------------------------------

allAxioms :: CharId -> [Axiom]
allAxioms you =
  [ windAxiom
  , deerMovementAxiom
  , spookAxiom you
  , signPlacementAxiom you
  , signDiscoveryAxiom you
  , deerPresenceAxiom you
  , stillnessAxiom you
  , experienceAxiom you
  , nightfallAxiom you
  , tensionAxiom you
  , weatherNarrationAxiom weatherDesc
  ]

-- ---------------------------------------------------------------------------
-- Wind drift
-- ---------------------------------------------------------------------------

-- | Each tick, the wind direction drifts slightly and strength adjusts.
-- Weather changes force larger shifts.
windAxiom :: Axiom
windAxiom = Axiom
  { axiomId       = ScenarioAxiom "wind"
  , axiomPriority = 1
  , axiomEvaluate = \world _actions diff ->
      let oldAngle    = getWindAngle world
          oldStrength = getWindStrength world
          -- Use rollD to get a pseudo-random value for drift
          driftRoll   = rollD world saltWindDrift
          gustRoll    = rollD world saltWindGust
          -- Normal drift: ~±2° per tick (map 0-1 roll to -4..+4 range)
          normalDrift = (driftRoll - 0.5) * 8.0
          -- Occasional gust: 10% chance of 10-20° shift
          gustDrift   | gustRoll < 0.10 = (driftRoll - 0.5) * 40.0
                      | otherwise       = 0.0
          -- Weather change forces a larger shift
          weatherChanged = any isWeather (diffWorldTagsAdded diff)
          weatherShift | weatherChanged = (driftRoll - 0.3) * 90.0  -- 30-60° shift
                       | otherwise      = 0.0
          -- Compute new angle (wrap 0-360)
          rawAngle = oldAngle + normalDrift + gustDrift + weatherShift
          newAngle = rawAngle - fromIntegral (floor (rawAngle / 360.0) * 360 :: Int)
          -- Strength bias based on current weather
          weatherBias = weatherStrengthBias world
          -- Drift strength toward weather bias
          strengthDrift = (weatherBias - oldStrength) * 0.1  -- slow convergence
          randomNudge   = (driftRoll - 0.5) * 0.05           -- small random variation
          newStrength   = max 0.0 (min 1.0 (oldStrength + strengthDrift + randomNudge))
          -- Remove old wind tags and add new ones
          removeOld = [ immediate (RemoveWorldTag t)
                      | t <- orToList (worldTags world)
                      , isWindAngleTag t || isWindStrengthTag t ]
          addNew    = [ immediate (AddWorldTag (windAngleTag newAngle))
                      , immediate (AddWorldTag (windStrengthTag newStrength))
                      ]
      in removeOld ++ addNew
  }

-- | Weather-based target for wind strength.
weatherStrengthBias :: GameWorld -> Double
weatherStrengthBias world =
  case [ w | EngineTag (Weather w) <- orToList (worldTags world) ] of
    (WeatherDesc "Windy" : _)          -> 0.85
    (WeatherDesc "Light Snow" : _)     -> 0.40
    (WeatherDesc "Overcast" : _)       -> 0.50
    (WeatherDesc "Clear and Cold" : _) ->
      -- Calm dawn, rising by midday
      case currentHour world of
        Just h | h < 9     -> 0.15
               | h < 14    -> 0.40
               | otherwise -> 0.30
        Nothing -> 0.20
    _ -> 0.30

-- ---------------------------------------------------------------------------
-- Deer movement
-- ---------------------------------------------------------------------------

-- | Each tick, the deer may move to an adjacent node.
-- Biased toward its time-of-day preferred zone.
-- Frozen once the hunt is over (killed, gone, or shot a hunter).
deerMovementAxiom :: Axiom
deerMovementAxiom = Axiom
  { axiomId       = ScenarioAxiom "deerMovement"
  , axiomPriority = 1
  , axiomEvaluate = \world _actions _diff ->
      let frozen = huntOver world || hasTag world deerSpooked
      in if frozen then [] else
           let current   = fromMaybe stubbleRows (charLocation deer world)
               preferred = deerPreferredZone world
               prefLocs  = zoneLocations preferred
               neighbors = adjacentTo current
               prefNeighbors = filter (\l -> locationZone l == preferred) neighbors
               -- Moving fast = closing the gap, deer less likely to drift.
               -- Careful approach = quiet, but gives the deer time to wander.
               moveChance | hasTag world movingFast = 0.15
                          | otherwise               = 0.30
               moves = rollCheck world saltDeerMove moveChance
               dest | not (null prefNeighbors) = rollChoice world (saltDeerMove + 10) prefNeighbors
                    | not (null neighbors)     = rollChoice world (saltDeerMove + 10) neighbors
                    | not (null prefLocs)      = rollChoice world (saltDeerMove + 10) prefLocs
                    | otherwise                = current
           in [immediate (SetLocation deer dest) | moves && dest /= current]
  }

-- ---------------------------------------------------------------------------
-- Spook check
-- ---------------------------------------------------------------------------

-- | When the player enters the deer's node (or vice versa), check for spook.
-- If spooked, the deer bolts to a different zone.
-- Terrain noise at the player's location and visibility at the deer's location
-- modify the base spook chance.
spookAxiom :: CharId -> Axiom
spookAxiom you = Axiom
  { axiomId       = ScenarioAxiom "spook"
  , axiomPriority = 2
  , axiomEvaluate = \world _actions diff ->
      let playerLoc = charLocation you world
          deerLoc   = charLocation deer world
          sameNode  = case (playerLoc, deerLoc) of
            (Just pl, Just dl) -> pl == dl
            _                  -> False
          -- Did someone just move? Check location deltas.
          moved = any (\ld -> locationDeltaChar ld == you || locationDeltaChar ld == deer)
                      (diffLocations diff)
          -- Spook probability depends on whether player is sitting vs moving,
          -- modified by terrain at both locations and wind.
          sitting     = not moved
          baseChance  | sitting   = spookChanceSitting world you
                      | otherwise = spookChance world you
          terrainMod  = case (playerLoc, deerLoc) of
            (Just pl, Just dl) -> terrainSpookModifier pl dl (not sitting) world
            _                  -> 0.0
          -- Wind modifier: scent-based detection
          windAngle'  = getWindAngle world
          windStr     = getWindStrength world
          windMult    = case (playerLoc, deerLoc) of
            (Just pl, Just dl)
              | pl == dl  -> 1.3     -- same location, settled scent
              | otherwise -> windSpookModifier (windAlignment pl dl windAngle' world) windStr
            _ -> 1.0
          -- Stillness modifier: patient sitting reduces detection
          stillnessMod = stillnessSpookModifier (getStillness you world)
          finalChance = max 0.02 (baseChance * windMult + terrainMod + stillnessMod)
          spooked     = rollCheck world saltSpook finalChance
          -- Where does the deer bolt to?
          currentZone = maybe BushEdge locationZone deerLoc
          otherZones  = filter (\z -> z /= currentZone && not (isRoadZone z)) allZones
          boltZone    = rollChoice world (saltSpook + 20) otherZones
          boltDest    = rollChoice world (saltSpook + 30) (zoneLocations boltZone)
      in if not (huntOver world || hasTag world deerSpooked) && sameNode && moved
         then if spooked
              then [ immediate (Narrate "A crash of brush. White flag up. The buck bolts through the trees and is gone.")
                   , immediate (SetLocation deer boltDest)
                   , immediate (AddWorldTag deerSpooked)
                   , immediate (RemoveWorldTag deerSpotted)
                   ]
              else [ immediate (Narrate "You freeze. Through the branches — antlers. A buck. It hasn't seen you.")
                   , immediate (AddWorldTag deerSpotted)
                   , immediate (RemoveWorldTag deerSpooked)
                   ]
         else []
  }

-- ---------------------------------------------------------------------------
-- Deer presence tracking
-- ---------------------------------------------------------------------------

-- | Maintains DeerSpotted and FreshSign tags based on player/deer proximity.
-- Clears DeerSpooked after one tick so the deer can be found again.
deerPresenceAxiom :: CharId -> Axiom
deerPresenceAxiom you = Axiom
  { axiomId       = ScenarioAxiom "deerPresence"
  , axiomPriority = 3
  , axiomEvaluate = \world _actions _diff ->
      let over      = huntOver world
          playerLoc = charLocation you world
          deerLoc   = charLocation deer world
          sameNode  = case (playerLoc, deerLoc) of
            (Just pl, Just dl) -> pl == dl
            _                  -> False
          sameZone  = case (playerLoc, deerLoc) of
            (Just pl, Just dl) -> locationZone pl == locationZone dl
            _                  -> False
          clearSpotted = [immediate (RemoveWorldTag deerSpotted) | hasTag world deerSpotted && not sameNode && not over]
          clearSign    = [immediate (RemoveWorldTag freshSign) | hasTag world freshSign && not sameZone && not over]
          clearSpooked = [immediate (RemoveWorldTag deerSpooked) | hasTag world deerSpooked]
          addSign      = [immediate (AddWorldTag freshSign) | sameZone && not (hasTag world freshSign) && not over]
      in if over then clearSpotted ++ clearSign
         else clearSpotted ++ clearSign ++ clearSpooked ++ addSign
  }

-- ---------------------------------------------------------------------------
-- Sign placement
-- ---------------------------------------------------------------------------

-- | Zone-wide "fresh sign" hint when the deer actually walks through
-- the player's current location.
--
-- The durable, location-specific "treasure" signs are seeded at
-- scenario start (see 'initialSignTags' in Constants) and are never
-- produced by this axiom. This axiom only drops the legacy global
-- signTracks/signScrape tags, and only when the deer's recent move
-- touches the player's spot, so lookForDeer's "fresh tracks in the
-- mud" branch still has a trigger.
signPlacementAxiom :: CharId -> Axiom
signPlacementAxiom you = Axiom
  { axiomId       = ScenarioAxiom "signPlacement"
  , axiomPriority = 3
  , axiomEvaluate = \world _actions diff ->
      if huntOver world then [] else
      let playerLoc = charLocation you world
          deerTouchedHere =
            [ ()
            | ld <- diffLocations diff
            , locationDeltaChar ld == deer
            , Just (locationDeltaFrom ld) == playerLoc
                || Just (locationDeltaTo ld)   == playerLoc
            ]
          isSnow = weatherTag (WeatherDesc "Light Snow")
                     `elem` orToList (worldTags world)
          trackDuration  = if isSnow then 12 else 24
          scrapeDuration = 12
      in concatMap (const
           [ timed trackDuration  (AddWorldTag signTracks)
           , timed scrapeDuration (AddWorldTag signScrape)
           ]) deerTouchedHere
  }

-- ---------------------------------------------------------------------------
-- Sign discovery — automatic on arrival
-- ---------------------------------------------------------------------------

-- | When the player arrives at a location that holds one or more
-- undiscovered "treasure" signs (seeded at scenario init), mark them
-- discovered and narrate what they notice. Detail scales with the
-- player's Understanding stat.
--
-- No dedicated action is needed: walking in is enough. The axiom
-- fires on the tick where the diff contains a LocationDelta for the
-- player, so it never duplicates for a subsequent no-move tick.
signDiscoveryAxiom :: CharId -> Axiom
signDiscoveryAxiom you = Axiom
  { axiomId       = ScenarioAxiom "signDiscovery"
  , axiomPriority = 3
  , axiomEvaluate = \world _actions diff ->
      if huntOver world then [] else
      let arrivals =
            [ locationDeltaTo ld
            | ld <- diffLocations diff
            , locationDeltaChar ld == you
            ]
          exp' = experience you world
      in concatMap (discoverAt exp' world) arrivals
  }
  where
    discoverAt exp' world loc =
      let present = signsAt world loc
          found   = foundSignsAt world loc
          fresh   = [ t | t <- present, t `notElem` found ]
      in concatMap (discover loc exp') fresh

    discover loc exp' t =
      [ immediate (AddWorldTag (foundSignAt t loc))
      , immediate (Narrate (signProse exp' t))
      ]

    signProse e STracks
      | e <= 2    = "Hoof marks in the dirt. Something came through."
      | e <= 5    = "Tracks. Two-toed, pointed — deer. Older than today."
      | otherwise = "Tracks. A mature buck by the size. The drag between hoofs says he's not in a hurry."
    signProse e SScrape
      | e <= 2    = "Ground torn up. Something's been digging."
      | e <= 5    = "A scrape. Fresh dirt turned up. Buck territory."
      | otherwise = "Scrape line. He works this spot regularly. Urine-damp earth at the centre."
    signProse e SBed
      | e <= 2    = "A flat oval in the grass. Something slept here."
      | e <= 5    = "A bed. Deer-shaped, matted flat, hair in the grass."
      | otherwise = "A bed. Cold — he's been up hours. Body-sized oval pressed into the leaves."
    signProse e SRub
      | e <= 2    = "Bark stripped off a sapling. Scars on the wood."
      | e <= 5    = "A rub. Antler scars on the sapling, bark curled at the edges."
      | otherwise = "Rub line. Velvet shreds on the bark. He works this corridor when the rut starts."
    signProse e SDroppings
      | e <= 2    = "Dark pellets on the ground. Animal."
      | e <= 5    = "Droppings. Deer — oval pellets, dry on top."
      | otherwise = "Droppings. Dry on top, damp underneath. Yesterday at most."
    signProse e SHair
      | e <= 2    = "Hair caught on a branch."
      | e <= 5    = "Deer hair. Hollow-cored, brown with a grey tip. Buck coat."
      | otherwise = "Hair snagged on the wire. Coarse, tipped grey — mature buck, rubbing through."

-- | Track stillness while sitting. Increments each tick while PlayerSitting
-- is active, resets to 0 when the player moves.
stillnessAxiom :: CharId -> Axiom
stillnessAxiom you = Axiom
  { axiomId       = ScenarioAxiom "stillness"
  , axiomPriority = 4
  , axiomEvaluate = \world _actions diff ->
      let isSitting   = hasTag world playerSitting
          playerMoved = any (\ld -> locationDeltaChar ld == you) (diffLocations diff)
          current     = getStillness you world
      in if huntOver world then []
         else if playerMoved && current > 0
              -- Reset to 0: modify by negative current value
              then [modifyCharacterStatEffect you (Capacity Stillness) (negate current)]
         else [modifyCharacterStatEffect you (Capacity Stillness) 1
              | isSitting && not playerMoved && current < 10]
  }

-- ---------------------------------------------------------------------------
-- Experience
-- ---------------------------------------------------------------------------

-- | Gain experience from time in the bush and finding sign.
-- Each sign type grants +1 Understanding the first time it's discovered.
experienceAxiom :: CharId -> Axiom
experienceAxiom you = Axiom
  { axiomId       = ScenarioAxiom "experience"
  , axiomPriority = 5
  , axiomEvaluate = \world _actions diff ->
      let exp' = experience you world
          -- New day: +1 Understanding (slept on it, know the land better)
          newDay = dayNumberTag 1 `elem` diffWorldTagsAdded diff
                || dayNumberTag 2 `elem` diffWorldTagsAdded diff
          dayBonus = [modifyCharacterStatEffect you (Capacity Understanding) 1 | newDay && exp' < 8]
          -- First discovery of any sign: +1 (original FreshSign behavior)
          firstFreshSign = freshSign `elem` diffWorldTagsAdded diff
                        && not (hasTag world foundSignTracks)
                        && not (hasTag world foundSignBed)
                        && not (hasTag world foundSignRub)
                        && not (hasTag world foundSignScrape)
          freshBonus = [modifyCharacterStatEffect you (Capacity Understanding) 1 | firstFreshSign && exp' < 6]
          -- Per-type first-discovery bonuses
          firstTracks = signTracks `elem` diffWorldTagsAdded diff
                     && not (hasTag world foundSignTracks)
                     && exp' < 8
          trackBonus | firstTracks = [ modifyCharacterStatEffect you (Capacity Understanding) 1
                                     , immediate (AddWorldTag foundSignTracks) ]
                     | otherwise   = []
          firstScrape = signScrape `elem` diffWorldTagsAdded diff
                     && not (hasTag world foundSignScrape)
                     && exp' < 8
          scrapeBonus | firstScrape = [ modifyCharacterStatEffect you (Capacity Understanding) 1
                                      , immediate (AddWorldTag foundSignScrape) ]
                      | otherwise   = []
      in dayBonus ++ freshBonus ++ trackBonus ++ scrapeBonus
  }

-- ---------------------------------------------------------------------------
-- Day/night cycle
-- ---------------------------------------------------------------------------

-- | At 7 PM, force the player back to their truck.
nightfallAxiom :: CharId -> Axiom
nightfallAxiom you = Axiom
  { axiomId       = ScenarioAxiom "nightfall"
  , axiomPriority = 2
  , axiomEvaluate = \world _actions diff ->
      let evening  = timeTag 19 `elem` diffWorldTagsAdded diff
          startLoc = fromMaybe truckNorth (charLocation you world)
          truck    = nearestTruck startLoc
      in if evening && not (huntOver world)
         then [ immediate (Narrate "The light is going. Legal shooting is over. You mark your spot mentally and head back to the road.")
              , immediate (SetLocation you truck)
              , immediate (AddWorldTag nightFall)
              , immediate (AddWorldTag backAtTruck)
              , immediate (RemoveWorldTag deerSpotted)
              , immediate (RemoveWorldTag freshSign)
              , immediate (RemoveWorldTag movingFast)
              , immediate (RemoveWorldTag playerSitting)
              , immediate (AddTag you sleepingTag)
              ]
         else []
  }

-- | At 7 AM the next day, wake the player and let them re-enter.
dawnRule :: CharId -> AxiomRule
dawnRule you = AxiomRule
  { ruleId       = ScenarioAxiom "dawn"
  , rulePriority = 2
  , ruleTrigger  = WhenWorldTagAdded (timeTag 7)
  , ruleGuard    = All [ HasWorldTag backAtTruck
                       , Not (HasWorldTag deerKilled)
                       , Not (HasWorldTag deerGone)
                       , Not (HasWorldTag hunterShot)
                       ]
  , ruleTarget   = SpecificChar you
  , ruleEffects  = [ immediate (Narrate "Dawn. Frost on the windshield. Coffee from the thermos. The deer is out there somewhere.")
                   , immediate (RemoveWorldTag backAtTruck)
                   , immediate (RemoveWorldTag nightFall)
                   , immediate (RemoveTag you sleepingTag)
                   ]
  }

-- | Nearest truck based on current zone.
nearestTruck :: Location -> Location
nearestTruck loc = case locationZone loc of
  NorthRoad    -> truckNorth
  NorthField   -> truckNorth
  BushEdge     -> truckNorth    -- came in from north
  OakRidge     -> truckNorth
  WestRoad     -> truckWest
  SouthField   -> truckWest     -- closest to west
  PoplarStand  -> truckWest
  WillowBottom -> truckSouth
  SouthRoad    -> truckSouth
  FieldBreak   -> truckNorth    -- belt between the fields, closest to north road
  CreekBed     -> truckSouth    -- drains down toward south

-- ---------------------------------------------------------------------------
-- Tension
-- ---------------------------------------------------------------------------

tensionAxiom :: CharId -> Axiom
tensionAxiom _you = Axiom
  { axiomId       = ScenarioAxiom "tension"
  , axiomPriority = 10
  , axiomEvaluate = \world _actions _diff ->
      let has     = hasTag world
          current = getTension world
          target
            | has shotTaken     = 10
            | has deerSpotted && any (\(c,_) -> c /= deer && c /= Truth)
                                     (Map.toList (worldCharacters world))
                               = 9   -- deer + other hunter
            | has deerSpotted  = 8   -- buck fever
            | has freshSign    = 6   -- same zone, close
            | has backAtTruck  = 0   -- safe at truck
            | otherwise        = 2   -- in the bush
      in [setTension target | target /= current]
  }

-- ---------------------------------------------------------------------------
-- Weather narration
-- ---------------------------------------------------------------------------

weatherDesc :: WeatherDesc -> String
weatherDesc (WeatherDesc "Clear and Cold") = "Clear sky. Sharp cold. Your breath hangs in the air."
weatherDesc (WeatherDesc "Overcast")       = "Low clouds. The light has gone flat. Hard to judge distance."
weatherDesc (WeatherDesc "Light Snow")     = "Snow coming down. Big flakes, no wind. Everything muffled."
weatherDesc (WeatherDesc "Windy")          = "Wind picks up from the northwest. The poplars are moving. Good — it covers your noise."
weatherDesc w                              = "The weather shifts. " <> weatherName w <> "."

-- ---------------------------------------------------------------------------
-- Merge axiom: hunter arrival
-- ---------------------------------------------------------------------------

-- | When another hunter arrives at the player's location from an unaware
-- merge, narrate it. Only fires if they actually ended up on your section.
hunterArrivalMergeAxiom :: CharId -> MergeAxiom
hunterArrivalMergeAxiom you = MergeAxiom
  { mergeAxiomId       = ScenarioAxiom "hunterArrival"
  , mergeAxiomPriority = 2
  , mergeAxiomEvaluate = \world md ->
      let myLoc = Map.lookup you (worldLocations world)
          arrivedHere d = mdProvenance d == Unaware
                       && Just (locationDeltaTo (mdValue d)) == myLoc
                       && locationDeltaChar (mdValue d) /= you
      in [ immediate (Narrate "There's another hunter on the section. You didn't hear them come in.")
         | any arrivedHere (mergeLocations md) ]
  }
