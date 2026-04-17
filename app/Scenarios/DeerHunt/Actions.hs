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

-- | Per-location narration for intra-zone movement.
-- Three variants per location, each grounded in what that spot actually is.
withinZoneNarr :: Zone -> Location -> [String]
withinZoneNarr _z loc
  -- Roads — North
  | loc == truckNorth =
    [ "Your truck sits where you left it. Frost on the windshield."
    , "The truck's still there. Tailgate down, thermos on the bumper."
    , "Back at the truck. Engine's cold. You can see your breath."
    ]
  | loc == ditchNorth =
    [ "You drop down into the north ditch. Frozen cattails snap underfoot."
    , "The ditch is knee-deep here. Ice in the puddles."
    , "Down in the ditch. Dead grass and old fence wire."
    ]
  -- Roads — South
  | loc == truckSouth =
    [ "The south truck sits on the shoulder. Mud on the wheel wells."
    , "Back at the truck. Someone's tire tracks in the gravel beside yours."
    , "Your truck. The cab will be warm for about two minutes."
    ]
  | loc == ditchSouth =
    [ "South ditch. Deeper than the north one. Water at the bottom."
    , "You follow the south ditch. Tall grass, a few old beer cans."
    , "The ditch runs along the section. Quiet down here."
    ]
  -- Roads — West
  | loc == truckWest =
    [ "The west truck. Parked tight to the fence line."
    , "Back at the truck. Sun's low through the windshield."
    , "Your truck on the west road. Gravel dust on everything."
    ]
  | loc == ditchWest =
    [ "West ditch. Cattails thick on both sides."
    , "The ditch here is shallow. More of a low spot than a channel."
    , "You step down into the west ditch. Ice crunches."
    ]
  -- North Field
  | loc == nFieldEdge =
    [ "The field edge. Stubble meets treeline. Transition ground."
    , "You stop at the field edge. Open country ahead."
    , "Edge of the north field. Wind hits you as you leave the trees."
    ]
  | loc == stubbleRows =
    [ "Wheat stubble in rows. Dry stalks crack underfoot."
    , "Through the stubble rows. Each step crunches loud in the cold air."
    , "Stubble rows running north-south. Frost on the broken stems."
    ]
  | loc == hayBale =
    [ "A round bale, half-frozen to the ground. Good cover."
    , "The hay bale. Mice have been at it. You crouch behind it."
    , "Big round bale. You lean against it. Cold through your jacket."
    ]
  | loc == drainageDitch =
    [ "The drainage ditch cuts across the field. Thin ice on standing water."
    , "A low drainage channel. Your boots break through the ice crust."
    , "The ditch. Frozen mud and dead cattails. Lower than the field."
    ]
  | loc == cornStubbleStrip =
    [ "A strip of corn stubble, broken stalks knee-high. Cobs on the ground."
    , "Corn stubble. The deer come for the leftover kernels. Droppings everywhere."
    , "You walk the stubble strip. Dry leaves rattle when the wind moves through."
    ]
  -- South Field
  | loc == sFieldEdge =
    [ "South field edge. Flat and wide open."
    , "You reach the edge of the south field. Exposed."
    , "The south field stretches ahead. Nothing taller than your knees."
    ]
  | loc == stubbleFlat =
    [ "Canola stubble, flat as a table. You can see for half a mile."
    , "The stubble flat. Wind pushes across it unbroken."
    , "Short stubble. Nowhere to hide out here."
    ]
  | loc == fenceLine =
    [ "Old barbed wire on leaning posts. You follow it south."
    , "The fence line. Wire's loose between the posts."
    , "Along the fence. A meadowlark sitting on a post watches you pass."
    ]
  | loc == sloughEdge =
    [ "The slough. Frozen over but you don't trust it. Cattails at the edge."
    , "Edge of the slough. Dead reeds and thin ice."
    , "The slough is iced over. Muskrat push-ups dot the surface."
    ]
  | loc == sunflowerStubble =
    [ "Sunflower stubble. Broken heads on the ground, empty of seeds."
    , "The sunflower field after harvest. Dry stems snap as you step past."
    , "Sunflower stalks chest-high. A pheasant bolts up in front of you."
    ]
  -- Bush Edge
  | loc == thinPoplars =
    [ "Thin poplars, barely more than saplings. You can still see the field behind you."
    , "Young poplar grove. The trunks are close together."
    , "Into the thin poplars. Leaves gone, but the branches tangle overhead."
    ]
  | loc == brushPile =
    [ "Somebody piled brush here years ago. It's head-high now, frozen solid."
    , "The brush pile. Deadfall and old slash heaped together."
    , "A big brush pile. Rabbits have been using it — tracks everywhere."
    ]
  | loc == gameTrailEntrance =
    [ "A worn trail cuts into the heavier bush. Tracks in the mud."
    , "The game trail entrance. Branches broken at shoulder height."
    , "Where the game trail starts. The ground is packed hard."
    ]
  | loc == oldFence =
    [ "An old fence, half-collapsed. Wire rusted through in places."
    , "The old fence line. Posts gray and split. Property line, maybe."
    , "Rotting fence posts and sagging wire. The bush is taking it back."
    ]
  | loc == clearing =
    [ "A small clearing. Grass and sky. Quieter here."
    , "The clearing opens up. Sun hits the ground. A few stumps."
    , "You step into the clearing. Open ground in every direction."
    ]
  | loc == deadfall =
    [ "A big poplar came down here. Root ball sticking up six feet."
    , "The deadfall. You climb over the trunk. Bark peeling off in sheets."
    , "Downed tree blocking the way. You work around the root ball."
    ]
  | loc == stumpField =
    [ "Old cut block. Stumps in rows, some rotten through, some hard as bone."
    , "The stump field. Logging happened here years back. New growth coming up."
    , "You pick your way through the stumps. Every step tests for rot."
    ]
  | loc == hazelClump =
    [ "A thick clump of hazel brush. Nuts scattered in the leaf litter."
    , "Hazel bush, head-high and tangled. You have to duck under."
    , "The hazel clump. Branches whip across your face if you're not careful."
    ]
  -- Oak Ridge
  | loc == ridgeTop =
    [ "The ridge top. You can see over the canopy. Fields to the north, bush in every other direction."
    , "Up on the ridge. Wind is stronger here. The oaks are shorter, wind-bent."
    , "Top of the ridge. Good vantage. You catch your breath."
    ]
  | loc == oakThicket =
    [ "Dense oaks. Trunks close together, branches low."
    , "The thicket. You push through. Jacket catching on everything."
    , "Thick oaks. Dark under the canopy even with the leaves gone."
    ]
  | loc == scrapeLine =
    [ "A line of scrapes along the ridge. Dirt torn up in a row."
    , "The scrape line. Ground worked over by antlers. This is his territory."
    , "Scrapes in the dirt, one after another. The bark on the saplings is shredded."
    ]
  | loc == mossyHollow =
    [ "A dip in the ridge. Moss on everything. Damp and still."
    , "The hollow. Moss-covered rocks, standing water, old leaves."
    , "Down in the mossy hollow. Sheltered from the wind. Quiet."
    ]
  | loc == blowdown =
    [ "Storm damage. Three oaks down across each other."
    , "The blowdown. Trees snapped off at the base, roots in the air."
    , "Tangled blowdown. You pick your way through broken branches."
    ]
  | loc == deerTrail =
    [ "A deer trail worn smooth. Easy walking if you watch your head."
    , "The deer trail. Packed dirt, low tunnel through the brush."
    , "You follow the deer trail. Tracks everywhere. Fresh ones on top of old."
    ]
  | loc == acornGround =
    [ "Ground under the oaks is littered with acorns. Sign of feeding everywhere."
    , "Acorns underfoot. The shells crack when you step on them."
    , "The deer have been digging here. Torn leaves, acorn caps, fresh droppings."
    ]
  | loc == rockOutcrop =
    [ "A limestone outcrop juts out of the ridge. Lichen crusts the cold rock."
    , "Rock outcrop. You climb up a couple steps for the view. Wind hits harder up here."
    , "Grey stone pushing through the moss. A raven watches you from the top."
    ]
  -- Willow Bottom
  | loc == cattailMarsh =
    [ "Cattails as far as you can see. Your boots are wet immediately."
    , "The marsh. Brown cattail heads heavy with frost. Water under the ice."
    , "Into the cattails. Something moves ahead of you — muskrat, probably."
    ]
  | loc == willowTangle =
    [ "Willows grown together into a wall. You push through."
    , "The tangle. Willow whips everywhere. Wet branches across your face."
    , "Willow tangle. You can barely see ten feet ahead."
    ]
  | loc == creekCrossing =
    [ "The creek. Ankle-deep, running clear over round stones."
    , "You cross the creek. Water fills your boot prints on the other side."
    , "Creek crossing. Ice along the edges, open water in the middle."
    ]
  | loc == mudFlat =
    [ "Mud flat. Every step sucks at your boots."
    , "The mud flat. Goose tracks and old deer prints frozen in the muck."
    , "Flat mud, cracked where it's dried. Your boot prints fill with water."
    ]
  | loc == beaverDam =
    [ "The beaver dam. Sticks and mud piled four feet high. Water behind it."
    , "A beaver dam. You walk across the top, testing each step."
    , "The dam. Chewed sticks and packed mud. Pond backing up behind it."
    ]
  | loc == dryHummock =
    [ "A raised hummock. Dry ground at last. Grass and a few scrubby willows."
    , "Up on the hummock. Your boots stop squelching. Solid ground."
    , "The hummock. An island of dry ground in the wet. Good spot to rest."
    ]
  | loc == sedgeMeadow =
    [ "A sedge meadow, tawny grass bent flat by the wind."
    , "The sedge is thigh-high and soft underfoot. Quiet walking."
    , "Sedge meadow. Frost still on the blades where the sun hasn't hit."
    ]
  | loc == islandWillow =
    [ "A clump of willows on a small island in the wet. You wade the last few steps."
    , "The island willow. Tight-grown stems, a cleared bed inside — deer bed, recent."
    , "You push into the island stand. Dry ground in the middle, wet all around."
    ]
  -- Poplar Stand
  | loc == poplarAlley =
    [ "Tall poplars in a line, like a hallway. Light through the canopy."
    , "The poplar alley. White trunks in a row. Easy walking on the packed leaves."
    , "Between the poplars. The trunks are straight and pale. Your footsteps echo."
    ]
  | loc == birchClump =
    [ "A clump of birch mixed with the poplars. White bark peeling."
    , "Birch trees here. Paper bark curling off in sheets."
    , "The birch clump. Thinner trunks, more light getting through."
    ]
  | loc == rubLine =
    [ "Rubs on the saplings. Bark stripped clean. He's been working this line."
    , "The rub line. Every third sapling is scarred. Fresh wood showing."
    , "Rub marks on the trees. Velvet shreds hanging from the bark."
    ]
  | loc == openUnderstory =
    [ "Open understory. Big poplars, nothing beneath them but old leaves."
    , "The canopy is high here. Easy walking. Ground is soft and quiet."
    , "Under the big poplars. Open and still. You can see a long way through the trunks."
    ]
  | loc == gameTrailFork =
    [ "The trail splits here. One way north into the bush, one south toward the poplar."
    , "A fork in the game trail. Both paths well-worn."
    , "Trail fork. Tracks going both directions. Fresh droppings at the junction."
    ]
  | loc == windbreak =
    [ "The windbreak. Two rows of spruce planted as a shelterbelt."
    , "Behind the windbreak. Sheltered from the west wind. Quiet."
    , "Dense spruce windbreak. Dark underneath. Needles soft underfoot."
    ]
  | loc == aspenGrove =
    [ "A grove of young aspen, trunks pale and close. Leaves long fallen."
    , "The aspen grove. Small trees, thick stand. You have to turn sideways in places."
    , "Aspens shiver overhead where the wind catches the tops. Quieter down below."
    ]
  | loc == stumpRow =
    [ "A row of old cut stumps, lined up where somebody pulled a fence."
    , "The stump row. Half-rotted, moss on the north sides."
    , "Stumps in a line. You step from one to the next to keep your boots dry."
    ]
  -- Field Break
  | loc == lonePoplar =
    [ "A single old poplar, twice the size of anything around it. Crows roost here."
    , "The lone poplar. Landmark tree. You can see it from either field."
    , "Under the big poplar. Bark black with age. Deer have rubbed the base smooth."
    ]
  | loc == stoneRow =
    [ "A row of field stones piled by some old farmer. Moss and lichen now."
    , "The stone row. Rocks cleared off the field decades back. Good rest against."
    , "Old stone pile. Voles running between the rocks. Fox sign on top."
    ]
  | loc == brushyGap =
    [ "A brushy gap between the fields. Wild rose and saskatoon grown up thick."
    , "The gap. Low brush, thorns catching your sleeves. Deer move through here."
    , "Brushy ground. Chest-high cover in places. Tracks in the bare patches."
    ]
  | loc == fenceCorner =
    [ "The corner where two fences meet. Wire twisted around a rotting post."
    , "Fence corner. Old wire coiled in the grass. Ground packed hard at the post."
    , "The corner post leans. Staples rusted through. Deer cross right here every time."
    ]
  -- Creek Bed
  | loc == creekMouth =
    [ "Where the creek runs out of the poplar stand into the bottom. Wet mud."
    , "The creek mouth. Cold water seeping over your boot tops."
    , "Creek flattens out here. Slow water, fine silt, deer tracks along the bank."
    ]
  | loc == gravelBar =
    [ "A gravel bar on the inside of the bend. Round stones, all sizes."
    , "The gravel bar. Dry footing in the middle of the wet. Worth resting on."
    , "Gravel pushed up by last spring's flood. Tracks in the sand between the stones."
    ]
  | loc == alderThicket =
    [ "Alder thick along the creek. Branches low, leaves stuck to everything."
    , "The alder thicket. Stems crossed and tangled. You break your way through."
    , "Alder bush. Rabbit tracks everywhere in the mud. Deer came through too."
    ]
  | loc == creekBend =
    [ "The creek bends sharp here. Deep water on the outside, shallow on the inside."
    , "Bend in the creek. Undercut bank, roots hanging into the water."
    , "Sharp bend. Current slows. A trout hangs in the deep pool, watching."
    ]
  | loc == driftwoodPile =
    [ "A tangle of driftwood jammed up against the bank. Old flood wrack."
    , "The driftwood pile. Bleached logs and branches, higher than your head."
    , "Driftwood heaped on a snag. Mink sign in the mud beside it."
    ]
  | otherwise =
    let Location name = loc
    in [ "You keep moving. " <> name <> "."
       , "You push on. " <> name <> "."
       , "Onward. " <> name <> "."
       ]

-- | Narration for crossing between zones. Location-specific where it matters,
-- with zone-generic fallbacks.
crossZoneNarr :: Zone -> Zone -> Location -> [String]
crossZoneNarr from to loc = case (from, to, loc) of
  -- Specific cross-zone transitions
  (NorthRoad, NorthField, _) ->
    [ "You climb down off the road into the field. Stubble stretches ahead."
    , "Off the gravel, into the field. The ground changes under your boots."
    ]
  (NorthField, NorthRoad, _) ->
    [ "You step back up onto the road grade. Gravel underfoot again."
    , "Back to the road. Your boots are muddy."
    ]
  (NorthField, BushEdge, _) ->
    [ "The field ends and the bush begins. You push through the first branches."
    , "Stubble gives way to saplings. The treeline closes in."
    ]
  (BushEdge, NorthField, _) ->
    [ "The bush opens up. Open field, flat to the road."
    , "You step out of the trees. Wide open ahead. Wind hits you."
    ]
  (BushEdge, OakRidge, _) ->
    [ "The ground rises. Oaks replace the poplars. Heavier timber."
    , "Uphill now, into the oaks. The ridge is ahead."
    ]
  (OakRidge, BushEdge, _) ->
    [ "Down off the ridge. The oaks thin to mixed bush."
    , "You come down off the ridge. Lighter timber, easier walking."
    ]
  (BushEdge, PoplarStand, _) ->
    [ "South through the bush edge into taller poplars."
    , "The bush changes character. Poplars now, taller and straighter."
    ]
  (PoplarStand, BushEdge, _) ->
    [ "North out of the poplars. Mixed bush again."
    , "The poplars give way to scrubby mixed brush."
    ]
  (OakRidge, WillowBottom, _) ->
    [ "The ridge drops off. Wet ground ahead. You can smell the marsh."
    , "Down into the bottom. The ground goes soft immediately."
    ]
  (WillowBottom, OakRidge, _) ->
    [ "Uphill out of the wet. The ground firms up. Oaks overhead."
    , "You climb out of the bottom. Dry ground and oak trees."
    ]
  (PoplarStand, WillowBottom, _) ->
    [ "The poplars end at the creek. Wet ground beyond."
    , "Out of the poplar and into the willows. Boots sinking."
    ]
  (WillowBottom, PoplarStand, _) ->
    [ "You leave the wet ground and climb into the poplar stand."
    , "Up out of the bottom. Dry leaves and pale trunks."
    ]
  (SouthField, PoplarStand, _) ->
    [ "You leave the open field and push into the poplar stand."
    , "Out of the stubble and into cover. Poplars close overhead."
    ]
  (PoplarStand, SouthField, _) ->
    [ "The poplars end. Open stubble field stretches ahead."
    , "Trees thin out. Wide open. You can see the road."
    ]
  (WestRoad, SouthField, _) ->
    [ "You leave the road and head east into the field."
    , "Off the west road. Stubble underfoot."
    ]
  (SouthField, WestRoad, _) ->
    [ "You walk out to the west road. Gravel again."
    , "Back on the road. Your boots leave mud on the gravel."
    ]
  (SouthRoad, SouthField, _) ->
    [ "North off the south road into the field."
    , "You leave the road and head into the stubble."
    ]
  (SouthField, SouthRoad, _) ->
    [ "South to the road. Open ground the whole way."
    , "You walk back out to the south road."
    ]
  (NorthField, FieldBreak, _) ->
    [ "The stubble ends at a narrow belt of trees. You step into cover."
    , "South off the north field, into the tree break. Wind drops behind the poplars."
    ]
  (FieldBreak, NorthField, _) ->
    [ "Out of the tree break. North field opens ahead. Stubble and sky."
    , "You leave the brushy belt. Open field again, wind picking up."
    ]
  (SouthField, FieldBreak, _) ->
    [ "Off the south field and into the tree break. Cover at last."
    , "You climb the shallow bank into the belt of trees between the fields."
    ]
  (FieldBreak, SouthField, _) ->
    [ "Out of the break. South field stretches away, flat to the road."
    , "You step out of the trees. Open stubble ahead."
    ]
  (BushEdge, FieldBreak, _) ->
    [ "The bush thins into the narrow break between the fields."
    , "West through scrub into the tree belt. Ground opens up a little."
    ]
  (FieldBreak, BushEdge, _) ->
    [ "East out of the break, into heavier bush."
    , "The tree belt ends and the bush thickens. Deeper cover now."
    ]
  (PoplarStand, CreekBed, _) ->
    [ "The poplars drop away into the creek bed. Water audible ahead."
    , "Down off the poplar flat, into the creek. Wet ground starts here."
    ]
  (CreekBed, PoplarStand, _) ->
    [ "Up out of the creek bed. Dry leaves and poplar trunks ahead."
    , "You climb the bank out of the creek. Boots muddy, trees close in."
    ]
  (WillowBottom, CreekBed, _) ->
    [ "The willows open at the creek. You follow the bank upstream."
    , "Out of the tangle, down to the creek itself. Water running clear."
    ]
  (CreekBed, WillowBottom, _) ->
    [ "Off the creek bank, into the willow tangle. Stems close in fast."
    , "You leave the creek bed. Willow whips across your face."
    ]
  -- Generic fallbacks by zone type
  _ -> let Location name = loc in case (from, to) of
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
  (PoplarStand, CreekBed)     -> Just "Creek bed to the east."
  -- From Field Break
  (FieldBreak, NorthField)    -> Just "North field to the north."
  (FieldBreak, SouthField)    -> Just "South field to the south."
  (FieldBreak, BushEdge)      -> Just "Bush edge to the east."
  -- To Field Break
  (NorthField, FieldBreak)    -> Just "Tree break to the south."
  (SouthField, FieldBreak)    -> Just "Tree break to the north."
  (BushEdge, FieldBreak)      -> Just "Tree break to the west."
  -- From Creek Bed
  (CreekBed, PoplarStand)     -> Just "Poplar stand to the west."
  (CreekBed, WillowBottom)    -> Just "Willow bottom to the east."
  -- To Creek Bed
  (WillowBottom, CreekBed)    -> Just "Creek bed to the west."
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
