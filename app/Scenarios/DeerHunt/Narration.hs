-- | Narration pools for procedural hunt maps.  Keyed by terrain class
-- and (for intra-zone arrivals) by 'PositionHint'; cross-zone
-- transitions are keyed by the pair of classes you're moving between.
-- Every pool has multiple variants so repeated visits read differently
-- and two instances of the same class (say, North Field and South
-- Field) don't feel identical to traverse.
--
-- All prose is written in terms of the terrain itself, not the
-- location's proper name — the top-bar location label already carries
-- identity.  This lets the generator pick whatever name it likes
-- without prose rotting as vocabularies change.
module Scenarios.DeerHunt.Narration
  ( intraNarration
  , crossNarration
  , sensoryFragment
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Engine.Author.Transition (TransitionPool (..), crossVariants)
import           Scenarios.DeerHunt.Generation (TerrainClass(..))
import           Scenarios.DeerHunt.World      (PositionHint(..))

-- | The hunt's transition narration table.  Used by 'huntGraph' via
-- 'transitionNarration'; intra/cross lookups go through the engine
-- helpers, with one hunt-specific quirk preserved by 'intraNarration'
-- below (fall back to the class's Interior pool when the specific
-- @(class, hint)@ cell is empty).
huntTransitionPool :: TransitionPool TerrainClass PositionHint
huntTransitionPool = TransitionPool
  { tpIntra    = intraPool
  , tpCross    = crossPool
  , tpFallback = fallbackPool
  }

-- | Look up intra-zone narration, falling back through the class's
-- Interior pool before yielding to the table's neutral fallback.
-- Engine's 'intraVariants' would skip the Interior step.
intraNarration :: TerrainClass -> PositionHint -> [String]
intraNarration cls hint =
  case Map.lookup (cls, hint) intraPool of
    Just xs@(_:_) -> xs
    _ ->
      case Map.lookup (cls, Interior) intraPool of
        Just xs@(_:_) -> xs
        _             -> fallbackPool

-- | Look up cross-zone narration via the engine helper.
crossNarration :: TerrainClass -> TerrainClass -> [String]
crossNarration = crossVariants huntTransitionPool

-- | Neutral variants used when a specific pool is empty.  Should never
-- actually appear in play with the full pools below, but keeps
-- 'NarrationPool' well-formed.
fallbackPool :: [String]
fallbackPool =
  [ "You move up, watching the ground."
  , "New ground. You settle."
  , "The terrain changes under your boots."
  , "You come to a stop and listen."
  ]

-- ---------------------------------------------------------------------------
-- Intra-zone pool — 10 variants per (class, hint) cell.  Keys:
-- (class, position hint).
-- ---------------------------------------------------------------------------

intraPool :: Map (TerrainClass, PositionHint) [String]
intraPool = Map.fromList
  [ -- ========== FIELD ==========
    ((CField, Interior),
      [ "Stubble every direction. Your boots crack dry stalks."
      , "Open ground. Wind moves through the rows, low and constant."
      , "The field keeps going. You're exposed out here and you feel it."
      , "Flat ground, combed over. Nothing to hide behind for a hundred yards."
      , "Stubble to the horizon. A hawk works the updraft, far off."
      , "You stop in the middle of the field. The sky takes up most of the world."
      , "Dry stalks crunch with each step. No deer would come through here in daylight."
      , "Nothing but stubble. The wind smells like cut straw and iron cold."
      , "The field hums with small things — mice in the rows, something chittering."
      , "Flat, open, quiet. You can see a long way. So can anything watching you."
      ])
  , ((CField, Edge),
      [ "The field edge. Stubble thinning where something else starts."
      , "Ground gives way just ahead. The field ends and the air changes."
      , "You come to the field's border. Cover one direction, open the other."
      , "Stubble thins. A different texture starts a few yards off."
      , "Edge of the field. You can see where the open stops."
      , "The field runs out. Line of something darker up ahead."
      , "You stand where the stubble frays. Decision ground."
      , "Field gives way. The horizon crowds in closer here."
      , "You pause at the edge. The wind changes pitch as you step."
      , "End of the open. You feel yourself narrowing your shoulders."
      ])
  , ((CField, Bridge),
      [ "Field meets the next thing. You crouch instinctively."
      , "A seam. Stubble one side, something else the other."
      , "You're at a fold in the land. Open behind, closer up ahead."
      , "Boundary ground. The kind of place a deer crosses at dusk."
      , "The field ends right here. You can step either way from this spot."
      , "A threshold. You can feel the two worlds meeting under your boots."
      , "Edge on edge. You're visible from the field and exposed to the cover."
      , "The field stops abruptly. You wait a beat before crossing."
      , "A line. You're on it. Either side reads different."
      , "You stand at the hinge. Open behind you, hidden ahead."
      ])

    -- ========== ROAD ==========
  , ((CRoad, Interior),
      [ "Gravel underfoot. The road runs straight in both directions."
      , "You're on the road. Ditch one side, ditch the other."
      , "Road under you. You can see a long way, and be seen."
      , "Gravel and dust. A section road smells like nothing much at all."
      , "You walk on the road. Nobody's passed this way in hours."
      , "The road keeps going. Farmstead somewhere, poplars somewhere."
      , "Middle of the road. You feel conspicuous, and you are."
      , "Graded gravel, fence posts. You're an obvious shape out here."
      , "The road gives under your boots — loose stone, packed dust."
      , "You stand on the section road. Nothing's coming; nothing's moving."
      ])
  , ((CRoad, Edge),
      [ "Road's edge. Ditch drops off beside you."
      , "Shoulder of the road. You could step down and into the ditch."
      , "Edge of the gravel. Grass starts past the shoulder."
      , "The road's lip. You can see the low ditch water from here."
      , "Gravel gives way to ditch grass. You stop, listen."
      , "You stand on the shoulder. Fence posts stagger off toward a field."
      , "Road edge, grass verge, ditch. A clear transition."
      , "The ditch cuts a dark line beside the gravel. You look down it."
      , "You come to the road's margin. Cover's right there if you need it."
      , "Edge of the road. Something moved in the ditch grass — wind, probably."
      ])
  , ((CRoad, Bridge),
      [ "Road meets cover. You're conspicuous for the next ten seconds."
      , "Shoulder with a field coming up. You'd want to cross quick."
      , "The gravel runs into something green. You slow down."
      , "End of the road's openness. Cover begins across a short margin."
      , "Road gives way to field or bush. You stand at the edge."
      , "A crossing point. You feel watched even if nothing's there."
      , "The road opens into different ground. Your boots know the switch before you do."
      , "You're at a hinge: road behind, field or bush ahead."
      , "Gravel thins out. You're leaving the road."
      , "The road ends visibly here. The terrain takes over."
      ])

    -- ========== BUSH ==========
  , ((CBush, Interior),
      [ "You're deep in it. Canopy closes overhead, sightlines measured in yards."
      , "Poplars, hazel, deadfall. Every step makes noise."
      , "Cover holds you. The sky's a patchwork through the branches."
      , "Thick bush. You couldn't see a deer at twenty paces."
      , "The bush encloses you. A stick cracks — your own."
      , "Deep cover. You slow your breathing just from being here."
      , "Tangle on tangle. You duck, weave, set your feet careful."
      , "You're swallowed by it. Everything is close; everything is green-brown."
      , "Canopy above, deadfall below. The bush keeps its own time."
      , "You move slow. The bush makes you earn every step."
      ])
  , ((CBush, Edge),
      [ "The bush thins where you are. Light reaches the ground differently."
      , "Edge of the cover. You can see a clearing through the trees."
      , "You're where the bush is just bush — not thicket, not open."
      , "The canopy breaks ahead. Something brighter's nearby."
      , "Bush gives a little. You can almost see across to something else."
      , "You stand where the bush lets the light in. Yards, not feet, visible."
      , "Thinner here. The branches give up some of their grip on the air."
      , "You come to a place the bush respects less. An opening somewhere."
      , "Light coming through in wedges. The bush is ending soon."
      , "You pause. The trees ahead are standing more apart."
      ])
  , ((CBush, Bridge),
      [ "Bush edge. Field or ridge starts just past the last of the trees."
      , "You step to where the cover breaks. Open ground one way, still thick the other."
      , "The bush ends cleanly here. A line, almost. Two worlds touching."
      , "Last of the poplars before something else opens up."
      , "You stand where the bush lets you go. Different ground next."
      , "Cover ends. You feel the exposure coming a second before it arrives."
      , "The treeline. Beyond it, a new kind of quiet."
      , "Bush gives way. You check the ground for tracks before stepping out."
      , "A clean seam. You could wait here all day and watch the edge."
      , "The cover ends in a row. You stop, listen, decide."
      ])

    -- ========== RIDGE ==========
  , ((CRidge, Interior),
      [ "Higher ground. You can see over the canopy of whatever's below."
      , "On the ridge. The wind finds you sooner up here."
      , "You're up. The land falls away in two directions."
      , "Ridgetop ground. Rocks, moss, the occasional oak."
      , "The ridge carries you along its spine. Nothing close; everything distant."
      , "You stand on the high ground. The sky does more of the work up here."
      , "Up above the low country. The bush reads like texture, not terrain."
      , "Ridge underfoot. Dry leaves and outcrop."
      , "Elevation changes what you hear. Everything's further and softer."
      , "Up here the wind is more honest. It brings smells from the whole section."
      ])
  , ((CRidge, Edge),
      [ "The ridge shoulder. Ground starts pitching down."
      , "You're on the slope. Not up, not down. In between."
      , "Edge of the high ground. You can see farther than you did below."
      , "The ridge ends in a soft shoulder. Rocks loose underfoot."
      , "Slope. Cover thicker on the low side, thinner up above."
      , "You come to where the ridge gives up its height."
      , "A shelf on the ridge's side. You could sit here and watch a while."
      , "The ridge's edge. One step and you'd be lower ground."
      , "You balance on the shoulder. Drop to your right, ridge to your left."
      , "Where the ridge turns into something else. Oak gives way to poplar."
      ])
  , ((CRidge, Bridge),
      [ "The ridge comes to an end. Next class starts right here."
      , "Down off the high ground. The ridge gives up its drainage."
      , "End of the oak ridge. Lower country begins beyond the last rocks."
      , "You stand where the ridge runs out. The whole character of the land changes."
      , "Ridge ends. Ground level from here."
      , "The oaks stop. Something else starts. You feel the ground smooth out."
      , "Ridge meets lower ground. A clean transition, visible under your boots."
      , "You're at the ridge's foot. Other terrain waits."
      , "End of the climb, or the descent, depending on how you came."
      , "The high ground gives way. You're about to be somewhere else."
      ])

    -- ========== CREEK ==========
  , ((CCreek, Interior),
      [ "Creek bed underfoot. Water sound, constant and low."
      , "Down in the creek. Alders close overhead, air colder."
      , "You're in the bottom. The water does most of the talking."
      , "Gravel and mud. The creek's cut you out a corridor."
      , "The creek holds its own weather. Wetter air, stiller."
      , "You move along the water. Your boots print clear in the mud."
      , "Down here the world shrinks to the banks and the sound."
      , "Creek bottom. A duck flushes somewhere downstream."
      , "Still water, moving water. Deer tracks old in the mud."
      , "You walk the creek. The land above is almost a different country."
      ])
  , ((CCreek, Edge),
      [ "Top of the bank. Water below, dry ground above."
      , "Creek's edge. You could drop down or stay up."
      , "The bank. Alder, willow, the water a few feet below."
      , "You stand where the creek's influence starts. Mud here, dry there."
      , "Bank ground. You can smell the water from here."
      , "The creek's margin. Grass still stiff with frost just past the bank."
      , "You come to the edge of the cut. The water sound is clear."
      , "Above the creek. The land starts its drop just past your boots."
      , "Bank edge. You can look down at the water without being in it."
      , "You're at the seam. Wet ground, dry ground, one long step apart."
      ])
  , ((CCreek, Bridge),
      [ "The creek turns into something else. Poplar or willow takes over."
      , "Edge of the creek bed. You could cross into cover right here."
      , "Water behind, dry ground ahead. The creek's handing you off."
      , "End of the creek bed, start of the bush."
      , "The banks open up. You stand where the creek becomes less important."
      , "The alders give up. Different trees start."
      , "Creek bed opens out. The land widens and lifts."
      , "You step up from the water. Cover receives you."
      , "Transition. The water sound fades behind you."
      , "End of the creek. Whatever comes next is drier."
      ])
  ]

-- ---------------------------------------------------------------------------
-- Cross-zone pool — 6 variants per ordered (from, to) pair
-- ---------------------------------------------------------------------------

crossPool :: Map (TerrainClass, TerrainClass) [String]
crossPool = Map.fromList (crossPairs ++ autoPairs)

-- | All ordered class-pair arrivals.  Twelve of the 30 non-identity
-- pairs are explicitly-written below; the rest inherit a sensible
-- default built from origin + destination keywords.  This keeps the
-- prose specific for the combinations the player actually makes often
-- (field ↔ bush, road ↔ field, creek ↔ bush) while still sounding
-- authored for the rarer transitions.
crossPairs :: [((TerrainClass, TerrainClass), [String])]
crossPairs =
  [ ( (CField, CBush)
      , [ "Out of the open and into cover. The canopy closes overhead."
        , "Stubble gives way to poplars. Your footsteps change sound."
        , "You leave the field. The air cools in the bush's shade."
        , "Field ends. Bush starts. The world narrows."
        , "Cover takes you. You feel less watched and more watched at once."
        , "The open is behind you. Branches start to shape your path."
        ] )
    , ( (CBush, CField)
      , [ "Out of the bush. The sky opens and the ground goes flat."
        , "You push through the last of the branches into open stubble."
        , "Cover ends. You blink at the sudden light and space."
        , "The bush lets you go. The field takes you in."
        , "You step clear of the poplars. Nothing close; everything far."
        , "Out of the thick. The open lies ahead, dry and quiet."
        ] )
    , ( (CField, CRoad)
      , [ "Stubble to gravel. The road takes your weight differently."
        , "You step off the field onto the road's shoulder."
        , "Dry stalks give way to graded stone. The ditch runs beside you."
        , "Field ends at the road. You're back on something flat and made."
        , "Out of the stubble and onto the gravel. Fence posts lean off into the distance."
        , "You leave the field. The road carries you now."
        ] )
    , ( (CRoad, CField)
      , [ "Off the gravel and into the stubble. The ground crackles."
        , "You step from the road into the field. The open swallows you."
        , "Road behind. Field ahead. Ditch to clear first."
        , "Gravel gives way to dry stalks. You're in the field now."
        , "You leave the road. Stubble for as far as you can see."
        , "Off the shoulder, into the stubble. The wind finds you immediately."
        ] )
    , ( (CBush, CRidge)
      , [ "Up out of the bush. The ground starts to pitch."
        , "Cover thins as you climb. Oaks ahead, poplars behind."
        , "You leave the tangle for the ridge. Light comes in cleaner."
        , "Bush gives up its grip. The ridge rises under you."
        , "Deadfall to slope. Each step gets you higher."
        , "Out of the bush, onto the ridge. The air moves faster."
        ] )
    , ( (CRidge, CBush)
      , [ "Down off the ridge and into the thick."
        , "The high ground ends. The bush closes in."
        , "You drop off the oak ridge. Branches start to whip at your sleeves."
        , "Ridge behind. Poplars ahead. You're lower now."
        , "You leave the ridgetop. Cover takes you back."
        , "Off the rocks, into the leaves. The canopy rises above you."
        ] )
    , ( (CCreek, CBush)
      , [ "Up out of the creek bed. The alders give up."
        , "You leave the water behind. Dry ground receives you."
        , "Creek to bush. The air warms a degree or two."
        , "Bank, then brush. You step up into cover."
        , "Out of the cut and into the trees."
        , "You rise from the creek. The poplars stand a little straighter here."
        ] )
    , ( (CBush, CCreek)
      , [ "Down off the bank, into the creek bed."
        , "The bush ends at the water. You hear it before you see it."
        , "You drop down. The creek's corridor takes you."
        , "Cover to water. The air cools, the ground softens."
        , "Out of the thick and into the wet."
        , "You step down to the creek. Alders lean overhead now."
        ] )
    , ( (CField, CCreek)
      , [ "Stubble drops away into the creek bed."
        , "Field ends at the bank. Water runs low below you."
        , "You step down. The creek opens up under the field's edge."
        , "Out of the open, into the cut. The air gets wetter."
        , "Field behind, water ahead. Willows thicken as you descend."
        , "You drop from the field to the creek. Everything closes in."
        ] )
    , ( (CCreek, CField)
      , [ "Out of the creek bed and up onto stubble."
        , "You climb the bank. Open field spreads above."
        , "Creek behind. Sky ahead. Field all the way to the fence line."
        , "You step up from the water. The world widens again."
        , "Alders to stubble. Sudden light."
        , "Up out of the cut, into the open. You feel bigger up here."
        ] )
    , ( (CField, CRidge)
      , [ "The field climbs, and then it's no longer a field — it's the ridge."
        , "Stubble gives up to slope. You start to feel the elevation."
        , "You leave the flat. The ridge rises under you."
        , "Out of the open into something higher."
        , "The field bends up. The ground takes a different shape."
        , "You climb off the stubble. The ridge begins."
        ] )
    , ( (CRidge, CField)
      , [ "Down off the ridge, onto stubble."
        , "The slope ends. The field opens out."
        , "You leave the high ground. Stubble spreads below."
        , "Ridge behind, field ahead. You can see a long way."
        , "You step off the last of the rocks into the open."
        , "Off the ridge and into the field. The sky doubles in size."
        ] )
    ]

-- | Default variants for any class pair not explicitly listed above.
-- Generates six lines per pair using simple origin/destination
-- keywords, enough to sound authored when a rare transition happens.
autoPairs :: [((TerrainClass, TerrainClass), [String])]
autoPairs =
  [ ((from, to), defaultTransition from to)
  | from <- allClasses
  , to   <- allClasses
  , from /= to
  , (from, to) `notElem` explicitPairs
  ]
  where
    allClasses = [CField, CRoad, CBush, CRidge, CCreek]
    explicitPairs =
      [ (CField, CBush), (CBush, CField)
      , (CField, CRoad), (CRoad, CField)
      , (CBush, CRidge), (CRidge, CBush)
      , (CCreek, CBush), (CBush, CCreek)
      , (CField, CCreek), (CCreek, CField)
      , (CField, CRidge), (CRidge, CField)
      ]

defaultTransition :: TerrainClass -> TerrainClass -> [String]
defaultTransition from to =
  let fromW = classAdj from
      toW   = classAdj to
  in [ "Out of the " <> fromW <> " and into the " <> toW <> "."
     , fromW <> " behind you. " <> toW <> " ahead."
     , "You leave the " <> fromW <> ". The " <> toW <> " begins."
     , "The " <> fromW <> " ends. The " <> toW <> " takes over."
     , "You step from " <> fromW <> " to " <> toW <> ". The ground changes."
     , "End of the " <> fromW <> ". Start of the " <> toW <> "."
     ]

-- | Single-word descriptor for a terrain class, used in the default
-- cross-pool strings.
classAdj :: TerrainClass -> String
classAdj CField = "open"
classAdj CRoad  = "gravel"
classAdj CBush  = "bush"
classAdj CRidge = "ridge"
classAdj CCreek = "creek bed"
classAdj CEmpty = "ground"

-- ---------------------------------------------------------------------------
-- Sensory fragments — the fleeting one-liner beside each revealed choice
-- ---------------------------------------------------------------------------

-- | Pick one short fragment for a destination based on its class,
-- position hint, and a per-arrival seed.  Rendered under the label
-- while that label is the most-recently revealed choice; replaced by
-- the next choice's fragment as the reveal continues.
--
-- These are deliberately sparse — four or five words — so a row under
-- a neighbour label doesn't feel like a second screen of text.
sensoryFragment :: TerrainClass -> PositionHint -> Int -> String
sensoryFragment cls hint salt =
  let variants   = Map.findWithDefault [] (cls, hint) sensoryPool
      sameClass  = Map.findWithDefault [] (cls, Interior) sensoryPool
      -- If the specific (class, hint) cell has no variants (or the class
      -- isn't represented at all, e.g. CEmpty), fall back through the
      -- class's Interior pool and then to a generic neutral pool so
      -- every location always gets *some* whisper.
      pool       = firstNonEmpty [variants, sameClass, sensoryNeutral]
  in case pool of
       [] -> ""
       xs -> xs !! (abs salt `mod` length xs)
  where
    firstNonEmpty = foldr (\x acc -> if null x then acc else x) []

-- | Empty — if the specific pool and class Interior pool are both
-- empty, better to show no fragment than a generic one.  The previous
-- neutral pool produced lines like "a patch of the section" that
-- read as filler and broke the evocative feel of the specific pools.
sensoryNeutral :: [String]
sensoryNeutral = []

sensoryPool :: Map (TerrainClass, PositionHint) [String]
sensoryPool = Map.fromList
  [ ((CField, Interior),
      [ "wind through the stalks"
      , "flat, exposed — deer will see you first"
      , "mouse runs in the rows, owls work the edges"
      , "stubble to the horizon, sightlines forever"
      , "no cover for a hundred yards"
      , "hot sun on the stubble keeps the deer bedded"
      ])
  , ((CField, Edge),
      [ "stubble thinning ahead"
      , "line of something darker — worth closing on"
      , "edge of the open; scent carries here"
      , "where deer step out to feed at dusk"
      , "wind changes pitch here; check your direction"
      ])
  , ((CField, Bridge),
      [ "open behind, cover ahead"
      , "a seam the deer use at dusk"
      , "scent split — carries to both sides"
      , "visible from both sides while you cross"
      , "dry stalks, then shadow — worth sitting just inside"
      ])
  , ((CRoad, Interior),
      [ "gravel warm from the sun"
      , "fence posts leaning north — wind's been steady"
      , "no tire tracks since dawn; nobody else out"
      , "every step kicks stone; deer hear you a quarter mile"
      , "easy walking, no cover either way"
      ])
  , ((CRoad, Edge),
      [ "ditch water black and still"
      , "grass frayed at the shoulder — cover if you need it"
      , "cold pools in the ditch; scent hangs low here"
      , "loose stone, frost under it — footing's bad"
      ])
  , ((CRoad, Bridge),
      [ "gravel running into green — cover starts just past the shoulder"
      , "brief open stretch; cross it quick"
      , "about ten seconds exposed before you reach cover"
      , "cover close on the far side"
      ])
  , ((CBush, Interior),
      [ "canopy closed overhead; sound muffled, scent short"
      , "deadfall underfoot — hard to move quiet"
      , "sightlines in yards; he could bed ten feet off"
      , "every step makes noise"
      , "thick enough to hide you if you stop moving"
      , "slow going; good spot to sit and let him come to you"
      ])
  , ((CBush, Edge),
      [ "light gaps ahead — where deer browse at first light"
      , "canopy breaking; you'll be visible past this"
      , "trees standing apart; sightlines opening up"
      , "bush thinning here"
      ])
  , ((CBush, Bridge),
      [ "last of the poplars before the open"
      , "cover ends in a line; watch your exposure"
      , "treeline, then something else — check wind before stepping out"
      , "edge of the thick; deer travel these seams"
      ])
  , ((CRidge, Interior),
      [ "higher ground, honest wind"
      , "oak mast — deer come to these rocks to feed"
      , "long views, wind you can trust"
      , "the low country below; watch for movement in the field"
      , "lichen on limestone; deer bed on south-facing pockets"
      ])
  , ((CRidge, Edge),
      [ "slope, not summit"
      , "ground pitching down"
      , "shoulder of the ridge"
      , "rocks loose underfoot"
      ])
  , ((CRidge, Bridge),
      [ "ridge running out"
      , "down into lower country"
      , "oaks giving way"
      , "end of the climb"
      ])
  , ((CCreek, Interior),
      [ "water sound masks your footfalls"
      , "alders close overhead; good cover, short sightlines"
      , "colder air down here — deer water in the morning"
      , "mud holding prints — check before you step"
      , "deer cross the creek here; watch the far bank"
      ])
  , ((CCreek, Edge),
      [ "top of the bank — look for tracks in the wet ground"
      , "wet ground, dry ground; prints show up clean"
      , "water a few feet below; deer come down for it"
      , "smell of the creek; scent sits low"
      ])
  , ((CCreek, Bridge),
      [ "crossing opens out to new ground"
      , "bank opening; sightlines coming back"
      , "end of the cut; easier walking ahead"
      , "cover thinning as the bank widens"
      ])
  ]
