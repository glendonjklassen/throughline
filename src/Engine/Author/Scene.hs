{-# LANGUAGE DataKinds #-}
-- | The spatial layer.  A 'SceneGraph' is a list of 'Scene's (a
-- location plus the actions available there) plus a list of
-- 'SceneEdge's (traversable connections with their movement
-- narration).  'compileSceneGraph' compiles the graph into the flat
-- @[AnyAction]@ list a scenario hands to the engine.
--
-- Build edges with 'edge' \/ 'biEdge' for ad-hoc connections, or
-- 'biEdgeWith' when narration is derived per-direction (e.g. from a
-- terrain classifier).  Lift a 'LocationGraph' into a full scene
-- graph in one call with 'sceneGraphFromLocations'.
module Engine.Author.Scene
  ( Scene(..)
  , SceneEdge(..)
  , SceneGraph(..)
  , compileSceneGraph
  , edge
  , biEdge
  , biEdgeWith
  , edgeActionId
  , edgeSalt
  , narrationEffects
  , poolNarration
  , sceneGraphFromLocations
  ) where

import qualified Data.Set as Set
import Engine.Author.DSL (atScene, repeatableAction, immediate, immediateWhen, anyAction)
import GameTypes

-- ---------------------------------------------------------------------------
-- Scene graph types
-- ---------------------------------------------------------------------------

-- | A scene at a specific location with its own actions.
-- Actions provided here should NOT be pre-gated with atScene — compileSceneGraph
-- handles location-gating from the sceneLocation.
data Scene = Scene
  { sceneLocation :: Location
  , sceneActions  :: CharacterId -> [AnyAction]
  }

-- | A traversable path between two locations.
data SceneEdge = SceneEdge
  { edgeId        :: ActionId           -- ^ unique action identifier
  , edgeFrom      :: Location           -- ^ source location
  , edgeTo        :: Location           -- ^ destination location
  , edgeLabel     :: String             -- ^ action label shown to the player
  , edgeNarration :: Narration          -- ^ narration when traversing
  , edgeCondition :: Condition          -- ^ extra gate beyond AtLocation
  }

-- | A graph of scenes connected by traversable edges.
data SceneGraph = SceneGraph
  { sgScenes :: [Scene]
  , sgEdges  :: [SceneEdge]
  }

-- ---------------------------------------------------------------------------
-- Narration resolution
-- ---------------------------------------------------------------------------

-- | Convert a Narration to condition-gated effects.
-- Static narration becomes a single immediate Narrate effect.
-- Conditional narration becomes multiple condition-gated Narrate effects,
-- with the fallback firing when none of the branch conditions hold.
narrationEffects :: Narration -> [Effect]
narrationEffects (Static s) = [immediate (Narrate s)]
narrationEffects (Conditional branches fallback) =
  [ immediateWhen cond (Narrate text) | (cond, text) <- branches ]
  ++ [immediateWhen (All [Not c | (c, _) <- branches]) (Narrate fallback)]
narrationEffects (NarrationPool salt variants) = [immediate (NarratePool salt variants)]

-- ---------------------------------------------------------------------------
-- Edge constructors
-- ---------------------------------------------------------------------------

-- | Derive a deterministic ActionId from a location pair.
edgeActionId :: Location -> Location -> ActionId
edgeActionId from to = ActionId ("walk:" <> locationName from <> ":" <> locationName to)

-- | Unidirectional edge with no extra conditions.
edge :: Location -> Location -> String -> String -> SceneEdge
edge from to label narration = SceneEdge (edgeActionId from to) from to label (Static narration) unconditional

-- | Bidirectional pair of edges between two locations.
biEdge :: Location -> Location
       -> String -> String    -- ^ forward label and narration
       -> String -> String    -- ^ reverse label and narration
       -> [SceneEdge]
biEdge a b labelAB narrAB labelBA narrBA =
  [ edge a b labelAB narrAB
  , edge b a labelBA narrBA
  ]

-- | Bidirectional pair of edges with a derived 'Narration' per direction.
-- Labels are the destination's location name; for richer labels build
-- 'SceneEdge' values directly.
biEdgeWith :: (Location -> Location -> Narration)
           -> Location -> Location
           -> [SceneEdge]
biEdgeWith mkNarr a b =
  [ SceneEdge (edgeActionId a b) a b (locationName b) (mkNarr a b) unconditional
  , SceneEdge (edgeActionId b a) b a (locationName a) (mkNarr b a) unconditional
  ]

-- | Deterministic salt derived from a location pair.  Useful for
-- per-edge 'NarrationPool' seeds so adjacent edges produce independent
-- PRNG sequences.
edgeSalt :: Location -> Location -> Int
edgeSalt (Location a) (Location b) = sum (map fromEnum a) + sum (map fromEnum b) * 31

-- | Build a 'NarrationPool' for an edge from a per-edge variant
-- function, salted by the location pair.
poolNarration :: (Location -> Location -> [String])
              -> Location -> Location
              -> Narration
poolNarration variants from to = NarrationPool (edgeSalt from to) (variants from to)

-- | Lift a 'LocationGraph' into a 'SceneGraph' by attaching per-scene
-- actions and an edge-builder for each location pair.  Pass
-- @\\_ _ -> []@ for @mkScene@ when actions are universal rather than
-- per-scene, and a builder like 'biEdgeWith' for @mkEdges@.
sceneGraphFromLocations
  :: [Location]
  -> LocationGraph
  -> (Location -> CharacterId -> [AnyAction])
  -> (Location -> Location -> [SceneEdge])
  -> SceneGraph
sceneGraphFromLocations locs lg mkScene mkEdges = SceneGraph
  { sgScenes = [ Scene loc (mkScene loc) | loc <- locs ]
  , sgEdges  = concatMap (uncurry mkEdges) (Set.toList (lgEdges lg))
  }

-- ---------------------------------------------------------------------------
-- Assembly
-- ---------------------------------------------------------------------------

-- | Assemble a scene graph into the flat action list a Scenario needs.
-- Location-gates each scene's actions and generates movement actions
-- from edges.
compileSceneGraph :: CharacterId -> SceneGraph -> [AnyAction]
compileSceneGraph cid sg =
  concatMap sceneToActions (sgScenes sg)
  ++ map edgeToAction (sgEdges sg)
  where
    sceneToActions s = atScene cid (sceneLocation s) (sceneActions s cid)

    edgeToAction e = anyAction $ repeatableAction (edgeId e) (edgeLabel e) cond
      (narrationEffects (edgeNarration e) ++ [immediate (SetLocation cid (edgeTo e))])
      where
        cond = All [AtLocation cid (edgeFrom e), edgeCondition e]
