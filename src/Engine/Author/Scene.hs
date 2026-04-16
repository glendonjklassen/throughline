{-# LANGUAGE DataKinds #-}
-- | Scene graph construction: locations, edges, and action gating by scene.
module Engine.Author.Scene
  ( Scene(..)
  , SceneEdge(..)
  , SceneGraph(..)
  , buildActions
  , edge
  , biEdge
  , edgeActionId
  , narrationEffects
  ) where

import Engine.Author.DSL (atScene, repeatableAction, immediate, immediateWhen, anyAction)
import GameTypes

-- ---------------------------------------------------------------------------
-- Scene graph types
-- ---------------------------------------------------------------------------

-- | A scene at a specific location with its own actions.
-- Actions provided here should NOT be pre-gated with atScene — buildActions
-- handles location-gating from the sceneLocation.
data Scene = Scene
  { sceneLocation :: Location
  , sceneActions  :: CharId -> [AnyAction]
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

-- ---------------------------------------------------------------------------
-- Assembly
-- ---------------------------------------------------------------------------

-- | Assemble a scene graph into the flat action list a Scenario needs.
-- Location-gates each scene's actions and generates movement actions
-- from edges.
buildActions :: CharId -> SceneGraph -> [AnyAction]
buildActions cid sg =
  concatMap sceneToActions (sgScenes sg)
  ++ map edgeToAction (sgEdges sg)
  where
    sceneToActions s = atScene cid (sceneLocation s) (sceneActions s cid)

    edgeToAction e = anyAction $ repeatableAction (edgeId e) (edgeLabel e) cond
      (narrationEffects (edgeNarration e) ++ [immediate (SetLocation cid (edgeTo e))])
      where
        cond = All [AtLocation cid (edgeFrom e), edgeCondition e]
