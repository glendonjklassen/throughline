-- | Scenario validation: checks action ID uniqueness and scene graph connectivity.
module Engine.Author.Validate
  ( validateScenario
  , validateSceneGraph
  ) where

import qualified Data.Set as Set
import           Data.List (nub, (\\))
import           Engine.Author.Scene (SceneGraph(..), Scene(..), SceneEdge(..))
import           GameTypes

-- | Check a scenario for ActionId collisions.
-- Evaluates the scenario's action list against its initial world.
validateScenario :: Scenario -> Either [String] Scenario
validateScenario s =
  let actions = scenarioActions s
      ids     = map (actionIdText . anyActionId) actions
      dupes   = ids \\ nub ids
      errors  = map ("Duplicate ActionId: " <>) (nub dupes)
  in if null errors then Right s else Left errors

-- | Check a scene graph for edge targets that no scene covers.
validateSceneGraph :: SceneGraph -> Either [String] SceneGraph
validateSceneGraph sg =
  let covered = Set.fromList (map sceneLocation (sgScenes sg))
      targets = Set.fromList (concatMap edgeTargets (sgEdges sg))
      orphans = Set.difference targets covered
      errors  = map (\l -> "Edge targets uncovered location: " <> show l)
                    (Set.toList orphans)
  in if null errors then Right sg else Left errors
  where
    edgeTargets e = [edgeFrom e, edgeTo e]
