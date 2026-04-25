module Scenarios.TopBuy.Actions (allActions, topBuyGraph) where

import           Engine.Author.DSL               (continueAction, anyAction)
import           Engine.Author.Scene
import           GameTypes
import           Scenarios.TopBuy.Constants       (home)
import           Scenarios.TopBuy.HomeScene       (homeActions)
import           Scenarios.TopBuy.Locations       (salesFloor)
import           Scenarios.TopBuy.SalesFloorScene (salesFloorActions)

topBuyGraph :: SceneGraph
topBuyGraph = SceneGraph
  { sgScenes =
      [ Scene salesFloor salesFloorActions
      , Scene home       homeActions
      ]
  , sgEdges = []   -- transitions are axiom-driven (shift system)
  }

allActions :: CharacterId -> [AnyAction]
allActions you = anyAction continueAction : compileSceneGraph you topBuyGraph
