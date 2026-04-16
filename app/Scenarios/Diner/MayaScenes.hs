module Scenarios.Diner.MayaScenes (mayaGraph, mayaActions) where

import           Engine.Author.DSL               (continueAction, anyAction)
import           Engine.Author.Scene
import           GameTypes
import           Scenarios.Diner.Constants        (counter, outside)
import           Scenarios.Diner.Scenes.MayaCounter  (mayaCounterActions)
import           Scenarios.Diner.Scenes.MayaOutside  (mayaOutsideActions)

mayaGraph :: SceneGraph
mayaGraph = SceneGraph
  { sgScenes =
      [ Scene counter  mayaCounterActions
      , Scene outside  mayaOutsideActions
      ]
  , sgEdges =
      biEdge counter outside
             "Step out the back for some air."    "You push through the service door into the rain."
             "Head back to the counter."          "The warm diner air settles around you."
  }

mayaActions :: CharId -> [AnyAction]
mayaActions mayaId = anyAction continueAction : buildActions mayaId mayaGraph
