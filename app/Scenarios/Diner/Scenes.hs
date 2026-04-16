module Scenarios.Diner.Scenes (dinerGraph, dinerActions) where

import           Engine.Author.DSL               (continueAction, anyAction)
import           Engine.Author.Scene
import           GameTypes
import           Scenarios.Diner.Constants        (booth, counter, outside)
import           Scenarios.Diner.Scenes.Booth     (boothActions)
import           Scenarios.Diner.Scenes.Counter   (counterActions)
import           Scenarios.Diner.Scenes.Outside   (outsideActions)

dinerGraph :: SceneGraph
dinerGraph = SceneGraph
  { sgScenes =
      [ Scene booth    boothActions
      , Scene counter  counterActions
      , Scene outside  outsideActions
      ]
  , sgEdges = concat
      [ biEdge booth counter
               "Slide out and head to the counter."  "You cross the floor to the counter."
               "Go back to your booth."              "You settle back into the booth."

      , biEdge booth outside
               "Step outside for some air."           "You push through the door into the night."
               "Head back inside."                    "The warmth hits you as you step back in."

      , biEdge counter outside
               "Step outside."                        "You duck out through the side door."
               "Go back to the counter."              "You come back in, wiping rain from your sleeves."
      ]
  }

dinerActions :: CharId -> [AnyAction]
dinerActions you = anyAction continueAction : buildActions you dinerGraph
