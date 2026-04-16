module Scenarios.Customer.WalkScene (customerGraph) where

import           Engine.Author.Scene
import           Scenarios.TopBuy.Locations

customerGraph :: SceneGraph
customerGraph = SceneGraph
  { sgScenes =
      [ Scene parkingLot  (const [])
      , Scene entrance    (const [])
      , Scene salesFloor  (const [])
      , Scene electronics (const [])
      ]
  , sgEdges = concat
      [ biEdge parkingLot entrance
               "Head to the entrance."           "You walk to the entrance."
               "Go back to the parking lot."     "You head back out to the parking lot."

      , biEdge entrance salesFloor
               "Walk into the store."            "You push through the doors into the store."
               "Walk back to the entrance."      "You make your way back to the entrance."

      , biEdge salesFloor electronics
               "Browse the electronics section." "You wander over to the electronics section."
               "Head back to the main floor."    "You drift back toward the main floor."
      ]
  }
