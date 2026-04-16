module Engine.Author.SceneSpec (spec) where

import qualified Data.Map.Strict as Map
import           Test.Hspec

import           Engine.Author.DSL      (anyAction, repeatableAction)
import           Engine.Author.Scene
import           Engine.Core.Conditions (checkCondition)
import           GameTypes
import           TestFixtures           (emptyWorld, player)

-- ---------------------------------------------------------------------------
-- Test locations
-- ---------------------------------------------------------------------------

kitchen :: Location
kitchen = Location "kitchen"

hallway :: Location
hallway = Location "hallway"

garden :: Location
garden = Location "garden"

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "Engine.Author.Scene" $ do

  describe "buildActions" $ do

    it "generates movement actions from edges" $ do
      let sg = SceneGraph
            { sgScenes = [Scene kitchen (const []), Scene hallway (const [])]
            , sgEdges  = [edge kitchen hallway "Go to hallway" "You walk to the hallway."]
            }
          actions = buildActions player sg
          moveActions = filter (\a -> anyActionId a == edgeActionId kitchen hallway) actions
      length moveActions `shouldBe` 1

    it "movement action is available when player is at source location" $ do
      let sg = SceneGraph
            { sgScenes = [Scene kitchen (const []), Scene hallway (const [])]
            , sgEdges  = [edge kitchen hallway "Go to hallway" "You walk."]
            }
          world = emptyWorld { worldLocations = locAt player kitchen }
          actions = buildActions player sg
          moveActions = filter (\a -> anyActionId a == edgeActionId kitchen hallway) actions
      case moveActions of
        [moveAction] -> checkCondition world (anyActionCondition moveAction) `shouldBe` True
        _            -> expectationFailure "expected exactly one move action"

    it "movement action is unavailable when player is elsewhere" $ do
      let sg = SceneGraph
            { sgScenes = [Scene kitchen (const []), Scene hallway (const [])]
            , sgEdges  = [edge kitchen hallway "Go to hallway" "You walk."]
            }
          world = emptyWorld { worldLocations = locAt player hallway }
          actions = buildActions player sg
          moveActions = filter (\a -> anyActionId a == edgeActionId kitchen hallway) actions
      case moveActions of
        [moveAction] -> checkCondition world (anyActionCondition moveAction) `shouldBe` False
        _            -> expectationFailure "expected exactly one move action"

    it "scene actions are location-gated" $ do
      let myAction = anyAction (repeatableAction (ActionId "cook") "Cook dinner" unconditional [])
          sg = SceneGraph
            { sgScenes = [Scene kitchen (const [myAction])]
            , sgEdges  = []
            }
          worldAtKitchen = emptyWorld { worldLocations = locAt player kitchen }
          worldAtHallway = emptyWorld { worldLocations = locAt player hallway }
          actions = buildActions player sg
      case actions of
        [actionHere] -> do
          checkCondition worldAtKitchen (anyActionCondition actionHere)  `shouldBe` True
          checkCondition worldAtHallway (anyActionCondition actionHere) `shouldBe` False
        _ -> expectationFailure "expected exactly one action"

  describe "biEdge" $ do

    it "produces two edges in opposite directions" $ do
      let edges = biEdge kitchen hallway
                    "Go to hallway" "You walk to the hallway."
                    "Go to kitchen" "You walk to the kitchen."
      length edges `shouldBe` 2
      case edges of
        (e0:e1:_) -> do
          edgeFrom e0 `shouldBe` kitchen
          edgeTo   e0 `shouldBe` hallway
          edgeFrom e1 `shouldBe` hallway
          edgeTo   e1 `shouldBe` kitchen
        _ -> expectationFailure "expected at least two edges"

  describe "edge with condition" $ do

    it "extra condition gates the movement action" $ do
      let tag = ScenarioTag (MkScenarioTag "has-key")
          gatedEdge = (edge hallway garden "Enter garden" "You step outside.")
                        { edgeCondition = HasWorldTag tag }
          sg = SceneGraph
            { sgScenes = [Scene hallway (const []), Scene garden (const [])]
            , sgEdges  = [gatedEdge]
            }
          worldNoKey = emptyWorld { worldLocations = locAt player hallway }
          actions = buildActions player sg
          moveActions = filter (\a -> anyActionId a == edgeActionId hallway garden) actions
      case moveActions of
        [moveAction] -> checkCondition worldNoKey (anyActionCondition moveAction) `shouldBe` False
        _            -> expectationFailure "expected exactly one move action"

locAt :: CharId -> Location -> Map.Map CharId Location
locAt = Map.singleton
