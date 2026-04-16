module Scenarios.DinerSpec (spec) where

import           Data.Either        (isRight)
import           Data.List          (find)
import           Test.Hspec

import           Engine.Author.Scene        (edgeActionId)
import           Engine.Author.Validate    (validateScenario, validateSceneGraph)
import           Engine.Core.Conditions    (checkCondition)
import           Engine.Core.Effects       (executeStep)
import           GameTypes
import           MonadStack

import           TestFixtures              (mkScenarioEnv, step)

import           Scenarios.Diner           (diner)
import           Scenarios.Diner.Constants
import           Scenarios.Diner.MayaScenes (mayaGraph)
import           Scenarios.Diner.Scenes     (dinerGraph)
import           Scenarios.DinerMaya        (dinerMaya)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Tick by picking any available diner wait action until the condition holds.
tickUntil :: Condition -> Env -> GameWorld -> IO GameWorld
tickUntil cond env = go (100 :: Int)
  where
    waitIds = [ActionId "waitBooth", ActionId "waitCounter", ActionId "continue-dialogue"]
    go 0 _ = error "tickUntil: condition never fired within 100 steps"
    go n w
      | checkCondition w cond = pure w
      | otherwise = do
          let available = filter (checkCondition w . anyActionCondition) (envActions env)
              mWait = find (\a -> anyActionId a `elem` waitIds) available
          case mWait of
            Nothing -> error $ "No wait action available: " <> show (map anyActionId available)
            Just (AnyAction action) -> do
              result <- runApp env w (executeStep action)
              case result of
                Left err     -> error $ "AppError: " <> show err
                Right (_, w') -> go (n - 1) w'

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  let scenario = diner 0 visitor

  describe "Diner scenario" $ do

    it "FRANK PATH: observation unlocks deep dialogue, leads to settled" $ do
      env <- mkScenarioEnv visitor scenario
      let w0 = scenarioInitial scenario

      w1 <- step env (ActionId "lookAround")          w0
      w2 <- step env (ActionId "orderCoffee")         w1
      w3 <- step env (edgeActionId booth counter)  w2

      w4 <- step env (ActionId "sitNearFrank")        w3
      w5 <- step env (ActionId "askFrankName")        w4
      w6 <- step env (ActionId "askWhyHeComes")       w5
      checkCondition w6 (HasWorldTag frankOpened) `shouldBe` True
      w7 <- step env (ActionId "listenToFrank")       w6

      checkCondition w7 (HasWorldTag settled)   `shouldBe` True
      checkCondition w7 (HasWorldTag restless)  `shouldBe` False

    it "MAYA PATH: observation and connection leads to settled" $ do
      env <- mkScenarioEnv visitor scenario
      let w0 = scenarioInitial scenario

      w1 <- step env (ActionId "lookAround")            w0
      w2 <- step env (ActionId "orderCoffee")           w1
      w3 <- step env (edgeActionId booth counter)    w2

      w4 <- step env (ActionId "talkToMaya")            w3
      w5 <- step env (ActionId "askMayaAboutHerNight")  w4
      checkCondition w5 (HasWorldTag mayaOpened) `shouldBe` True
      w6 <- step env (ActionId "stayWithMaya")          w5

      checkCondition w6 (HasWorldTag settled)   `shouldBe` True
      checkCondition w6 (HasWorldTag restless)  `shouldBe` False

    it "SOLITARY PATH: staying in the booth reaches dawn" $ do
      env <- mkScenarioEnv visitor scenario
      let w0 = scenarioInitial scenario

      wFinal <- tickUntil (HasWorldTag dawnArrived) env w0

      checkCondition wFinal (HasWorldTag dawnArrived)  `shouldBe` True
      checkCondition wFinal (HasWorldTag settled)      `shouldBe` False

    it "OUTSIDE PATH: stepping out and coming back works" $ do
      env <- mkScenarioEnv visitor scenario
      let w0 = scenarioInitial scenario

      w1 <- step env (edgeActionId booth outside)  w0
      w2 <- step env (ActionId "watchStreet")         w1
      w3 <- step env (edgeActionId outside booth)  w2

      checkCondition w3 (AtLocation visitor booth) `shouldBe` True

      wFinal <- tickUntil (HasWorldTag dawnArrived) env w3
      checkCondition wFinal (HasWorldTag dawnArrived) `shouldBe` True

  describe "validation" $ do
    it "visitor scenario has no duplicate ActionIds" $
      isRight (validateScenario (diner 0 visitor)) `shouldBe` True
    it "Maya scenario has no duplicate ActionIds" $
      isRight (validateScenario (dinerMaya 0 maya)) `shouldBe` True
    it "diner scene graph is connected" $
      isRight (validateSceneGraph dinerGraph) `shouldBe` True
    it "Maya scene graph is connected" $
      isRight (validateSceneGraph mayaGraph) `shouldBe` True
