module Scenarios.TopBuySpec (spec) where

import           Data.Either        (isRight)
import           Test.Hspec

import           Engine.Author.Validate    (validateScenario, validateSceneGraph)
import           Engine.Core.Conditions    (checkCondition)
import           GameTypes

import           TestFixtures              (mkScenarioEnv, step, tickUntil)

import           Scenarios.TopBuy           (topBuy)
import           Scenarios.TopBuy.Actions   (topBuyGraph)
import           Scenarios.TopBuy.Constants

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  let you = Named "test-player"

  describe "TopBuy scenario" $ do

    it "PATH A: reporting the discrepancy leads to playerCleared" $ do
      let scenario = topBuy 0 you
      env <- mkScenarioEnv you scenario
      let w0 = scenarioInitial scenario

      -- Raise Understanding to 5 before Bradley evaluates you, so that
      -- perceptionAxiom will set playerSuspecting when smallTalk fires.
      w1 <- step env (ActionId "helpCustomer")      w0

      -- smallTalk bumps Bradley's Perceived Understanding of you from 0 → 2,
      -- triggering perceptionAxiom: Understanding (5) > 4 → playerSuspecting.
      w2 <- step env (ActionId "smallTalk")         w1

      -- Discover the inventory gap.
      w3 <- step env (ActionId "checkStockroom")    w2

      -- Report it to Kyle before getting implicated.
      -- earlyReportAxiom starts a 3-tick timer → kyleInvestigating.
      w4 <- step env (ActionId "reportDiscrepancy") w3

      w5 <- tickUntil (HasWorldTag kyleInvestigating) env w4

      -- Kyle confrontation: isReported = true → PATH A → playerCleared.
      w6 <- step env (ActionId "talkToKyle")        w5

      wFinal <- tickUntil (HasWorldTag playerCleared) env w6

      checkCondition wFinal (HasWorldTag playerCleared)   `shouldBe` True
      checkCondition wFinal (HasWorldTag playerSuspended) `shouldBe` False

    it "PATH C: covering for Bradley and staying silent leads to playerSuspended" $ do
      let scenario = topBuy 0 you
      env <- mkScenarioEnv you scenario
      let w0 = scenarioInitial scenario

      -- smallTalk without raising Understanding first: stays at 4, so
      -- perceptionAxiom does NOT set playerSuspecting.
      w1 <- step env (ActionId "smallTalk")            w0

      -- Discover the discrepancy (do not report it).
      w2 <- step env (ActionId "checkStockroom")       w1

      -- waitAction adds bradleyAsking; smallAskAxiom fires → bradleySmallAsk.
      w3 <- step env (ActionId "wait")       w2

      -- Log the return under your ID — implicates you, starts dialogue
      -- chain that eventually adds bradleyBigAsk.
      w4 <- step env (ActionId "logReturnForBradley")  w3

      w5 <- tickUntil (HasWorldTag bradleyBigAsk) env w4

      -- Cover the floor. accompliceAxiom starts a 2-tick timer → kyleInvestigating.
      w6 <- step env (ActionId "coverForBradley")      w5

      w7 <- tickUntil (HasWorldTag kyleInvestigating) env w6

      -- Kyle confrontation: isReported = false, isImplicated = true → PATH C → playerSuspended.
      w8 <- step env (ActionId "talkToKyle")           w7

      wFinal <- tickUntil (HasWorldTag playerSuspended) env w8

      checkCondition wFinal (HasWorldTag playerSuspended) `shouldBe` True
      checkCondition wFinal (HasWorldTag playerCleared)   `shouldBe` False

    it "PATH B: Kyle audits independently, player neither reported nor implicated, leads to playerCleared" $ do
      let scenario = topBuy 0 you
      env <- mkScenarioEnv you scenario
      let w0 = scenarioInitial scenario

      w1 <- step env (ActionId "smallTalk")      w0
      -- checkStockroom sets inventoryDiscrepancy; kyleAuditAxiom starts a
      -- 6-tick timer → kyleInvestigating, without requiring the player to
      -- report or cover.
      w2 <- step env (ActionId "checkStockroom") w1

      w3 <- tickUntil (HasWorldTag kyleInvestigating) env w2

      w4 <- step env (ActionId "talkToKyle")     w3

      wFinal <- tickUntil (HasWorldTag playerCleared) env w4

      checkCondition wFinal (HasWorldTag playerCleared)          `shouldBe` True
      checkCondition wFinal (HasWorldTag playerSuspended)        `shouldBe` False
      checkCondition wFinal (HasWorldTag reportedToKyle)         `shouldBe` False
      checkCondition wFinal (HasWorldTag loggedReturnForBradley) `shouldBe` False
      checkCondition wFinal (HasWorldTag coveredForBradley)      `shouldBe` False

  describe "validation" $ do
    it "has no duplicate ActionIds" $
      isRight (validateScenario (topBuy 0 you)) `shouldBe` True
    it "has a connected scene graph" $
      isRight (validateSceneGraph topBuyGraph) `shouldBe` True
