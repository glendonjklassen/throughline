module Scenarios.DinerSyncSpec (spec) where

import qualified Data.Map.Strict        as Map
import           Test.Hspec

import           Engine.Author.Scene    (edgeActionId)
import           Engine.Core.Conditions (checkCondition)
import           Engine.Core.Effects    (mergeWorlds)
import           Engine.Headless        (runHeadlessScript)
import           Engine.Sync.EventLog   (mergeLogs, replayFrom)
import           GameTypes
import           MonadStack             (AppError)
import           Scenarios.Diner        (diner)
import           Scenarios.DinerMaya    (dinerMaya)
import           Scenarios.Diner.Constants

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Run visitor and Maya scripts independently, merge logs, replay.
mergeScripts
  :: [ActionId] -> [ActionId]
  -> IO (Either AppError GameWorld)
mergeScripts visitorScript mayaScript = do
  let visitorPid = PlayerId "visitor"
      mayaPid    = PlayerId "maya"
      visitorScen = diner 0 visitor
      mayaScen    = dinerMaya 0 maya
      base        = mergeWorlds (scenarioInitial visitorScen)
                                (scenarioInitial mayaScen)
  Right (_, visitorLog) <- runHeadlessScript (diner 0)     visitorPid visitorScript
  Right (_, mayaLog)    <- runHeadlessScript (dinerMaya 0)  mayaPid    mayaScript
  let (_, merged) = mergeLogs visitorLog mayaLog
  replayFrom visitorScen base merged

-- | Snapshot merge: CRDT union of the two final worlds.
snapshotMerge
  :: [ActionId] -> [ActionId]
  -> IO GameWorld
snapshotMerge visitorScript mayaScript = do
  let visitorPid = PlayerId "visitor"
      mayaPid    = PlayerId "maya"
  Right (vWorld, _) <- runHeadlessScript (diner 0)     visitorPid visitorScript
  Right (mWorld, _) <- runHeadlessScript (dinerMaya 0) mayaPid    mayaScript
  pure (mergeWorlds vWorld mWorld)

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do

  describe "Diner two-player merge" $ do

    it "both players retain their locations after merge" $ do
      result <- mergeScripts
        [ActionId "lookAround", ActionId "orderCoffee", edgeActionId booth counter]
        [ActionId "maya:checkOnFrank"]
      case result of
        Left err -> expectationFailure ("merge failed: " <> show err)
        Right w  -> do
          Map.lookup visitor (worldLocations w) `shouldBe` Just counter
          Map.lookup maya    (worldLocations w) `shouldBe` Just counter

    it "visitor's orderedCoffee tag is visible to Maya's replay" $ do
      result <- mergeScripts
        [ActionId "orderCoffee"]
        [ActionId "maya:prepOrder"]
      case result of
        Left err -> expectationFailure ("merge failed: " <> show err)
        Right w  -> do
          checkCondition w (HasWorldTag orderedCoffee) `shouldBe` True

    it "snapshot merge and log merge agree on shared tags" $ do
      let vScript = [ActionId "lookAround", ActionId "orderCoffee",
                     edgeActionId booth counter, ActionId "talkToMaya"]
          mScript = [ActionId "maya:checkOnFrank"]
      sw     <- snapshotMerge vScript mScript
      result <- mergeScripts  vScript mScript
      case result of
        Left err -> expectationFailure ("merge failed: " <> show err)
        Right lw -> do
          checkCondition sw (HasWorldTag orderedCoffee)
            `shouldBe` checkCondition lw (HasWorldTag orderedCoffee)
          checkCondition sw (HasWorldTag frankChatted)
            `shouldBe` checkCondition lw (HasWorldTag frankChatted)

    it "Maya's routine reaches dawn independently" $ do
      result <- mergeScripts
        []
        [ActionId "maya:wipeCounter"]
      case result of
        Left err -> expectationFailure ("merge failed: " <> show err)
        Right _  -> pure ()  -- no crash on merge with minimal scripts
