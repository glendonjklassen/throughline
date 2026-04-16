{-# LANGUAGE DataKinds #-}
module Engine.Sync.EventLogSpec (spec) where

import           Test.Hspec
import qualified Data.ByteString   as BS

import           Engine.Sync.EventLog
import           Engine.CRDT.ORSet
import           GameTypes
import           GameTypes.Types (Action(..))
import           MonadStack
import qualified Data.Map.Strict   as Map
import           TestFixtures

-- ---------------------------------------------------------------------------
-- Test scenarios
-- ---------------------------------------------------------------------------

tagOne :: Tag
tagOne = ScenarioTag (MkScenarioTag "did-one")

tagTwo :: Tag
tagTwo = ScenarioTag (MkScenarioTag "did-two")

act1 :: Action 'Repeatable
act1 = Action
  { actionId        = ActionId "act1"
  , actionLabel     = "Action One"
  , actionTarget    = Nothing
  , actionCondition = unconditional
  , actionEffects   = [Effect (AddWorldTag tagOne) (Just 1) unconditional Nothing]
  }

act2 :: Action 'Repeatable
act2 = Action
  { actionId        = ActionId "act2"
  , actionLabel     = "Action Two"
  , actionTarget    = Nothing
  , actionCondition = HasWorldTag tagOne
  , actionEffects   = [Effect (AddWorldTag tagTwo) (Just 1) unconditional Nothing]
  }

testScenario :: Scenario
testScenario = Scenario
  { scenarioName         = "test-scenario"
  , scenarioInitial      = emptyWorld
  , scenarioActions      = [AnyAction act1, AnyAction act2]
  , scenarioAxioms       = []
  , scenarioMergeAxioms  = []
  , scenarioRules        = []
  , scenarioMergeRules   = []
  , scenarioTerminal     = Any []
  , scenarioDebugDefault = Off
  , scenarioPlayerCharId = player
  }

actTagA :: Action 'Repeatable
actTagA = Action (ActionId "actA") "A" Nothing unconditional
  [Effect (AddWorldTag (ScenarioTag (MkScenarioTag "tag-a"))) (Just 1) unconditional Nothing]

actTagB :: Action 'Repeatable
actTagB = Action (ActionId "actB") "B" Nothing unconditional
  [Effect (AddWorldTag (ScenarioTag (MkScenarioTag "tag-b"))) (Just 1) unconditional Nothing]

mergeScenario :: Scenario
mergeScenario = Scenario
  { scenarioName         = "merge-scenario"
  , scenarioInitial      = emptyWorld
  , scenarioActions      = [AnyAction actTagA, AnyAction actTagB]
  , scenarioAxioms       = []
  , scenarioMergeAxioms  = []
  , scenarioRules        = []
  , scenarioMergeRules   = []
  , scenarioTerminal     = Any []
  , scenarioDebugDefault = Off
  , scenarioPlayerCharId = player
  }

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "Engine.Sync.EventLog" $ do

  describe "replayFrom initial world" $ do

    it "applies stored diff regardless of action availability" $ do
      -- act2 normally requires tagOne, but the stored diff is applied as-is
      let pid         = PlayerId "test"
          diffWithTwo = emptyDiff { diffWorldTagsAdded = [tagTwo] }
          entry2 = mkLogEntry pid (LamportClock 1 pid) (ActionId "act2") diffWithTwo Map.empty
      result <- replayFrom testScenario emptyWorld [entry2]
      case result of
        Left err -> expectationFailure (show err)
        Right w  -> orMember tagTwo (worldTags w) `shouldBe` True

    it "replays the correct tags in order" $ do
      let pid     = PlayerId "test"
          diffOne = emptyDiff { diffWorldTagsAdded = [tagOne] }
          entry1 = mkLogEntry pid (LamportClock 1 pid) (ActionId "act1") diffOne Map.empty

      result <- replayFrom testScenario emptyWorld [entry1]
      case result of
        Left err          -> expectationFailure (show err)
        Right replayWorld -> do
          orMember tagOne (worldTags replayWorld) `shouldBe` True
          orMember tagTwo (worldTags replayWorld) `shouldBe` False

    it "worldClock reflects the last replayed entry's Lamport clock" $ do
      let pid     = PlayerId "test"
          diffOne = emptyDiff { diffWorldTagsAdded = [tagOne] }
          diffTwo = emptyDiff { diffWorldTagsAdded = [tagTwo] }
          entry1 = mkLogEntry pid (LamportClock 1 pid) (ActionId "act1") diffOne Map.empty
          entry2 = mkLogEntry pid (LamportClock 2 pid) (ActionId "act2") diffTwo Map.empty
      result <- replayFrom testScenario emptyWorld [entry1, entry2]
      case result of
        Left err -> expectationFailure (show err)
        Right w  -> worldClock w `shouldBe` entryClock entry2

    it "rejects an entry with an invalid signature" $ do
      let foreignId = PlayerId (replicate 64 'a')
          diff      = emptyDiff { diffWorldTagsAdded = [tagOne] }
          entry = mkLogEntry foreignId (LamportClock 1 foreignId) (ActionId "act1") diff Map.empty
      let tampered = entry { entrySignature = Just (BS.replicate 64 0) }
      result <- replayFrom testScenario emptyWorld [tampered]
      case result of
        Left (InvalidAction _) -> pure ()
        Left err               -> expectationFailure ("Wrong error: " <> show err)
        Right _                -> expectationFailure "Expected rejection of tampered entry"

  describe "replayFrom" $ do

    it "entries apply their stored diff (no action lookup)" $ do
      let pid = PlayerId "player-a"
          ownDiff = emptyDiff { diffWorldTagsAdded = [ScenarioTag (MkScenarioTag "tag-a")] }
          eA = mkLogEntry pid (LamportClock 1 pid) (ActionId "nonexistent-action") ownDiff Map.empty
      result <- replayFrom mergeScenario emptyWorld [eA]
      case result of
        Left err -> expectationFailure (show err)
        Right w  -> orMember (ScenarioTag (MkScenarioTag "tag-a")) (worldTags w) `shouldBe` True

    it "foreign entries apply their stored diff without action lookup" $ do
      let foreignId = PlayerId "player-b"
          fakeDiff  = emptyDiff { diffWorldTagsAdded = [ScenarioTag (MkScenarioTag "foreign-tag")] }
          eB = mkLogEntry foreignId (LamportClock 1 foreignId) (ActionId "nonexistent-action") fakeDiff Map.empty
      result <- replayFrom mergeScenario emptyWorld [eB]
      case result of
        Left err -> expectationFailure (show err)
        Right w  -> orMember (ScenarioTag (MkScenarioTag "foreign-tag")) (worldTags w) `shouldBe` True

    it "an entry with invalid signature is rejected" $ do
      let -- 64-char hex = a valid-length PlayerId (key-derived format)
          foreignId = PlayerId (replicate 64 'a')
          fakeDiff  = emptyDiff { diffWorldTagsAdded = [ScenarioTag (MkScenarioTag "injected")] }
          eB = mkLogEntry foreignId (LamportClock 1 foreignId) (ActionId "act") fakeDiff Map.empty
      -- Attach a garbage signature — 64 zero bytes
      let tampered = eB { entrySignature = Just (BS.replicate 64 0) }
      result <- replayFrom mergeScenario emptyWorld [tampered]
      case result of
        Left (InvalidAction _) -> pure ()
        Left err               -> expectationFailure ("Wrong error: " <> show err)
        Right _                -> expectationFailure "Expected rejection of tampered entry"

  describe "mkLogEntry" $ do

    it "uses the provided clock tick in the entry" $ do
      let pid = PlayerId "test"
          e1 = mkLogEntry pid (LamportClock 1 pid) (ActionId "act1") emptyDiff Map.empty
          e2 = mkLogEntry pid (LamportClock 2 pid) (ActionId "act2") emptyDiff Map.empty
          e3 = mkLogEntry pid (LamportClock 3 pid) (ActionId "act3") emptyDiff Map.empty
      map (lcTick . entryClock) [e1, e2, e3] `shouldBe` [1, 2, 3]

    it "entryId includes tick and player" $ do
      let pid = PlayerId "player-a"
          e = mkLogEntry pid (LamportClock 5 pid) (ActionId "act") emptyDiff Map.empty
      entryId e `shouldBe` "5-player-a"

  describe "logAction" $ do

    it "is a no-op for an empty diff (isNoOp branch)" $ do
      (_, w) <- runApp' emptyWorld (logAction (ActionId "act1") emptyDiff)
      worldClock w `shouldBe` worldClock emptyWorld

    it "is a no-op when no event log path is configured" $ do
      let diff = emptyDiff { diffWorldTagsAdded = [ScenarioTag (MkScenarioTag "tag-a")] }
      (_, w) <- runApp' emptyWorld (logAction (ActionId "act1") diff)
      worldClock w `shouldBe` worldClock emptyWorld
