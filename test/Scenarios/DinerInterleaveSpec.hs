module Scenarios.DinerInterleaveSpec (spec) where

import           Data.List              (find, isInfixOf)
import           Test.Hspec

import           Engine.Author.Scene    (edgeActionId)
import           Engine.Core.Conditions (checkCondition)
import           Engine.Core.Effects    (mergeWorlds)
import           Engine.CRDT.ORSet      (orInsert, initToken)
import           Engine.Headless        (runHeadlessScript)
import           GameTypes

import           TestFixtures              (mkScenarioEnv, step)

import           Scenarios.Diner        (diner)
import           Scenarios.DinerMaya    (dinerMaya)
import           Scenarios.Diner.Constants
import           Scenarios.Diner.Scenes.Counter    (counterActions)
import           Scenarios.Diner.Scenes.MayaCounter (mayaCounterActions)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Add a tag to the world (for test setup).
addTag :: Tag -> GameWorld -> GameWorld
addTag t w = w { worldTags = orInsert (initToken t) t (worldTags w) }

-- | Find a specific action by ID from a list.
findAction :: ActionId -> [AnyAction] -> Maybe AnyAction
findAction aid =
  find (\a -> anyActionId a == aid)

-- | Extract the first Narrate string from an action's effects.
-- For condition-gated effects (immediateWhen), we look for Narrate bodies
-- regardless of the condition — the test world will determine which fires.
firstNarrate :: GameWorld -> AnyAction -> Maybe String
firstNarrate world action =
  case [ s | Effect { effectBody = Narrate s, effectCondition = cond } <- anyActionEffects action
           , checkCondition world cond ] of
    (s:_) -> Just s
    []    -> Nothing

-- | Check that a narration contains a substring.
narrateShouldContain :: GameWorld -> AnyAction -> String -> Expectation
narrateShouldContain world action needle =
  case firstNarrate world action of
    Nothing -> expectationFailure "Expected Narrate effect but found none"
    Just s  -> s `shouldSatisfy` (needle `isInfixOf`)

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do

  let baseWorld = addTag orderedCoffee (initialWorld 0)

  describe "Visitor senseTheRoom — ambient interleave" $ do

    it "SOLO: no ambient tags → solitary narration" $ do
      let world = baseWorld
      case findAction (ActionId "senseTheRoom") (counterActions visitor) of
        Nothing -> expectationFailure "senseTheRoom not available"
        Just action -> narrateShouldContain world action "Nothing special"

    it "LIVE: one ambient tag → partial narration" $ do
      let world = addTag smallKindness baseWorld
      case findAction (ActionId "senseTheRoom") (counterActions visitor) of
        Nothing -> expectationFailure "senseTheRoom not available"
        Just action -> narrateShouldContain world action "texture to the quiet"

    it "COMPLETE: both ambient tags → full narration" $ do
      let world = addTag smallKindness (addTag worryInTheWalls baseWorld)
      case findAction (ActionId "senseTheRoom") (counterActions visitor) of
        Nothing -> expectationFailure "senseTheRoom not available"
        Just action -> narrateShouldContain world action "holds more than you expected"

  describe "Maya takeStock — ambient interleave" $ do

    it "SOLO: no ambient tags → routine narration" $ do
      let world = baseWorld
      case findAction (ActionId "maya:takeStock") (mayaCounterActions maya) of
        Nothing -> expectationFailure "maya:takeStock not available"
        Just action -> narrateShouldContain world action "Another night"

    it "LIVE: one ambient tag → partial narration" $ do
      let world = addTag lateNightConfession baseWorld
      case findAction (ActionId "maya:takeStock") (mayaCounterActions maya) of
        Nothing -> expectationFailure "maya:takeStock not available"
        Just action -> narrateShouldContain world action "Something shifted tonight"

    it "COMPLETE: both ambient tags → full narration" $ do
      let world = addTag lateNightConfession (addTag quietPresence baseWorld)
      case findAction (ActionId "maya:takeStock") (mayaCounterActions maya) of
        Nothing -> expectationFailure "maya:takeStock not available"
        Just action -> narrateShouldContain world action "night had weight"

  describe "Ambient tags through actual gameplay + merge" $ do

    it "visitor actions set ambient tags that Maya's action reads" $ do
      -- Run visitor through the Frank connection path
      let scenario = diner 0 visitor
      env <- mkScenarioEnv visitor scenario
      let w0 = scenarioInitial scenario
      w1 <- step env (ActionId "lookAround")          w0
      w2 <- step env (ActionId "orderCoffee")         w1
      w3 <- step env (edgeActionId booth counter)  w2
      w4 <- step env (ActionId "sitNearFrank")        w3
      w5 <- step env (ActionId "askFrankName")        w4
      w6 <- step env (ActionId "askWhyHeComes")       w5
      w7 <- step env (ActionId "listenToFrank")       w6

      -- Visitor's actions should have set both ambient tags
      checkCondition w7 (HasWorldTag lateNightConfession) `shouldBe` True
      checkCondition w7 (HasWorldTag quietPresence)       `shouldBe` True

      -- Maya's takeStock should now see the full narration
      case findAction (ActionId "maya:takeStock") (mayaCounterActions maya) of
        Nothing -> expectationFailure "maya:takeStock not available"
        Just action -> narrateShouldContain w7 action "night had weight"

    it "Maya actions set ambient tags that visitor's action reads" $ do
      -- Run Maya through her routine
      let scenario = dinerMaya 0 maya
      env <- mkScenarioEnv maya scenario
      let w0 = addTag orderedCoffee (scenarioInitial scenario)
      w1 <- step env (ActionId "maya:checkOnFrank")  w0
      -- Tick to 3 AM for worryAboutKid
      w2 <- step env (ActionId "maya:wipeCounter")   w1
      w3 <- step env (ActionId "maya:wipeCounter")   w2
      w4 <- step env (ActionId "maya:worryAboutKid") w3

      checkCondition w4 (HasWorldTag smallKindness)   `shouldBe` True
      checkCondition w4 (HasWorldTag worryInTheWalls) `shouldBe` True

      -- Visitor's senseTheRoom should see full narration
      case findAction (ActionId "senseTheRoom") (counterActions visitor) of
        Nothing -> expectationFailure "senseTheRoom not available"
        Just action -> narrateShouldContain w4 action "holds more than you expected"

    it "snapshot merge propagates ambient tags across scenarios" $ do
      let visitorPid = PlayerId "visitor"
          mayaPid    = PlayerId "maya"
      -- Visitor runs solo — no ambient tags from Maya
      Right (vWorld, _) <- runHeadlessScript (diner 0) visitorPid
        [ActionId "orderCoffee", ActionId "lookAround"]
      -- Maya runs her routine, setting smallKindness + worryInTheWalls
      Right (mWorld, _) <- runHeadlessScript (dinerMaya 0) mayaPid
        [ActionId "maya:checkOnFrank", ActionId "maya:wipeCounter",
         ActionId "maya:wipeCounter", ActionId "maya:worryAboutKid"]

      -- Before merge: visitor's world has no Maya ambient tags
      checkCondition vWorld (HasWorldTag smallKindness) `shouldBe` False

      -- After merge: Maya's ambient tags are visible
      let merged = mergeWorlds vWorld mWorld
      checkCondition merged (HasWorldTag smallKindness)   `shouldBe` True
      checkCondition merged (HasWorldTag worryInTheWalls) `shouldBe` True

      -- Visitor's action now picks up the merged state
      case findAction (ActionId "senseTheRoom") (counterActions visitor) of
        Nothing -> expectationFailure "senseTheRoom not available after merge"
        Just action -> narrateShouldContain merged action "holds more than you expected"

  describe "Per-player dawn tags" $ do

    it "visitor's dawn does not terminate Maya's scenario" $ do
      let vScen = diner 0 visitor
          mScen = dinerMaya 0 maya
      checkCondition (initialWorld 0) (scenarioTerminal vScen)
        `shouldBe` False
      checkCondition (initialWorld 0) (scenarioTerminal mScen)
        `shouldBe` False
      -- visitorDawn terminates visitor but not Maya
      let wVDawn = addTag visitorDawn (initialWorld 0)
      checkCondition wVDawn (scenarioTerminal vScen) `shouldBe` True
      checkCondition wVDawn (scenarioTerminal mScen) `shouldBe` False
      -- mayaDawn terminates Maya but not visitor
      let wMDawn = addTag mayaDawn (initialWorld 0)
      checkCondition wMDawn (scenarioTerminal vScen) `shouldBe` False
      checkCondition wMDawn (scenarioTerminal mScen) `shouldBe` True
