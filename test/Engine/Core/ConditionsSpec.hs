module Engine.Core.ConditionsSpec (spec) where

import           Test.Hspec
import qualified Data.Map.Strict as Map

import           Engine.Core.Conditions
import           Engine.Core.World    (getWeather)
import           Engine.Author.DSL
import           Engine.CRDT.ORSet
import           GameTypes
import           TestFixtures

testTag :: Tag
testTag = ScenarioTag (MkScenarioTag "test-tag")

spec :: Spec
spec = describe "Engine.Core.Conditions" $ do

  describe "unconditional" $
    it "always passes" $
      checkCondition emptyWorld unconditional `shouldBe` True

  describe "Not" $ do
    it "inverts a passing condition" $
      checkCondition emptyWorld (Not unconditional) `shouldBe` False
    it "inverts a failing condition" $
      checkCondition emptyWorld (Not (HasWorldTag testTag)) `shouldBe` True
    it "double-negation is identity" $
      checkCondition emptyWorld (Not (Not (HasWorldTag testTag))) `shouldBe` False

  describe "All" $ do
    it "passes with an empty list" $
      checkCondition emptyWorld (All []) `shouldBe` True
    it "passes when all conditions pass" $
      checkCondition emptyWorld (All [unconditional, unconditional]) `shouldBe` True
    it "fails when any condition fails" $
      checkCondition emptyWorld (All [unconditional, HasWorldTag testTag]) `shouldBe` False
    it "can be nested inside Not" $
      checkCondition emptyWorld (Not (All [unconditional, HasWorldTag testTag])) `shouldBe` True

  describe "Any" $ do
    it "fails with an empty list" $
      checkCondition emptyWorld (Any []) `shouldBe` False
    it "passes when at least one condition passes" $
      checkCondition emptyWorld (Any [HasWorldTag testTag, unconditional]) `shouldBe` True
    it "fails when no conditions pass" $
      checkCondition emptyWorld (Any [HasWorldTag testTag, Not unconditional]) `shouldBe` False
    it "can be nested inside All" $
      checkCondition emptyWorld (All [unconditional, Any [HasWorldTag testTag, unconditional]]) `shouldBe` True

  describe "HasWorldTag" $ do
    let worldWithTag = emptyWorld { worldTags = orSingleton testTag }
    it "passes when the tag is present" $
      checkCondition worldWithTag (HasWorldTag testTag) `shouldBe` True
    it "fails when the tag is absent" $
      checkCondition emptyWorld (HasWorldTag testTag) `shouldBe` False

  describe "HasTag" $ do
    let worldWithCharTag = twoCharWorld
          { worldCharacters = Map.adjust (\c -> c { charTags = orSingleton testTag }) player
              (worldCharacters twoCharWorld)
          }
    it "passes when the character has the tag" $
      checkCondition worldWithCharTag (HasTag player testTag) `shouldBe` True
    it "fails when the character lacks the tag" $
      checkCondition twoCharWorld (HasTag player testTag) `shouldBe` False
    it "fails for an unknown character" $
      checkCondition emptyWorld (HasTag (Named "nobody") testTag) `shouldBe` False

  describe "RelationAbove / trustAbove" $ do
    it "passes when trust exceeds the threshold" $
      checkCondition twoCharWorld (trustAbove player npc 4) `shouldBe` True
    it "fails when trust equals the threshold" $
      checkCondition twoCharWorld (trustAbove player npc 5) `shouldBe` False
    it "fails for unknown characters" $
      checkCondition emptyWorld (trustAbove player npc 0) `shouldBe` False
    it "works for Perceived stat" $
      checkCondition emptyWorld (RelationAbove player npc (Perceived Intelligence) 0) `shouldBe` False

  describe "statAbove (ground truth)" $ do
    it "passes when the stat exceeds the threshold" $
      checkCondition twoCharWorld (statAbove player (Capacity Intelligence) 4) `shouldBe` True
    it "fails when the stat equals the threshold" $
      checkCondition twoCharWorld (statAbove player (Capacity Intelligence) 5) `shouldBe` False
    it "fails for an unknown character" $
      checkCondition emptyWorld (statAbove (Named "nobody") (Capacity Intelligence) 0) `shouldBe` False
    it "works for Strength" $
      checkCondition twoCharWorld (statAbove player (Capacity Strength) 4) `shouldBe` True
    it "works for Charisma" $
      checkCondition twoCharWorld (statAbove player (Capacity Charisma) 4) `shouldBe` True
    it "works for Understanding" $
      checkCondition twoCharWorld (statAbove player (Capacity Understanding) 4) `shouldBe` True

  describe "getWeather" $ do
    it "returns Nothing when no weather tag is set" $
      getWeather emptyWorld `shouldBe` Nothing
    it "returns the weather description when a weather tag is set" $
      let w = emptyWorld { worldTags = orSingleton (weatherTag (WeatherDesc "rain")) }
      in getWeather w `shouldBe` Just (WeatherDesc "rain")
