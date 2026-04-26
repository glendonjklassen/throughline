module Engine.Author.NarrativeSpec (spec) where

import           Test.Hspec

import           Engine.Author.Narrative
import           Engine.Core.World    (setCharacterStat)
import           GameTypes
import           TestFixtures

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

withUnderstanding :: Int -> GameWorld -> GameWorld
withUnderstanding n world = world
  { worldGraph = setCharacterStat player (Capacity Understanding) n (worldGraph world) }

highWorld, midWorld, lowWorld, silentWorld :: GameWorld
highWorld   = withUnderstanding 7 twoCharWorld  -- >= 7: named, precise
midWorld    = withUnderstanding 5 twoCharWorld  -- >= 3: directional
lowWorld    = withUnderstanding 2 twoCharWorld  -- >= 1: barely perceptible
silentWorld = withUnderstanding 0 twoCharWorld  -- 0: silence

spec :: Spec
spec = describe "Engine.Author.Narrative" $ do

  -- -------------------------------------------------------------------------
  -- tiered (Understanding levels)
  -- -------------------------------------------------------------------------

  describe "tiered" $ do
    it "returns the high string at Understanding >= 7" $
      narrateEffect player highWorld (ModifyRelation player npc Trust 1)
        `shouldBe` Just "Player feels something ease between them and NPC."

    it "returns the mid string at Understanding >= 3" $
      narrateEffect player midWorld (ModifyRelation player npc Trust 1)
        `shouldBe` Just "Something eases between Player and NPC."

    it "returns the low string at Understanding >= 1" $
      narrateEffect player lowWorld (ModifyRelation player npc Trust 1)
        `shouldBe` Just "You sense a subtle warmth in the air."

    it "returns Nothing at Understanding 0" $
      narrateEffect player silentWorld (ModifyRelation player npc Trust 1)
        `shouldBe` Nothing

  -- -------------------------------------------------------------------------
  -- ModifyStat narration
  -- -------------------------------------------------------------------------

  describe "ModifyStat capacity stats" $ do
    it "returns Nothing for Intelligence (no default narration)" $
      narrateEffect player midWorld (ModifyRelation Truth player (Capacity Intelligence) (-1))
        `shouldBe` Nothing
    it "returns Nothing for Strength (no default narration)" $
      narrateEffect player midWorld (ModifyRelation Truth player (Capacity Strength) (-1))
        `shouldBe` Nothing
    it "returns Nothing for Charisma (no default narration)" $
      narrateEffect player midWorld (ModifyRelation Truth player (Capacity Charisma) (-1))
        `shouldBe` Nothing
    it "returns Nothing for Hunger (no default narration)" $
      narrateEffect player midWorld (ModifyRelation Truth player (Capacity Hunger) (-1))
        `shouldBe` Nothing
    it "returns Nothing for Understanding (no default narration)" $
      narrateEffect player midWorld (ModifyRelation Truth player (Capacity Understanding) (-1))
        `shouldBe` Nothing

  -- -------------------------------------------------------------------------
  -- ModifyRelation Trust narration
  -- -------------------------------------------------------------------------

  describe "ModifyRelation Trust" $ do
    it "narrates a trust increase" $
      narrateEffect player midWorld (ModifyRelation player npc Trust 4)
        `shouldBe` Just "Something eases between Player and NPC."

    it "narrates a trust decrease" $
      narrateEffect player midWorld (ModifyRelation player npc Trust (-4))
        `shouldBe` Just "Something changes between Player and NPC."

    it "returns Nothing when delta is zero" $
      narrateEffect player midWorld (ModifyRelation player npc Trust 0) `shouldBe` Nothing

    it "narrates with character names at high Understanding" $
      narrateEffect player highWorld (ModifyRelation player npc Trust 4)
        `shouldBe` Just "Player feels something ease between them and NPC."

    it "returns Nothing at Understanding 0" $
      narrateEffect player silentWorld (ModifyRelation player npc Trust 4) `shouldBe` Nothing

  describe "ModifyRelation non-Trust" $
    it "returns Nothing (no narration defined for other relation stats)" $
      narrateEffect player midWorld (ModifyRelation player npc (Perceived Intelligence) 4) `shouldBe` Nothing

  describe "name" $ do
    it "returns the character's name when the character exists" $
      narrateEffect player highWorld (ModifyRelation player npc Trust 1)
        `shouldBe` Just "Player feels something ease between them and NPC."
    it "falls back to the CharacterId when the character does not exist" $
      narrateEffect player highWorld (ModifyRelation (Named "ghost") npc Trust 1)
        `shouldBe` Just "ghost feels something ease between them and NPC."

  -- -------------------------------------------------------------------------
  -- Unnarrated effects
  -- -------------------------------------------------------------------------

  describe "unnarrated effects" $ do
    it "returns Nothing for AddWorldTag" $
      narrateEffect player midWorld (AddWorldTag (ScenarioTag (MkScenarioTag "x"))) `shouldBe` Nothing
    it "returns Nothing for DoNothing" $
      narrateEffect player midWorld DoNothing `shouldBe` Nothing
    it "returns Nothing for Say" $
      narrateEffect player midWorld (Say player [] "hello") `shouldBe` Nothing

  -- -------------------------------------------------------------------------
  -- playerUnderstanding
  -- -------------------------------------------------------------------------

  describe "playerUnderstanding" $ do
    it "returns the player's Understanding stat" $
      playerUnderstanding player midWorld `shouldBe` 5
    it "returns 0 when no player character exists" $
      playerUnderstanding player emptyWorld `shouldBe` 0
