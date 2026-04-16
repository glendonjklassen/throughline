module Engine.Author.CommonAxiomsSpec (spec) where

import qualified Data.Map.Strict as Map
import           Test.Hspec

import           Engine.Author.CommonAxioms
import           Engine.Core.Axioms         (runAxioms)
import           Engine.Core.World          (setCharacterStat)
import           Engine.CRDT.ORSet
import           GameTypes
import           TestFixtures

shop :: Location
shop = Location "shop"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | World with player at shop and a specific hour + stat value.
worldAt :: Int -> Int -> GameWorld
worldAt hour str = emptyWorld
  { worldCharacters = Map.singleton player (Character player "Player" [] orEmpty)
  , worldGraph      = setCharacterStat player (Capacity Strength) str Map.empty
  , worldLocations  = Map.singleton player shop
  , worldTags       = orFromList [timeTag hour]
  }

-- | Diff that signals a new hour arrived.
hourDiff :: Int -> WorldDiff
hourDiff h = emptyDiff { diffWorldTagsAdded = [timeTag h] }

-- | Diff that signals a weather change.
weatherDiff :: WeatherDesc -> WorldDiff
weatherDiff w = emptyDiff { diffWorldTagsAdded = [weatherTag w] }

-- | Diff that signals a Strength stat drop.
strengthDropDiff :: CharId -> Int -> Int -> WorldDiff
strengthDropDiff cid old new = emptyDiff
  { diffStats = [StatDelta cid (Capacity Strength) old new (PlayerId "test")] }

spec :: Spec
spec = describe "Engine.Author.CommonAxioms" $ do

  -- -------------------------------------------------------------------------
  -- System axiom: fatigueSystemAxiom (via systemAxioms in runAxioms)
  -- -------------------------------------------------------------------------

  describe "fatigueSystemAxiom (system)" $ do

    it "no drain during morning hours (6-9)" $ do
      let world   = worldAt 7 5
          effects = runAxioms [] world [] (hourDiff 7)
          deltas  = [ d | e <- effects, ModifyRelation _ _ (Capacity Strength) d <- [effectBody e] ]
      deltas `shouldBe` []

    it "moderate drain at midday (10-12)" $ do
      let world   = worldAt 11 5
          effects = runAxioms [] world [] (hourDiff 11)
          deltas  = [ d | e <- effects, ModifyRelation _ _ (Capacity Strength) d <- [effectBody e] ]
      deltas `shouldBe` [-1]

    it "heavy drain during afternoon slump (13-15)" $ do
      let world   = worldAt 14 5
          effects = runAxioms [] world [] (hourDiff 14)
          deltas  = [ d | e <- effects, ModifyRelation _ _ (Capacity Strength) d <- [effectBody e] ]
      deltas `shouldBe` [-2]

    it "restores Strength while sleeping regardless of hour" $ do
      let world   = (worldAt 22 3)
            { worldCharacters = Map.singleton player
                (Character player "Player" [] (orFromList [sleepingTag])) }
          effects = runAxioms [] world [] (hourDiff 22)
          deltas  = [ d | e <- effects, ModifyRelation _ _ (Capacity Strength) d <- [effectBody e] ]
      deltas `shouldBe` [1]

    it "no effect when hour has not ticked" $ do
      let world   = worldAt 14 5
          effects = runAxioms [] world [] emptyDiff
          -- Filter out non-fatigue effects (location transition etc.)
          fatigue = [ d | e <- effects, ModifyRelation _ _ (Capacity Strength) d <- [effectBody e] ]
      fatigue `shouldBe` []

    it "drains all characters with Strength stat" $ do
      let world = emptyWorld
            { worldCharacters = Map.fromList
                [ (player, Character player "Player" [] orEmpty)
                , (npc,    Character npc    "NPC"    [] orEmpty)
                ]
            , worldGraph = setCharacterStat player (Capacity Strength) 5
                         . setCharacterStat npc    (Capacity Strength) 5
                         $ Map.empty
            , worldTags = orFromList [timeTag 14]
            }
          effects = runAxioms [] world [] (hourDiff 14)
          deltas  = [ (c, d) | e <- effects
                    , ModifyRelation _ c (Capacity Strength) d <- [effectBody e] ]
      deltas `shouldContain` [(player, -2)]
      deltas `shouldContain` [(npc, -2)]

  -- -------------------------------------------------------------------------
  -- System axiom: tirednessSystemAxiom (via systemAxioms in runAxioms)
  -- -------------------------------------------------------------------------

  describe "tirednessSystemAxiom (system)" $ do

    it "sets Fatigue Tired when Strength drops to 3" $ do
      let world   = worldAt 12 3
          diff    = strengthDropDiff player 4 3
          effects = runAxioms [] world [] diff
          tags    = [ t | e <- effects, AddTag _ t <- [effectBody e], isFatigueTag t ]
      tags `shouldBe` [fatigueTag Tired]

    it "sets Fatigue Exhausted when Strength drops to 1" $ do
      let world   = worldAt 12 1
          diff    = strengthDropDiff player 2 1
          effects = runAxioms [] world [] diff
          tags    = [ t | e <- effects, AddTag _ t <- [effectBody e], isFatigueTag t ]
      tags `shouldBe` [fatigueTag Exhausted]

    it "clears fatigue when Strength rises above 3" $ do
      let world   = (worldAt 12 5)
            { worldCharacters = Map.singleton player
                (Character player "Player" [] (orFromList [fatigueTag Tired])) }
          diff    = strengthDropDiff player 3 5
          effects = runAxioms [] world [] diff
          removes = [ t | e <- effects, RemoveTag _ t <- [effectBody e], isFatigueTag t ]
      removes `shouldNotBe` []

  -- -------------------------------------------------------------------------
  -- weatherInfluenceAxiom (still a CommonAxiom)
  -- -------------------------------------------------------------------------

  describe "weatherInfluenceAxiom" $ do

    let influence (WeatherDesc "Stormy") = [(Capacity Charisma, -1)]
        influence _                       = []
        axiom = weatherInfluenceAxiom player influence

    it "applies stat delta when stormy weather arrives" $ do
      let world   = (worldAt 12 5) { worldTags = orFromList [timeTag 12, weatherTag (WeatherDesc "Stormy")] }
          effects = runAxioms [axiom] world [] (weatherDiff (WeatherDesc "Stormy"))
          deltas  = [ d | e <- effects, ModifyRelation _ _ (Capacity Charisma) d <- [effectBody e] ]
      deltas `shouldBe` [-1]

    it "no effect for clear weather" $ do
      let world   = (worldAt 12 5) { worldTags = orFromList [timeTag 12, weatherTag (WeatherDesc "Clear")] }
          effects = runAxioms [axiom] world [] (weatherDiff (WeatherDesc "Clear"))
          -- Filter to only Charisma effects from weatherInfluence
          deltas  = [ d | e <- effects, ModifyRelation _ _ (Capacity Charisma) d <- [effectBody e] ]
      deltas `shouldBe` []

    it "no effect when weather has not changed" $ do
      let world   = (worldAt 12 5) { worldTags = orFromList [timeTag 12, weatherTag (WeatherDesc "Stormy")] }
          effects = runAxioms [axiom] world [] emptyDiff
          deltas  = [ d | e <- effects, ModifyRelation _ _ (Capacity Charisma) d <- [effectBody e] ]
      deltas `shouldBe` []

  -- -------------------------------------------------------------------------
  -- moodDriftAxiom (still a CommonAxiom)
  -- -------------------------------------------------------------------------

  describe "moodDriftAxiom" $ do

    let axiom = moodDriftAxiom player [(Capacity Charisma, 5)]

    it "drifts stat toward baseline when above" $ do
      let world  = (worldAt 12 5)
            { worldGraph = setCharacterStat player (Capacity Charisma) 8 Map.empty }
          effects = runAxioms [axiom] world [] (hourDiff 12)
          deltas  = [ d | e <- effects, ModifyRelation _ _ (Capacity Charisma) d <- [effectBody e] ]
      deltas `shouldContain` [-1]

    it "drifts stat toward baseline when below" $ do
      let world  = (worldAt 12 5)
            { worldGraph = setCharacterStat player (Capacity Charisma) 2 Map.empty }
          effects = runAxioms [axiom] world [] (hourDiff 12)
          deltas  = [ d | e <- effects, ModifyRelation _ _ (Capacity Charisma) d <- [effectBody e] ]
      deltas `shouldContain` [1]

    it "no effect when stat is at baseline" $ do
      let world  = (worldAt 12 5)
            { worldGraph = setCharacterStat player (Capacity Charisma) 5 Map.empty }
          effects = runAxioms [axiom] world [] (hourDiff 12)
          deltas  = [ d | e <- effects, ModifyRelation _ _ (Capacity Charisma) d <- [effectBody e] ]
      deltas `shouldBe` []

    it "no effect when hour has not ticked" $ do
      let world  = (worldAt 12 5)
            { worldGraph = setCharacterStat player (Capacity Charisma) 8 Map.empty }
          effects = runAxioms [axiom] world [] emptyDiff
          deltas  = [ d | e <- effects, ModifyRelation _ _ (Capacity Charisma) d <- [effectBody e] ]
      deltas `shouldBe` []
