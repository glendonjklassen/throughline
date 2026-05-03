-- | Scenario-side tests for the DeerHunt competency wiring:
-- * the pure 'competenciesEarned' classifier from final-world tags;
-- * the 'experiencedSitterAxiom' once-per-hunt narrate beat.
--
-- The engine-side grant/persist mechanics are covered by
-- 'Engine.CompetencySpec'; this spec only proves the scenario is
-- producing the right inputs.
module Scenarios.DeerHuntCompetencySpec (spec) where

import qualified Data.Map.Strict       as Map
import qualified Data.Set              as Set
import           Test.Hspec

import           Engine.Author.DSL     (tagsFromList)
import           Engine.CRDT.ORSet     (orFromList)
import           GameTypes
import           Engine.Competency     (Competency (..))
import           Engine.Sync.Progress  (Progress (..))

import           Scenarios.DeerHunt              (competenciesEarned,
                                                  experiencedSitterAxiom)
import           Scenarios.DeerHunt.Constants    (foundSignBed, foundSignRub,
                                                  foundSignScrape, foundSignTracks,
                                                  playerSitting,
                                                  waitedSitNarrated,
                                                  waitedStillMilestone)
import           Scenarios.DeerHuntTestFixtures  (fixtureProgress)


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Empty world carrying just the listed world tags.  Mirrors the
-- pattern in 'DeerHuntWhiteStagSpec' — the helpers we're testing read
-- world tags only, so the rest of the world can be vacant.
worldWith :: [Tag] -> GameWorld
worldWith tags = GameWorld
  { worldCharacters      = Map.empty
  , worldGraph           = Map.empty
  , worldLocations       = Map.empty
  , worldActiveEffects   = []
  , worldClock           = LamportClock 0 (PlayerId "test")
  , worldTags            = tagsFromList tags
  , worldLocationGraph   = LocationGraph Set.empty Map.empty Map.empty
  , worldSeed            = 0
  , worldLocationHistory = Map.empty
  , worldLocationVisits  = Map.empty
  , worldJournal         = []
  , worldDayNumber       = 0
  }

emptyDiff :: WorldDiff
emptyDiff = WorldDiff [] [] [] [] [] [] [] [] 0

isNarrate :: EffectBody -> Bool
isNarrate (Narrate _) = True
isNarrate _           = False


-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "DeerHunt — competencies" $ do

  describe "competenciesEarned (end-of-hunt classifier)" $ do

    it "returns no competencies for an empty world" $
      competenciesEarned (worldWith []) `shouldBe` []

    it "grants WaitingWithoutAct when the milestone tag was latched" $ do
      competenciesEarned (worldWith [waitedStillMilestone])
        `shouldBe` [WaitingWithoutAct]

    it "grants AnimalSignReading at three distinct found-sign tags" $ do
      competenciesEarned
        (worldWith [foundSignTracks, foundSignBed, foundSignRub])
        `shouldBe` [AnimalSignReading]

    it "withholds AnimalSignReading at two found-sign tags" $ do
      competenciesEarned (worldWith [foundSignTracks, foundSignBed])
        `shouldBe` []

    it "grants both when both conditions are met" $ do
      competenciesEarned
        (worldWith [ waitedStillMilestone
                   , foundSignTracks
                   , foundSignBed
                   , foundSignScrape
                   ])
        `shouldBe` [WaitingWithoutAct, AnimalSignReading]

  describe "experiencedSitterAxiom (read-time narrate beat)" $ do

    let you = Named "you"

    it "stays silent when the player lacks WaitingWithoutAct" $ do
      let ax    = experiencedSitterAxiom you fixtureProgress
          world = worldWith []
          diff  = emptyDiff { diffWorldTagsAdded = [playerSitting] }
      axiomEvaluate ax world [] diff `shouldBe` []

    it "stays silent if the player has the competency but didn't sit this tick" $ do
      let progress = fixtureProgress
            { progressCompetencies = Set.singleton WaitingWithoutAct }
          ax    = experiencedSitterAxiom you progress
          world = worldWith []
          diff  = emptyDiff
      axiomEvaluate ax world [] diff `shouldBe` []

    it "fires once when an experienced sitter sits down for the first time" $ do
      let progress = fixtureProgress
            { progressCompetencies = Set.singleton WaitingWithoutAct }
          ax    = experiencedSitterAxiom you progress
          world = worldWith []
          diff  = emptyDiff { diffWorldTagsAdded = [playerSitting] }
          fx    = axiomEvaluate ax world [] diff
      length fx `shouldBe` 2
      let bodies = map effectBody fx
      any isNarrate bodies                          `shouldBe` True
      AddWorldTag waitedSitNarrated `elem` bodies   `shouldBe` True

    it "does not re-fire if waitedSitNarrated is already on the world" $ do
      let progress = fixtureProgress
            { progressCompetencies = Set.singleton WaitingWithoutAct }
          ax    = experiencedSitterAxiom you progress
          world = (worldWith [waitedSitNarrated])
                   { worldTags = orFromList [waitedSitNarrated] }
          diff  = emptyDiff { diffWorldTagsAdded = [playerSitting] }
      axiomEvaluate ax world [] diff `shouldBe` []
