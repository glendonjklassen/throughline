-- | Tests for the engine's competency vocabulary + per-identity
-- grant/read pipeline.
module Engine.CompetencySpec (spec) where

import qualified Data.Aeson      as Aeson
import qualified Data.Set        as Set
import           Control.Exception (bracket_)
import           System.Directory  (createDirectoryIfMissing, removePathForcibly)
import           System.FilePath   ((</>))
import           Test.Hspec

import           Engine.Competency
import           Engine.Sync.Progress (Progress (..), defaultProgress,
                                       getProgress, grantCompetency,
                                       hasCompetency, recordHunt, rotateEpoch)
import           GameTypes            (PlayerId (..))


pid :: PlayerId
pid = PlayerId "competency-test-player"

-- | Run a test body inside a wiped scratch directory.  Mirrors the
-- pattern used by ProgressSpec / IdentitySpec.
withScratch :: (FilePath -> IO a) -> IO a
withScratch act = do
  let dir = "/tmp/throughline-test-competency"
  bracket_
    (do removePathForcibly dir; createDirectoryIfMissing True dir)
    (removePathForcibly dir)
    (act dir)

-- | Helper extracted from the JSON round-trip 'it' block to keep the
-- spec's indentation flat (a @where@ clause inside an @it@ confuses
-- the layout for following @describe@ blocks).
roundTripsCompetency :: Competency -> Expectation
roundTripsCompetency c = case Aeson.fromJSON (Aeson.toJSON c) of
  Aeson.Success c' -> c' `shouldBe` c
  Aeson.Error e    -> expectationFailure
    ("round-trip failed for " <> show c <> ": " <> e)

spec :: Spec
spec = describe "Engine.Competency" $ do

  describe "vocabulary" $ do

    it "allCompetencies enumerates every constructor exactly once" $ do
      length allCompetencies `shouldBe` length [(minBound :: Competency) .. maxBound]
      Set.size (Set.fromList allCompetencies) `shouldBe` length allCompetencies

    it "competencyName is unique across the vocabulary" $ do
      let names = map competencyName allCompetencies
      Set.size (Set.fromList names) `shouldBe` length names

    it "JSON round-trips every competency through its on-disk name" $
      mapM_ roundTripsCompetency allCompetencies

  describe "Progress integration" $ do

    it "default progress carries no competencies" $ do
      p <- defaultProgress
      Set.null (progressCompetencies p) `shouldBe` True
      mapM_ (\c -> hasCompetency c p `shouldBe` False) allCompetencies

    it "grantCompetency persists and re-reads as a set member" $
      withScratch $ \dir -> do
        let path = dir </> "progress.json"
        _   <- grantCompetency pid path WaitingWithoutAct
        p   <- getProgress    pid path
        hasCompetency WaitingWithoutAct  p `shouldBe` True
        hasCompetency AnimalSignReading  p `shouldBe` False

    it "grantCompetency is idempotent (set semantics)" $
      withScratch $ \dir -> do
        let path = dir </> "progress.json"
        _ <- grantCompetency pid path NightVision
        _ <- grantCompetency pid path NightVision
        _ <- grantCompetency pid path NightVision
        p <- getProgress     pid path
        Set.size (progressCompetencies p) `shouldBe` 1

    it "competencies survive epoch rotation (cross-epoch by design)" $
      withScratch $ \dir -> do
        let path = dir </> "progress.json"
        _ <- grantCompetency pid path AnimalSignReading
        _ <- grantCompetency pid path WeatherPrediction
        _ <- recordHunt      pid path
        _ <- rotateEpoch     pid path
        p <- getProgress     pid path
        progressEpoch p              `shouldBe` 2
        progressHuntCount p          `shouldBe` 0
        hasCompetency AnimalSignReading p `shouldBe` True
        hasCompetency WeatherPrediction p `shouldBe` True

    it "loads legacy records (no competencies field) as empty" $
      withScratch $ \dir -> do
        let path = dir </> "progress.json"
        -- Write a hand-crafted legacy file: one record whose
        -- progress object omits the competencies field entirely.
        writeFile path $ unlines
          [ "{"
          , "  \"version\": 1,"
          , "  \"records\": ["
          , "    {"
          , "      \"playerId\": \"competency-test-player\","
          , "      \"progress\": {"
          , "        \"epoch\": 1,"
          , "        \"huntCount\": 3,"
          , "        \"lifetimeFind\": { \"state\": \"pending\" },"
          , "        \"updatedAt\": \"2026-01-01T00:00:00Z\""
          , "      }"
          , "    }"
          , "  ]"
          , "}"
          ]
        p <- getProgress pid path
        progressHuntCount p              `shouldBe` 3
        Set.null (progressCompetencies p) `shouldBe` True
