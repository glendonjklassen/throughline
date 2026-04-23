module SDL.SaveSlotsSpec (spec) where

import           Control.Exception  (bracket_)
import           Control.Monad      (when)
import           Data.IORef         (newIORef)
import           System.Directory   (createDirectoryIfMissing, doesFileExist,
                                     removeDirectoryRecursive)
import           System.FilePath    ((</>))

import           Test.Hspec

import           Engine.Runtime      (sessionsRootDir)
import           Engine.Sync.EventLog (logFileName)
import           Engine.Sync.Snapshot (snapshotFileName)
import           GameTypes           (PlayerId(..))
import           SDL.SaveSlots       (SaveStatus(..), resetScenarioSave,
                                      scenarioSaveStatus)

-- Picks a throwaway scenario name so the test doesn't conflict with
-- real saves in the repo's sessions directory.
testScenario :: String
testScenario = "___saveslots-spec___"

testPlayer :: PlayerId
testPlayer = PlayerId "test-player"

testDir :: FilePath
testDir = sessionsRootDir </> testScenario </> "test-player"

withCleanSlate :: IO a -> IO a
withCleanSlate = bracket_ (cleanup >> pure (newIORef ())) cleanup
  where
    cleanup = do
      e <- doesFileExist (testDir </> logFileName)
      when e $ removeDirectoryRecursive (sessionsRootDir </> testScenario)

spec :: Spec
spec = describe "SDL.SaveSlots" $ do

  it "reports no save when the scenario directory is empty" $ withCleanSlate $ do
    s <- scenarioSaveStatus testPlayer testScenario
    s `shouldBe` SaveStatus False 0

  it "counts entries in an existing log file" $ withCleanSlate $ do
    createDirectoryIfMissing True testDir
    writeFile (testDir </> logFileName)
      "{\"id\":\"1-p\"}\n{\"id\":\"2-p\"}\n{\"id\":\"3-p\"}\n"
    s <- scenarioSaveStatus testPlayer testScenario
    hasSave s `shouldBe` True
    saveEntryCount s `shouldBe` 3

  it "reset removes both the log and snapshot files" $ withCleanSlate $ do
    createDirectoryIfMissing True testDir
    writeFile (testDir </> logFileName) "{\"id\":\"1\"}\n"
    writeFile (testDir </> snapshotFileName) "{}"
    resetScenarioSave testPlayer testScenario
    logGone  <- not <$> doesFileExist (testDir </> logFileName)
    snapGone <- not <$> doesFileExist (testDir </> snapshotFileName)
    logGone  `shouldBe` True
    snapGone `shouldBe` True

  it "reset is a no-op when there's nothing to remove" $ withCleanSlate $ do
    -- Must not throw even when the directory doesn't exist yet.
    resetScenarioSave testPlayer testScenario
    s <- scenarioSaveStatus testPlayer testScenario
    s `shouldBe` SaveStatus False 0
