-- | Tests for Engine.Sync.Progress: the per-identity epoch + hunt
-- counter used as scaffolding for the Tier-2 lifetime find.
module Engine.Sync.ProgressSpec (spec) where

import qualified Data.Map.Strict as Map
import           Control.Exception (bracket_)
import           System.Directory  (createDirectoryIfMissing, removePathForcibly)
import           System.FilePath   ((</>))
import           Test.Hspec

import           Engine.Sync.Progress
import           GameTypes            (PlayerId (..))

pid :: PlayerId
pid = PlayerId "testplayer"

-- | Run a test body inside a scratch directory that is wiped both
-- before and after the body runs.  Mirrors the pattern the Identity
-- spec uses — no extra dependency on the @temporary@ package.
withScratch :: (FilePath -> IO a) -> IO a
withScratch act = do
  let dir = "/tmp/throughline-test-progress"
  bracket_
    (do removePathForcibly dir; createDirectoryIfMissing True dir)
    (removePathForcibly dir)
    (act dir)

spec :: Spec
spec = describe "Engine.Sync.Progress" $ do

  it "returns a default record for a fresh identity without writing" $ do
    withScratch $ \dir -> do
      let path = dir </> "progress.json"
      p <- getProgress pid path
      progressEpoch     p `shouldBe` 1
      progressHuntCount p `shouldBe` 0
      m <- loadAll path
      Map.null m `shouldBe` True

  it "persists and re-loads a record after recordHunt" $ do
    withScratch $ \dir -> do
      let path = dir </> "progress.json"
      _  <- recordHunt pid path
      _  <- recordHunt pid path
      p3 <- recordHunt pid path
      progressHuntCount p3 `shouldBe` 3
      reloaded <- getProgress pid path
      progressHuntCount reloaded `shouldBe` 3
      progressEpoch     reloaded `shouldBe` 1

  it "rotateEpoch bumps epoch and resets hunt count" $ do
    withScratch $ \dir -> do
      let path = dir </> "progress.json"
      _ <- recordHunt pid path
      _ <- recordHunt pid path
      _ <- recordHunt pid path
      r <- rotateEpoch pid path
      progressEpoch     r `shouldBe` 2
      progressHuntCount r `shouldBe` 0
      reloaded <- getProgress pid path
      progressEpoch     reloaded `shouldBe` 2
      progressHuntCount reloaded `shouldBe` 0

  it "keeps multiple identities' records separate" $ do
    withScratch $ \dir -> do
      let path  = dir </> "progress.json"
          other = PlayerId "otherplayer"
      _ <- recordHunt pid   path
      _ <- recordHunt pid   path
      _ <- recordHunt other path
      p <- getProgress pid   path
      o <- getProgress other path
      progressHuntCount p `shouldBe` 2
      progressHuntCount o `shouldBe` 1

  it "treats a missing file as empty" $ do
    withScratch $ \dir -> do
      let path = dir </> "progress.json"
      m <- loadAll path
      Map.null m `shouldBe` True

  it "tolerates a malformed file by treating it as empty" $ do
    withScratch $ \dir -> do
      let path = dir </> "progress.json"
      writeFile path "not valid json"
      m <- loadAll path
      Map.null m `shouldBe` True
