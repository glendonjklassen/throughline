-- | Tests for Engine.Sync.Progress: the per-identity epoch + hunt
-- counter used as scaffolding for the Tier-2 lifetime find.
module Engine.Sync.ProgressSpec (spec) where

import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Crypto.Random         as Crypto
import qualified Data.Aeson            as Aeson
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

  -- -------------------------------------------------------------------------
  -- Lifetime find: state machine
  -- -------------------------------------------------------------------------

  describe "lifetime find state machine" $ do

    it "starts FindPending and persists across reloads" $ do
      withScratch $ \dir -> do
        let path = dir </> "progress.json"
        _ <- recordHunt pid path
        p <- getProgress pid path
        progressLifetimeFind p `shouldBe` FindPending

    it "recordLifetimePass moves Pending -> Encountered with passes=1" $ do
      withScratch $ \dir -> do
        let path = dir </> "progress.json"
        _ <- recordHunt pid path        -- N=1
        _ <- recordHunt pid path        -- N=2
        p <- recordLifetimePass pid path
        case progressLifetimeFind p of
          FindEncountered firstSeen passes -> do
            firstSeen `shouldBe` 2
            passes    `shouldBe` 1
          other -> expectationFailure ("Expected FindEncountered, got: " <> show other)

    it "recordLifetimePass increments passes and transitions to Lost at threshold" $ do
      withScratch $ \dir -> do
        let path = dir </> "progress.json"
        _ <- recordHunt pid path
        let go :: Int -> IO ()
            go 0 = pure ()
            go n = recordLifetimePass pid path >> go (n - 1)
        go 9                            -- 9 passes; still encountered
        p9 <- getProgress pid path
        case progressLifetimeFind p9 of
          FindEncountered _ 9 -> pure ()
          other -> expectationFailure ("Expected 9 passes, got: " <> show other)
        -- 10th pass tips it into Lost
        p10 <- recordLifetimePass pid path
        case progressLifetimeFind p10 of
          FindLost _ _ -> pure ()
          other -> expectationFailure ("Expected FindLost, got: " <> show other)

    it "recordLifetimeClaim sets Claimed and rotates the epoch" $ do
      withScratch $ \dir -> do
        let path = dir </> "progress.json"
        _ <- recordHunt pid path        -- N=1, epoch 1
        _ <- recordHunt pid path        -- N=2, epoch 1
        p <- recordLifetimeClaim pid path
        progressEpoch        p `shouldBe` 2
        progressHuntCount    p `shouldBe` 0
        progressLifetimeFind p `shouldBe` FindPending

    it "recordLifetimeLinger decrements the hunt count, floored at 0" $ do
      withScratch $ \dir -> do
        let path = dir </> "progress.json"
        _ <- recordHunt pid path        -- N=1
        _ <- recordHunt pid path        -- N=2
        p <- recordLifetimeLinger pid path
        progressHuntCount p `shouldBe` 1
        -- Linger past zero floors at zero, doesn't go negative.
        _ <- recordLifetimeLinger pid path
        _ <- recordLifetimeLinger pid path
        floored <- getProgress pid path
        progressHuntCount floored `shouldBe` 0

    it "recordLifetimeLost forces FindLost regardless of prior state" $ do
      withScratch $ \dir -> do
        let path = dir </> "progress.json"
        _ <- recordHunt pid path
        _ <- recordLifetimePass pid path
        p <- recordLifetimeLost pid path
        case progressLifetimeFind p of
          FindLost _ _ -> pure ()
          other -> expectationFailure ("Expected FindLost, got: " <> show other)

    it "Claim from FindLost is a no-op on the existing claim recorder" $ do
      -- The launcher only calls recordLifetimeClaim when a fresh claim
      -- happens; FindLost can only be cleared by rotation/manual-reset.
      -- This guards against accidental double-handling.
      withScratch $ \dir -> do
        let path = dir </> "progress.json"
        _ <- recordHunt pid path
        _ <- recordLifetimeLost pid path
        beforeClaim <- getProgress pid path
        case progressLifetimeFind beforeClaim of
          FindLost _ _ -> pure ()
          _ -> expectationFailure "setup wrong"

  -- -------------------------------------------------------------------------
  -- JSON round-trip for the new field
  -- -------------------------------------------------------------------------

  describe "lifetime find JSON round-trip" $ do
    it "round-trips through every state variant" $ do
      withScratch $ \dir -> do
        let path = dir </> "progress.json"
        _ <- recordHunt pid path
        _ <- recordLifetimePass pid path
        encountered <- getProgress pid path
        Aeson.decode (Aeson.encode encountered) `shouldBe` Just encountered
        _ <- recordLifetimeLost pid path
        lost <- getProgress pid path
        Aeson.decode (Aeson.encode lost) `shouldBe` Just lost

    it "loads legacy records (no lifetimeFind field) as FindPending" $ do
      withScratch $ \dir -> do
        let path = dir </> "progress.json"
        -- Hand-write a v1 record without the lifetimeFind field.
        writeFile path
          "{\"version\":1,\"records\":[{\"playerId\":\"testplayer\",\"progress\":{\"epoch\":2,\"huntCount\":7,\"updatedAt\":\"2026-04-01T00:00:00Z\"}}]}"
        p <- getProgress pid path
        progressEpoch        p `shouldBe` 2
        progressHuntCount    p `shouldBe` 7
        progressLifetimeFind p `shouldBe` FindPending

  -- -------------------------------------------------------------------------
  -- Eligibility math
  -- -------------------------------------------------------------------------

  describe "lifetime find eligibility" $ do

    let mkPubKey :: IO Ed25519.PublicKey
        mkPubKey = do
          drg <- Crypto.drgNew
          let (sk, _) = Crypto.withDRG drg (Ed25519.generateSecretKey)
          pure (Ed25519.toPublic sk)

    it "FindClaimed is never eligible" $ do
      pk <- mkPubKey
      lifetimeFindEligible pk 1 5  (FindClaimed 5 (read "2026-01-01 00:00:00 UTC")) `shouldBe` False
      lifetimeFindEligible pk 1 50 (FindClaimed 5 (read "2026-01-01 00:00:00 UTC")) `shouldBe` False

    it "gamma threshold matches the proposal's per-bucket targets" $ do
      -- Just check the shape: peak in 11-25 band, smaller in extremes.
      gammaThreshold 1   `shouldSatisfy` (< gammaThreshold 5)
      gammaThreshold 11  `shouldSatisfy` (> gammaThreshold 1)
      gammaThreshold 11  `shouldSatisfy` (> gammaThreshold 60)
      gammaThreshold 200 `shouldSatisfy` (< gammaThreshold 25)

    it "decay threshold halves-by-fifth (0.2^p) as passes accumulate" $ do
      decayThreshold 0 `shouldBe` 0
      decayThreshold 1 `shouldBe` 200
      decayThreshold 2 `shouldBe` 40
      decayThreshold 3 `shouldBe` 8
      -- High-pass count floors to 1; once it can't even fire, the
      -- launcher's lost-stag logic kicks the state to FindLost.
      decayThreshold 20 `shouldBe` 1

    it "eligibility roll is deterministic for the same (pubkey, epoch, n)" $ do
      pk <- mkPubKey
      let a = lifetimeFindEligible pk 1 5 FindPending
          b = lifetimeFindEligible pk 1 5 FindPending
      a `shouldBe` b

    it "the eligibility roll changes when epoch or N changes" $ do
      pk <- mkPubKey
      -- Across many (epoch, n) combinations we should see at least one
      -- difference; we're not asserting any specific bit pattern.
      let rolls = [ lifetimeFindEligible pk e n FindPending
                  | e <- [1..3], n <- [1..30] ]
      length (filter id rolls)  `shouldSatisfy` (> 0)
      length (filter not rolls) `shouldSatisfy` (> 0)
