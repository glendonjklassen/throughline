{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the relic client: local fragment/bundle storage,
-- stubbed transport, and JSON round-trip of every wire type.
module Engine.Sync.RelicSpec (spec) where

import qualified Data.Aeson              as Aeson
import qualified Data.Map.Strict         as Map
import           Control.Exception       (bracket_)
import           Data.Time.Calendar      (fromGregorian)
import           Data.Time.Clock         (UTCTime(..))
import           System.Directory        (createDirectoryIfMissing,
                                          removePathForcibly)
import           Test.Hspec

import           Engine.Sync.Relic

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

withScratch :: (FilePath -> IO a) -> IO a
withScratch act = do
  let dir = "/tmp/throughline-test-relic"
  bracket_
    (do removePathForcibly dir; createDirectoryIfMissing True dir)
    (removePathForcibly dir)
    (act dir)

nowFixed :: UTCTime
nowFixed = UTCTime (fromGregorian 2026 4 23) 0

sampleShare :: Share
sampleShare = Share
  { shareId    = ShareId "s-1"
  , shareX     = 1
  , shareBytes = "deadbeef"
  }

sampleAttestation :: Attestation
sampleAttestation = Attestation
  { attestationShareId     = ShareId "s-1"
  , attestationOwnerPubkey = "abc123"
  , attestationSerial      = 7
  , attestationIssuedAt    = nowFixed
  , attestationSignature   = "sig-bytes"
  , attestationKeyId       = OracleKeyId "oracle-2026-q2"
  }

sampleFragment :: Fragment
sampleFragment = Fragment
  { fragmentSetId       = SetId "whisper"
  , fragmentShare       = sampleShare
  , fragmentAttestation = sampleAttestation
  }

sampleBundle :: Bundle
sampleBundle = Bundle
  { bundleId              = BundleId "whisper-001"
  , bundleSetId           = SetId "whisper"
  , bundleKind            = BundleLore
  , bundleTitle           = "The Hermit"
  , bundleBody            = Aeson.object ["paragraphs" Aeson..= ["One." :: String, "Two."]]
  , bundleCombinerPubkeys = Nothing
  , bundleCreatedAt       = nowFixed
  , bundleSignature       = "sig"
  , bundleKeyId           = OracleKeyId "oracle-2026-q2"
  }

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "Engine.Sync.Relic" $ do

  describe "JSON round-trip" $ do

    it "Fragment" $ Aeson.decode (Aeson.encode sampleFragment)
                      `shouldBe` Just sampleFragment

    it "Bundle" $ Aeson.decode (Aeson.encode sampleBundle)
                   `shouldBe` Just sampleBundle

    it "BundleKind handles the four known kinds" $ do
      mapM_ (\k -> Aeson.decode (Aeson.encode k) `shouldBe` Just k)
        [BundleLore, BundleNamedCharacter, BundleMapReveal, BundleCapability]

    it "BundleKind preserves unknown values verbatim" $ do
      let json = "\"some-future-kind\""
      (Aeson.decode json :: Maybe BundleKind)
        `shouldBe` Just (BundleUnknown "some-future-kind")

    it "ErrorCode round-trips known and unknown" $ do
      Aeson.decode (Aeson.encode ErrBadRequest) `shouldBe` Just ErrBadRequest
      (Aeson.decode "\"weird-future-code\"" :: Maybe ErrorCode)
        `shouldBe` Just (ErrUnknown "weird-future-code")

  describe "fragment store" $ do

    it "an empty directory yields an empty store" $ do
      withScratch $ \dir -> do
        m <- loadFragments dir
        Map.null m `shouldBe` True

    it "saveFragment then loadFragments yields the same fragment" $ do
      withScratch $ \dir -> do
        saveFragment dir sampleFragment
        m <- loadFragments dir
        Map.lookup (SetId "whisper") m `shouldBe` Just [sampleFragment]

    it "loadFragmentsFor narrows to a single set" $ do
      withScratch $ \dir -> do
        saveFragment dir sampleFragment
        saveFragment dir (sampleFragment
          { fragmentSetId = SetId "compass"
          , fragmentShare = sampleShare { shareId = ShareId "s-2" }
          })
        ws <- loadFragmentsFor dir (SetId "whisper")
        cs <- loadFragmentsFor dir (SetId "compass")
        length ws `shouldBe` 1
        length cs `shouldBe` 1

    it "deleteFragment removes a fragment and is a no-op when absent" $ do
      withScratch $ \dir -> do
        saveFragment dir sampleFragment
        deleteFragment dir (ShareId "s-1")
        m <- loadFragments dir
        Map.null m `shouldBe` True
        -- Second delete should not throw.
        deleteFragment dir (ShareId "s-1")

  describe "bundle store" $ do

    it "saveBundle then loadBundle round-trips" $ do
      withScratch $ \dir -> do
        saveBundle dir sampleBundle
        result <- loadBundle dir (bundleId sampleBundle)
        result `shouldBe` Just sampleBundle

    it "loadAllBundles returns every cached bundle" $ do
      withScratch $ \dir -> do
        saveBundle dir sampleBundle
        saveBundle dir (sampleBundle { bundleId = BundleId "whisper-002" })
        bs <- loadAllBundles dir
        length bs `shouldBe` 2

    it "loadBundle on a missing id returns Nothing" $ do
      withScratch $ \dir -> do
        r <- loadBundle dir (BundleId "nonexistent")
        r `shouldBe` Nothing

  describe "stubTransport" $ do

    let nothingLeftNotConfigured action =
          case action of
            Left (Left NotConfigured) -> True
            _                         -> False

    it "claim returns NotConfigured" $ do
      let req = ClaimRequest
            { claimCacheId      = CacheId "x"
            , claimPlayerPubkey = "y"
            , claimProof        = ClaimProof 0 "" 0
            , claimSignature    = "z"
            }
      r <- transportClaim stubTransport req
      nothingLeftNotConfigured r `shouldBe` True

    it "combine returns NotConfigured" $ do
      let req = CombineRequest
            { combineSetId           = SetId "whisper"
            , combineCombinerPubkeys = []
            , combineAttestations    = []
            , combineSignature       = ""
            }
      r <- transportCombine stubTransport req
      nothingLeftNotConfigured r `shouldBe` True
