module Engine.Sync.IdentitySpec (spec) where

import           Test.Hspec
import qualified Data.ByteString    as BS
import qualified Data.Map.Strict    as Map

import           Engine.Sync.Identity
import           Engine.Sync.EventLog (mkLogEntry)
import           GameTypes

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

mkIdent :: IO Identity
mkIdent = do
  -- Write to /dev/null equivalent: use a temp path that won't persist.
  -- loadOrCreate creates the file only if it doesn't exist, so using a
  -- non-existent temp path forces creation each time.
  let tmpDir = "/tmp/throughline-test-identity"
  loadOrCreate (tmpDir <> "/test.key")

testEntry :: PlayerId -> LogEntry
testEntry pid =
  mkLogEntry pid (LamportClock 1 pid) (ActionId "test-action") (WorldDiff [] [] [] [] [] [] []) Map.empty

spec :: Spec
spec = describe "Engine.Sync.Identity" $ do

  -- -------------------------------------------------------------------------
  -- playerIdOf
  -- -------------------------------------------------------------------------

  describe "playerIdOf" $
    it "produces a 64-character hex string" $ do
      ident <- mkIdent
      let PlayerId s = playerIdOf ident
      length s `shouldBe` 64

  -- -------------------------------------------------------------------------
  -- signEntry / verifyEntry
  -- -------------------------------------------------------------------------

  describe "signEntry + verifyEntry" $ do
    it "a signed entry verifies successfully" $ do
      ident <- mkIdent
      let pid = playerIdOf ident
      let entry = testEntry pid
      let signed = signEntry ident entry
      verifyEntry signed `shouldBe` True

    it "an unsigned entry always passes verification" $ do
      let entry = testEntry (PlayerId "test")
      entrySignature entry `shouldBe` Nothing
      verifyEntry entry `shouldBe` True

    it "a non-key PlayerId passes verification even when signed" $
      -- PlayerId "test" is too short to be a valid public key; verifyEntry
      -- treats it as legacy and passes through without checking.
      do
        ident <- mkIdent
        let entry = testEntry (PlayerId "test")
        let signed = signEntry ident entry { entryPlayerId = PlayerId "test" }
        verifyEntry signed `shouldBe` True

    it "a tampered signature fails verification" $ do
      ident <- mkIdent
      let pid = playerIdOf ident
      let entry = testEntry pid
      let signed    = signEntry ident entry
          tampered  = signed { entrySignature = Just (BS.replicate 64 0) }
      verifyEntry tampered `shouldBe` False

    it "modifying the diff after signing fails verification" $ do
      ident <- mkIdent
      let pid = playerIdOf ident
      let entry = testEntry pid
      let signed   = signEntry ident entry
          modified = signed { entryDiff = WorldDiff [] [] [] []
                                [ScenarioTag (MkScenarioTag "injected")] [] [] }
      verifyEntry modified `shouldBe` False

    it "modifying the actionId after signing fails verification" $ do
      ident <- mkIdent
      let pid = playerIdOf ident
      let entry = testEntry pid
      let signed   = signEntry ident entry
          modified = signed { entryActionId = ActionId "tampered-action" }
      verifyEntry modified `shouldBe` False
