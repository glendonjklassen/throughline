module SDL.CrashHandlerSpec (spec) where

import           Control.Exception (SomeException, toException, ErrorCall(..))
import           Data.List         (isInfixOf, isPrefixOf)
import           System.Directory  (doesFileExist, removeFile)
import           System.FilePath   (takeFileName)

import           Test.Hspec

import           SDL.CrashHandler  (writeCrashReport, crashReportDir)

spec :: Spec
spec = describe "SDL.CrashHandler" $ do

  it "writes a crash report file that includes the exception text" $ do
    let e :: SomeException
        e = toException (ErrorCall "boom: deliberately thrown for the test")
    path <- writeCrashReport e
    -- The returned path lives under the expected directory and starts
    -- with the timestamped 'crash-' prefix — the basename is what the
    -- UI shows to a reporting player.
    path `shouldSatisfy` (crashReportDir `isPrefixOf`)
    takeFileName path `shouldSatisfy` ("crash-" `isPrefixOf`)

    exists <- doesFileExist path
    exists `shouldBe` True

    body <- readFile path
    body `shouldSatisfy` ("throughline crash report" `isInfixOf`)
    body `shouldSatisfy` ("boom: deliberately thrown" `isInfixOf`)

    removeFile path
