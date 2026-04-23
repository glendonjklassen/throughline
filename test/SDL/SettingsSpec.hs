{-# LANGUAGE OverloadedStrings #-}
module SDL.SettingsSpec (spec) where

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as BL

import           Test.Hspec

import           SDL.Settings

spec :: Spec
spec = describe "SDL.Settings" $ do

  it "round-trips defaults through JSON" $ do
    let raw = Aeson.encode defaultSettings
    Aeson.decode raw `shouldBe` Just defaultSettings

  it "round-trips Fullscreen mode" $ do
    let s = defaultSettings { sDisplayMode = Fullscreen }
    Aeson.decode (Aeson.encode s) `shouldBe` Just s

  -- Shipped builds will grow new fields over time.  Loading an older
  -- config that omits some fields must still produce a valid Settings:
  -- each missing field falls back to its default.  This guarantees a
  -- player who played version N keeps their v(N-1) customizations
  -- after an update that added new options.
  it "fills in missing fields from defaults" $ do
    let partial = BL.fromStrict "{\"fontScale\":1.5,\"highContrast\":true}"
    Aeson.decode partial `shouldBe`
      Just defaultSettings { sFontScale = 1.5, sHighContrast = True }

  it "rejects a bogus DisplayMode value" $ do
    let bad = BL.fromStrict "{\"displayMode\":\"hologram\"}"
    Aeson.decode bad `shouldBe` (Nothing :: Maybe Settings)
