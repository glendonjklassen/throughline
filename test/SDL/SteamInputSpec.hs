module SDL.SteamInputSpec (spec) where

import qualified Data.Map.Strict as Map
import           Data.List       (nub)

import           Test.Hspec

import           SDL.SteamInput

spec :: Spec
spec = describe "SDL.SteamInput" $ do

  it "exposes every action in both directions" $ do
    length allSteamActions `shouldSatisfy` (> 0)
    mapM_ (\a -> steamActionToChar a `shouldNotBe` Nothing) allSteamActions

  -- Two Steam actions must not collide on the same internal character,
  -- otherwise the runtime can't tell them apart once the FFI layer
  -- feeds 'steamActionToChar' into the shared input handler.
  it "maps every action to a distinct character" $ do
    let chars = Map.elems steamActionMap
    length (nub chars) `shouldBe` length chars

  it "returns Nothing for an unbound action name" $
    steamActionToChar "this-action-does-not-exist" `shouldBe` Nothing

  -- Stability check: the canonical journal key ('1') must stay bound
  -- to the same Steam action across releases — if we ever rename it,
  -- published Action Sets in Steam will silently stop opening the
  -- journal for players who haven't re-synced.  Better to fail a
  -- test than ship that.
  it "keeps the journal action bound to key '1'" $
    steamActionToChar "journal" `shouldBe` Just '1'
