module SDL.AudioBackendSpec (spec) where

import           Data.IORef      (modifyIORef', newIORef, readIORef)

import           Test.Hspec

import           SDL.AudioBackend

spec :: Spec
spec = describe "SDL.AudioBackend" $ do

  it "silentBackend is a total no-op" $ do
    abPlay silentBackend UiSelect
    abPlay silentBackend (Gameplay "anything")
    abStopAll silentBackend
    abSetGains silentBackend 1 1 1
    abShutdown silentBackend
    -- Reaching this line means none of the calls crashed or threw;
    -- there's nothing state-ful to assert on.
    True `shouldBe` True

  -- A hand-rolled recording backend demonstrates the shape of what
  -- the real SDL2_mixer implementation will look like (pure record
  -- construction) and lets us test that 'playEvent' dispatches
  -- through the record field rather than a hardcoded default.
  it "playEvent dispatches via the backend's abPlay" $ do
    ref <- newIORef ([] :: [AudioEvent])
    let recording = silentBackend { abPlay = \e -> modifyIORef' ref (e :) }
    playEvent recording UiSelect
    playEvent recording (Gameplay "shot")
    events <- readIORef ref
    -- Newest-first because we cons'd.
    events `shouldBe` [Gameplay "shot", UiSelect]
