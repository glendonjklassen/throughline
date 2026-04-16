module Terminal.RenderSpec (spec) where

import Test.Hspec
import Terminal.Render (glitchIntensity)

spec :: Spec
spec = describe "Terminal.Render" $ do
  describe "glitchIntensity" $ do
    it "returns Nothing below tension 4" $ do
      glitchIntensity 0 `shouldBe` Nothing
      glitchIntensity 3 `shouldBe` Nothing

    it "returns light glitch at tension 4-5" $ do
      glitchIntensity 4 `shouldBe` Just (1, 3)
      glitchIntensity 5 `shouldBe` Just (1, 3)

    it "returns moderate glitch at tension 6-7" $ do
      glitchIntensity 6 `shouldBe` Just (3, 6)
      glitchIntensity 7 `shouldBe` Just (3, 6)

    it "returns heavy glitch at tension 8+" $ do
      glitchIntensity 8  `shouldBe` Just (5, 10)
      glitchIntensity 10 `shouldBe` Just (5, 10)
