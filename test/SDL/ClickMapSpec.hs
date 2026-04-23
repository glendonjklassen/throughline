module SDL.ClickMapSpec (spec) where

import           Test.Hspec

import           SDL.ClickMap

spec :: Spec
spec = describe "SDL.ClickMap" $ do

  it "returns Nothing when no rect contains the point" $ do
    let cm = [(10, 10, 20, 20, 'a')]
    hitTest cm 0  0  `shouldBe` Nothing
    hitTest cm 5  15 `shouldBe` Nothing
    hitTest cm 35 15 `shouldBe` Nothing

  it "treats the rect as half-open: left/top inclusive, right/bottom exclusive" $ do
    let cm = [(10, 10, 20, 20, 'a')]
    -- Inside
    hitTest cm 10 10 `shouldBe` Just 'a'
    hitTest cm 29 29 `shouldBe` Just 'a'
    -- Exactly on the right / bottom edge is NOT a hit (prevents a
    -- click landing on the seam between two rects from picking the
    -- wrong one).
    hitTest cm 30 20 `shouldBe` Nothing
    hitTest cm 20 30 `shouldBe` Nothing

  it "first matching rect wins on overlap" $ do
    let cm = [(0, 0, 20, 20, 'a'), (10, 10, 20, 20, 'b')]
    hitTest cm 15 15 `shouldBe` Just 'a'

  it "distinguishes non-overlapping rects" $ do
    let cm =
          [ (0,  0,  50, 20, 'x')
          , (0,  20, 50, 20, 'y')
          , (0,  40, 50, 20, 'z')
          ]
    hitTest cm 25 5  `shouldBe` Just 'x'
    hitTest cm 25 25 `shouldBe` Just 'y'
    hitTest cm 25 45 `shouldBe` Just 'z'
