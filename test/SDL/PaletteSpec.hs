module SDL.PaletteSpec (spec) where

import           Test.Hspec

import           SDL.Palette

spec :: Spec
spec = describe "SDL.Palette" $ do

  describe "remapColor" $ do

    it "Autumn mode is identity" $ do
      remapColor Autumn textColor `shouldBe` textColor
      remapColor Autumn dimTextColor     `shouldBe` dimTextColor
      remapColor Autumn chromeColor    `shouldBe` chromeColor

    -- HighContrast should brighten every opaque foreground; a
    -- regression here would quietly make the accessibility mode
    -- render no differently from the default palette.
    it "HighContrast brightens opaque foregrounds" $ do
      let Color r  g  b  _ = textColor
          Color r' g' b' _ = remapColor HighContrast textColor
      (r' >= r && g' >= g && b' >= b) `shouldBe` True
      -- At least one channel actually moved — otherwise we didn't
      -- lift anything.
      (r' > r || g' > g || b' > b) `shouldBe` True

    it "HighContrast lifts very ansiDim foregrounds to a visible ansiGrey" $ do
      -- separatorColor is near-black; remapped it should be bright
      -- enough to read.
      let Color r g b _ = remapColor HighContrast separatorColor
      all (>= 150) [r, g, b] `shouldBe` True

    it "HighContrast leaves low-alpha overlays alone" $ do
      -- Zone tints rely on ~30-40 alpha to read as halos; remapping
      -- them would turn them into solid blocks.
      let halo = Color 196 148 58 38
      remapColor HighContrast halo `shouldBe` halo

  describe "remapBgColor" $ do

    it "Autumn returns the canonical bgColor" $
      remapBgColor Autumn `shouldBe` bgColor

    it "HighContrast darkens the background further" $ do
      let Color bR bG bB _ = bgColor
          Color r  g  b  _ = remapBgColor HighContrast
      (r <= bR && g <= bG && b <= bB) `shouldBe` True
