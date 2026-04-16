module SDL.TextSpec (spec) where

import           Test.Hspec
import           SDL.Text

spec :: Spec
spec = describe "SDL.Text" $ do

  -- -------------------------------------------------------------------------
  -- Colour wrappers (yellow, red — uncovered by other modules)
  -- -------------------------------------------------------------------------

  describe "colour wrappers" $ do
    it "yellow wraps and strips cleanly" $
      stripAnsi (yellow "warning") `shouldBe` "warning"
    it "red wraps and strips cleanly" $
      stripAnsi (red "error") `shouldBe` "error"

  -- -------------------------------------------------------------------------
  -- visibleLength
  -- -------------------------------------------------------------------------

  describe "visibleLength" $ do
    it "plain string" $
      visibleLength "hello" `shouldBe` 5
    it "ANSI-wrapped string counts only visible chars" $
      visibleLength (bold "hi") `shouldBe` 2
    it "empty string" $
      visibleLength "" `shouldBe` 0

  -- -------------------------------------------------------------------------
  -- wrapWords
  -- -------------------------------------------------------------------------

  describe "wrapWords" $ do
    it "empty input returns empty list" $
      wrapWords 10 "" `shouldBe` []

    it "single word shorter than width" $
      wrapWords 10 "hello" `shouldBe` ["hello"]

    it "single word longer than width is not split" $
      wrapWords 3 "hello" `shouldBe` ["hello"]

    it "two words that fit on one line" $
      wrapWords 20 "hello world" `shouldBe` ["hello world"]

    it "two words that do not fit: split at boundary" $
      wrapWords 10 "hello world" `shouldBe` ["hello", "world"]

    it "wraps long text across multiple lines" $
      wrapWords 10 "one two three four five"
        `shouldBe` ["one two", "three four", "five"]

    it "exact-fit line is not broken" $
      -- "hello" (5) + " " (1) + "world" (5) = 11, width = 11 → fits
      wrapWords 11 "hello world" `shouldBe` ["hello world"]

  -- -------------------------------------------------------------------------
  -- stripAnsi (direct tests)
  -- -------------------------------------------------------------------------

  describe "stripAnsi" $ do
    it "plain text is unchanged" $
      stripAnsi "hello world" `shouldBe` "hello world"
    it "single ANSI code is stripped" $
      stripAnsi "\ESC[32mhello\ESC[0m" `shouldBe` "hello"
    it "nested ANSI codes are all stripped" $
      stripAnsi "\ESC[1m\ESC[32mhello\ESC[0m\ESC[0m" `shouldBe` "hello"
    it "empty string gives empty" $
      stripAnsi "" `shouldBe` ""
    it "multiple ANSI-wrapped segments are stripped" $
      stripAnsi "\ESC[31mred\ESC[0m and \ESC[32mgreen\ESC[0m"
        `shouldBe` "red and green"

  -- -------------------------------------------------------------------------
  -- fitToWidth
  -- -------------------------------------------------------------------------

  describe "fitToWidth" $ do
    it "short string is padded to width" $ do
      let result = fitToWidth 10 "hi"
      visibleLength result `shouldBe` 10
      stripAnsi result `shouldBe` "hi        "
    it "long string is truncated to width" $ do
      let result = fitToWidth 5 "hello world"
      visibleLength result `shouldBe` 5
      stripAnsi result `shouldBe` "hello"
    it "exact-width string is unchanged modulo padding" $ do
      let result = fitToWidth 5 "hello"
      visibleLength result `shouldBe` 5
      stripAnsi result `shouldBe` "hello"
    it "ANSI-wrapped string is measured by visible length" $ do
      let result = fitToWidth 10 (bold "hi")
      visibleLength result `shouldBe` 10

  -- -------------------------------------------------------------------------
  -- padRight
  -- -------------------------------------------------------------------------

  describe "padRight" $ do
    it "plain string is padded correctly" $ do
      let result = padRight 10 "abc"
      result `shouldBe` "abc       "
      length result `shouldBe` 10
    it "ANSI string is padded by visible length" $ do
      let input  = green "abc"
          result = padRight 10 input
      visibleLength result `shouldBe` 10
      stripAnsi result `shouldBe` "abc       "
    it "string at target width gets no padding" $
      padRight 5 "hello" `shouldBe` "hello"
    it "string longer than target gets no padding" $
      padRight 3 "hello" `shouldBe` "hello"

  -- -------------------------------------------------------------------------
  -- Colour wrappers (grey, green, cyan, bold, dim)
  -- -------------------------------------------------------------------------

  describe "remaining colour wrappers" $ do
    let wrappers = [ ("grey",  grey)
                   , ("green", green)
                   , ("cyan",  cyan)
                   , ("bold",  bold)
                   , ("dim",   dim)
                   ]
        testText = "sample"
    mapM_ (\(name, fn) -> do
      it (name <> " output contains original text") $
        stripAnsi (fn testText) `shouldBe` testText
      it (name <> " output is longer than input") $
        length (fn testText) `shouldSatisfy` (> length testText)
      ) wrappers
