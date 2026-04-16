module Terminal.DisplaySpec (spec) where

import           Data.List  (isInfixOf)
import           Test.Hspec

import           Engine.Core.NarrativeMessage (NarrativeEntry (..), NarrativeMessage (..))
import           GameTypes (CharId (..))
import           Terminal.ANSI    (stripAnsi)
import           Terminal.Display
import           Terminal.Layout (LayoutConfig (..), defaultLayout)

spec :: Spec
spec = describe "Terminal.Display" $ do

  -- -------------------------------------------------------------------------
  -- takeLast
  -- -------------------------------------------------------------------------

  describe "takeLast" $ do
    it "returns empty for empty input" $
      takeLast 5 ([] :: [Int]) `shouldBe` []
    it "returns all when n >= length" $
      takeLast 10 [1,2,3 :: Int] `shouldBe` [1,2,3]
    it "returns last n elements" $
      takeLast 2 [1,2,3,4,5 :: Int] `shouldBe` [4,5]
    it "returns empty when n == 0" $
      takeLast 0 [1,2,3 :: Int] `shouldBe` []

  -- -------------------------------------------------------------------------
  -- safeIndex
  -- -------------------------------------------------------------------------

  describe "safeIndex" $ do
    let xs = ["a", "b", "c"] :: [String]
    it "maps '1' to the first element" $
      safeIndex '1' xs `shouldBe` Just "a"
    it "maps '3' to the last element" $
      safeIndex '3' xs `shouldBe` Just "c"
    it "returns Nothing for '0'" $
      safeIndex '0' xs `shouldBe` Nothing
    it "returns Nothing when index exceeds list length" $
      safeIndex '4' xs `shouldBe` Nothing
    it "returns Nothing for non-digit input" $
      safeIndex 'q' xs `shouldBe` Nothing
    it "returns Nothing for empty list" $
      safeIndex '1' ([] :: [String]) `shouldBe` Nothing

  -- -------------------------------------------------------------------------
  -- buildStatusPart
  -- -------------------------------------------------------------------------

  describe "buildStatusPart" $ do
    it "both Nothing: single blank line" $
      buildStatusPart Nothing Nothing Nothing `shouldBe` [""]
    it "engine only: 3 lines" $
      length (buildStatusPart (Just "engine") Nothing Nothing) `shouldBe` 3
    it "scenario only: 3 lines" $
      length (buildStatusPart Nothing (Just "scene") Nothing) `shouldBe` 3
    it "both present: 4 lines" $
      length (buildStatusPart (Just "engine") (Just "scene") Nothing) `shouldBe` 4
    it "engine text appears in output" $
      any (("engine" `isInfixOf`) . stripAnsi) (buildStatusPart (Just "engine") Nothing Nothing)
        `shouldBe` True
    it "scenario text appears in output" $
      any (("scene" `isInfixOf`) . stripAnsi) (buildStatusPart Nothing (Just "scene") Nothing)
        `shouldBe` True
    it "both texts appear when both present" $
      let ls = map stripAnsi (buildStatusPart (Just "eng") (Just "scn") Nothing)
      in (any ("eng" `isInfixOf`) ls, any ("scn" `isInfixOf`) ls)
           `shouldBe` (True, True)

  -- -------------------------------------------------------------------------
  -- computePanelWidths
  -- -------------------------------------------------------------------------

  describe "computePanelWidths" $ do
    it "leftW does not exceed layoutLeftMaxWidth" $ do
      let (leftW, _) = computePanelWidths defaultLayout 200
      leftW `shouldSatisfy` (<= layoutLeftMaxWidth defaultLayout)
    it "rightW is at least layoutRightMinWidth" $ do
      let (_, rightW) = computePanelWidths defaultLayout 200
      rightW `shouldSatisfy` (>= layoutRightMinWidth defaultLayout)
    it "respects leftPercent for normal terminal widths" $ do
      let layout = defaultLayout { layoutLeftPercent = 50, layoutLeftMaxWidth = 1000 }
          (leftW, _) = computePanelWidths layout 100
      leftW `shouldBe` 50
    it "caps leftW at layoutLeftMaxWidth" $ do
      let layout = defaultLayout { layoutLeftMaxWidth = 30, layoutLeftPercent = 90 }
          (leftW, _) = computePanelWidths layout 200
      leftW `shouldBe` 30
    it "rightW accounts for separator (3 columns)" $ do
      let layout = defaultLayout { layoutLeftMaxWidth = 1000, layoutLeftPercent = 50
                                 , layoutRightMinWidth = 0 }
          (leftW, rightW) = computePanelWidths layout 100
      leftW + rightW + 3 `shouldBe` 100

  -- -------------------------------------------------------------------------
  -- renderSplitRow
  -- -------------------------------------------------------------------------

  describe "renderSplitRow" $ do
    it "exact-width left panel: separator immediately follows" $
      stripAnsi (renderSplitRow 5 '│' ("hello", "world"))
        `shouldBe` "hello │ world"
    it "short left panel is padded to leftW before separator" $
      stripAnsi (renderSplitRow 5 '│' ("hi", "world"))
        `shouldBe` "hi    │ world"
    it "right panel content is preserved unchanged" $
      let row = stripAnsi (renderSplitRow 5 '│' ("hello", "right side text"))
      in drop (5 + 3) row `shouldBe` "right side text"

  -- -------------------------------------------------------------------------
  -- separatorFor
  -- -------------------------------------------------------------------------

  describe "separatorFor" $ do
    it "tick 0 gives '│'" $
      separatorFor 0 `shouldBe` '│'
    it "tick 1 gives '┊'" $
      separatorFor 1 `shouldBe` '┊'
    it "tick 2 gives '╎'" $
      separatorFor 2 `shouldBe` '╎'
    it "tick 3 gives '┆'" $
      separatorFor 3 `shouldBe` '┆'
    it "tick 4 wraps back to '│'" $
      separatorFor 4 `shouldBe` '│'
    it "tick 7 wraps to '┆'" $
      separatorFor 7 `shouldBe` '┆'

  -- -------------------------------------------------------------------------
  -- tensionNarrationColor
  -- -------------------------------------------------------------------------

  describe "tensionNarrationColor" $ do
    let testText = "some narration"
    it "preserves text content at any tension level" $ do
      stripAnsi (tensionNarrationColor 0 testText) `shouldBe` testText
      stripAnsi (tensionNarrationColor 5 testText) `shouldBe` testText
      stripAnsi (tensionNarrationColor 9 testText) `shouldBe` testText

    it "tension 0 produces green ANSI codes" $
      tensionNarrationColor 0 testText `shouldSatisfy` ("\ESC[32m" `isInfixOf`)

    it "tension 5 produces yellow ANSI codes" $
      tensionNarrationColor 5 testText `shouldSatisfy` ("\ESC[33m" `isInfixOf`)

    it "tension 9 produces dim ANSI codes" $
      tensionNarrationColor 9 testText `shouldSatisfy` ("\ESC[2m" `isInfixOf`)

    it "different tension levels produce different raw output" $ do
      let low  = tensionNarrationColor 0 testText
          mid  = tensionNarrationColor 5 testText
          high = tensionNarrationColor 9 testText
      low `shouldNotBe` mid
      mid `shouldNotBe` high
      low `shouldNotBe` high

  -- -------------------------------------------------------------------------
  -- buildHistoryLines
  -- -------------------------------------------------------------------------

  describe "buildHistoryLines" $ do
    let width = 40
    it "empty list produces empty result" $
      buildHistoryLines width [] `shouldBe` []

    it "single MsgNarrate entry produces wrapped coloured lines" $ do
      let entry = NarrativeEntry
            { neMessage   = MsgNarrate "The wind howls."
            , neTension   = 0
            , neTimeLabel = "8am"
            }
          result = buildHistoryLines width [entry]
      result `shouldNotBe` []
      any (("The wind howls." `isInfixOf`) . stripAnsi) result `shouldBe` True

    it "mixed message types are each formatted differently" $ do
      let sayEntry = NarrativeEntry
            { neMessage   = MsgSay (Named "alice") "Alice" [] [] "Hello there"
            , neTension   = 0
            , neTimeLabel = "9am"
            }
          thinkEntry = NarrativeEntry
            { neMessage   = MsgThink (Named "bob") "I wonder..."
            , neTension   = 0
            , neTimeLabel = "9am"
            }
          narrateEntry = NarrativeEntry
            { neMessage   = MsgNarrate "A door creaks."
            , neTension   = 3
            , neTimeLabel = "9am"
            }
          result = map stripAnsi (buildHistoryLines width [sayEntry, thinkEntry, narrateEntry])
      -- MsgSay produces "Name: text"
      any ("Alice: Hello there" `isInfixOf`) result `shouldBe` True
      -- MsgThink produces "~ text"
      any ("~ I wonder..." `isInfixOf`) result `shouldBe` True
      -- MsgNarrate produces "> text"
      any ("> A door creaks." `isInfixOf`) result `shouldBe` True

    it "time labels appear in the output" $ do
      let entry = NarrativeEntry
            { neMessage   = MsgNarrate "Hello."
            , neTension   = 0
            , neTimeLabel = "noon"
            }
          result = map stripAnsi (buildHistoryLines width [entry])
      any ("noon" `isInfixOf`) result `shouldBe` True
