module Terminal.Display where

import           Data.List                (intercalate)
import           Text.Read               (readMaybe)
import           Engine.Core.NarrativeMessage
import           Terminal.Layout
import           Terminal.ANSI

-- | Pick a separator character based on the world clock tick.
-- Cycles through thin vertical line variants for ambient texture.
separatorFor :: Int -> Char
separatorFor tick = separatorChars !! (tick `mod` length separatorChars)
  where
    separatorChars :: [Char]
    separatorChars = ['│', '┊', '╎', '┆']

-- | Render one row of the split layout: left panel, separator, right panel.
renderSplitRow :: Int -> Char -> (String, String) -> String
renderSplitRow leftW sep (l, r) = fitToWidth leftW l <> dim [' ', sep, ' '] <> r

-- | Pick the narration colour based on the current tension level (0–10).
-- Low tension is normal green; as tension rises the colour shifts toward
-- yellow-green, yellow, dark-yellow, and finally dim.
tensionNarrationColor :: Int -> (String -> String)
tensionNarrationColor t
  | t <= 2    = green
  | t <= 4    = \s -> "\ESC[38;5;148m" <> s <> "\ESC[0m"
  | t <= 6    = yellow
  | t <= 8    = \s -> "\ESC[38;5;172m" <> s <> "\ESC[0m"
  | otherwise = dim

-- | Format the message log for the right panel.
-- Each NarrativeEntry carries a structured message, tension level, and time
-- label.  The message type determines colouring directly — no prefix sniffing.
-- Returns lines ready to print.
buildHistoryLines :: Int -> [NarrativeEntry] -> [String]
buildHistoryLines rightW msgs = buildHistoryLinesWith rightW labelW msgs
  where labelW = maximum (0 : map (length . neTimeLabel) msgs)

-- | Like buildHistoryLines but with an explicit label column width,
-- so the typewriter can match the full history's alignment.
buildHistoryLinesWith :: Int -> Int -> [NarrativeEntry] -> [String]
buildHistoryLinesWith rightW maxLabelW = concatMap fmt
  where
    contentW  = max 10 (rightW - maxLabelW - 2)

    fmt entry =
      let tension  = neTension entry
          label    = neTimeLabel entry
          color    = colorForMessage tension (neMessage entry)
          lines_   = messageLines (neMessage entry)
          pad      = replicate (maxLabelW + 2) ' '
          labelPad = padRight (maxLabelW + 2) label
      in case lines_ of
           []     -> []
           (l:ls) ->
             (dim labelPad <> color l) :
             map (\r -> dim pad <> color r) ls

    -- | Pick the colouring function based on the message type.
    colorForMessage :: Int -> NarrativeMessage -> (String -> String)
    colorForMessage _ MsgSay {}         = cyan
    colorForMessage _ (MsgThink _ _)   = dim
    colorForMessage t (MsgNarrate _)   = tensionNarrationColor t
    colorForMessage t (MsgEffect _)    = tensionNarrationColor t
    colorForMessage _ (MsgDialogue _)  = cyan

    -- | Extract display lines from a message, word-wrapped to contentW.
    messageLines :: NarrativeMessage -> [String]
    messageLines (MsgSay _ sName _ lNames text) =
      wrapWords contentW (sName <> fmtListeners lNames <> ": " <> text)
    messageLines (MsgThink _ text)     = wrapWords contentW ("~ " <> text)
    messageLines (MsgNarrate text)     = wrapWords contentW ("> " <> text)
    messageLines (MsgEffect text)      = wrapWords contentW ("  " <> text)
    messageLines (MsgDialogue dls)     = concatMap fmtLine dls
      where fmtLine (_, sName, _, lNames, text) =
              wrapWords contentW (sName <> fmtListeners lNames <> ": " <> text)

    fmtListeners [] = ""
    fmtListeners ns = " (to " <> intercalate ", " ns <> ")"

takeLast :: Int -> [a] -> [a]
takeLast n xs = drop (max 0 (length xs - n)) xs

-- | Map a single character to a 1-based index into the list.
-- Returns Nothing for non-digit input, '0', or indices beyond the list length.
safeIndex :: Char -> [x] -> Maybe x
safeIndex c as =
  case readMaybe [c] of
    Just i | i >= 1, i <= length as -> Just (as !! (i - 1))
    _                               -> Nothing

-- | Assemble the status-line block from engine and scenario status strings,
-- with an optional compass line showing exit bearings.
-- Returns 1 line when both are absent, 3 lines for one, 4 lines for both.
-- The compass line (if non-empty) is appended after the status lines.
buildStatusPart :: Maybe String -> Maybe String -> Maybe String -> [String]
buildStatusPart Nothing  Nothing  _          = [""]
buildStatusPart (Just e) Nothing  mCompass   = ["", dim "[ " <> bold e <> dim " ]"]
                                               ++ compassLine mCompass ++ [""]
buildStatusPart Nothing  (Just s) _          = ["", dim "[ " <> bold s <> dim " ]", ""]
buildStatusPart (Just e) (Just s) mCompass   = ["", dim "[ " <> bold e <> dim " ]"]
                                               ++ compassLine mCompass
                                               ++ [dim "[ " <> bold s <> dim " ]", ""]

-- | Render the compass line if exit directions are available.
compassLine :: Maybe String -> [String]
compassLine Nothing  = []
compassLine (Just c) = [dim "[  " <> c <> dim "  ]"]

-- | Build the compass display string from a list of exit cardinal directions.
-- Returns Nothing if the list is empty.
buildCompassString :: [(String, Double)] -> Maybe String
buildCompassString [] = Nothing
buildCompassString exits =
  let sorted = sortExits exits
  in Just (intercalate (dim " · ") (map bold sorted))

-- | Sort exit labels clockwise from N.
sortExits :: [(String, Double)] -> [String]
sortExits = map fst . sortByBearing
  where
    sortByBearing = map snd . sort . map (\(l, b) -> (b, (l, b)))
    sort = foldr insert []
    insert x [] = [x]
    insert x (y:ys)
      | fst x <= fst y = x : y : ys
      | otherwise       = y : insert x ys

-- | Compute left and right panel widths from layout config and terminal width.
computePanelWidths :: LayoutConfig -> Int -> (Int, Int)
computePanelWidths layout termW =
  let leftW  = min (layoutLeftMaxWidth layout) (termW * layoutLeftPercent layout `div` 100)
      rightW = max (layoutRightMinWidth layout) (termW - leftW - 3)
  in (leftW, rightW)
