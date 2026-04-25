-- | Standard help-overlay text for scenarios.  The engine owns the
-- keymap and the autosave story; scenarios prepend their own framing
-- prose ("you are a hunter", "you are a clerk", etc.) and let
-- 'helpScreen' append the standard controls block.
module Engine.Author.Help
  ( standardControlsHelp
  , helpScreen
  ) where

-- | Engine-owned controls block.  Empty strings are paragraph
-- breaks.  Lines short enough for a narrow help overlay.
standardControlsHelp :: [String]
standardControlsHelp =
  [ "Movement keys are on the top row \x2014 Q, W, E, R, T,"
  , "Y, U, I, O, P \x2014 and correspond to directions shown"
  , "on the spatial HUD.  The home row \x2014 A, S, D, F,"
  , "G, H, J, K, L \x2014 is for everything else."
  , ""
  , "Press 1 to open your journal.  It holds today's"
  , "beats, past days, and the field catalog of what"
  , "you've seen.  1 again to close."
  , ""
  , "You autosave after every action.  Close the window"
  , "whenever; the scenario will be there when you return."
  ]

-- | Compose a help screen: scenario framing lines, a paragraph break,
-- then the standard controls block.
helpScreen :: [String] -> [String]
helpScreen scenarioLines = scenarioLines <> [""] <> standardControlsHelp
