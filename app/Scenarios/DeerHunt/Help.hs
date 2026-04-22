-- | Player-facing how-to-play copy for the Deer Hunt scenario.
-- Kept separate from the scenario body because it's UI text read by
-- the launcher before any world exists; it has no runtime dependency
-- on engine types.
module Scenarios.DeerHunt.Help (deerHuntHelp) where

-- | Lines shown on the Deer Hunt help overlay, one per screen row.
-- Empty strings are paragraph breaks.  Deliberately short: the game
-- rewards paying attention to the prose, so the help page should
-- orient rather than instruct.
deerHuntHelp :: [String]
deerHuntHelp =
  [ "Mid-November. You have a tag to fill."
  , ""
  , "Movement keys are on the top row — Q, W, E, R, T,"
  , "Y, U, I, O, P — and correspond to directions shown"
  , "on the spatial HUD.  The home row — A, S, D, F,"
  , "G, H, J, K, L — is for everything else: sitting,"
  , "glassing, shooting, heading back to the truck."
  , ""
  , "Press 1 to open your journal.  It holds today's"
  , "beats, past days, and the field catalog of what"
  , "you've seen.  1 again to close."
  , ""
  , "You autosave after every action.  Close the window"
  , "whenever; the hunt will be there when you return."
  ]
