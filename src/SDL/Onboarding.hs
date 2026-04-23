-- | How-to-play overlay for the launcher and scenario intros.
--
-- Content is a flat list of strings per scenario, kept here rather than
-- in a scenario module because this screen lives outside the runtime
-- loop — no engine primitives, no world state.  A scenario can ship
-- its own pages via 'ScenarioEntry'; if none is provided, the generic
-- 'defaultHowToPlay' is shown.
--
-- Pagination is implicit: the renderer just wraps and scrolls within
-- the grid.  The overlay dismisses on any key.
module SDL.Onboarding
  ( defaultHowToPlay
  , renderHowToPlay
  , howToPlayLoop
  ) where

import           SDL.FontContext  (renderText)
import           SDL.InputHandler (awaitInputSDL)
import           SDL.Palette      (defaultText, dimText, greyText)
import           SDL.Renderer     (SDLContext(..), clearSDL, presentSDL)

-- | The baseline how-to-play text used when a 'ScenarioEntry' doesn't
-- carry its own.  Intentionally short: scenario-specific pages (deer
-- hunt keys, journal hotkeys, etc.) are the preferred customization.
defaultHowToPlay :: [String]
defaultHowToPlay =
  [ "You act by choosing a letter."
  , ""
  , "The main row (QWERTY) is for moving through the"
  , "world; the home row (ASDF…) is for everything else."
  , "Press the number beside an option to take it."
  , ""
  , "Press 1 at any time during a scenario to open your"
  , "journal.  Press 1 again to close it."
  , ""
  , "Your progress is autosaved after every action.  You"
  , "can safely close the window and pick up where you"
  , "left off from the title screen."
  ]

-- | Render the how-to-play page and wait for any input to dismiss.
-- A keypress or a click (touch) anywhere closes the overlay.
howToPlayLoop :: SDLContext -> String -> [String] -> IO ()
howToPlayLoop ctx title pages = do
  renderHowToPlay ctx title pages
  _ <- awaitInputSDL (sdlWindow ctx)
  pure ()

renderHowToPlay :: SDLContext -> String -> [String] -> IO ()
renderHowToPlay ctx title pages = do
  clearSDL ctx
  let fc = sdlFont ctx
  renderText fc ("— " <> title <> " —") defaultText (3, 2)
  renderText fc ""                      dimText     (3, 3)
  mapM_ (renderLine fc) (zip [0 :: Int ..] pages)
  renderText fc "press any key or click to return" greyText
    (3, fromIntegral (5 + length pages + 1))
  presentSDL ctx
  where
    renderLine fc (i, line) =
      renderText fc line defaultText (3, fromIntegral (5 + i))
