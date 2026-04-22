-- | Standalone executable for a Deer Hunt-only Steam bundle.  Links
-- just the DeerHunt scenario; the launcher sees a single entry and
-- renders a title screen with Continue / New hunt instead of the
-- dev picker.
module Main where

import           SDL.Launcher            (ScenarioEntry(..), runLauncher)

import           Scenarios.DeerHunt      (deerHunt, deerHuntDisplay)
import           Scenarios.DeerHunt.Help (deerHuntHelp)

main :: IO ()
main = runLauncher
  [ ScenarioEntry "Deer Hunt"
      "Mid-November. Southern Manitoba. One square mile. One buck."
      deerHuntDisplay deerHunt (Just deerHuntHelp)
  ]
