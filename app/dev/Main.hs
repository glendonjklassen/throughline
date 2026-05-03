-- | Multi-scenario dev launcher.  Ships every scenario the project
-- currently has.  For Steam bundles, a single-scenario executable like
-- 'DeerHuntMain' is the right entry point.
module Main where

import           SDL.Launcher             (ScenarioEntry(..), runLauncher)
import           SDL.Layout               (defaultDisplay)

import           Scenarios.DeerHunt       (deerHunt, deerHuntDisplay,
                                           deerHuntName, deerHuntOnEnd)
import           Scenarios.DeerHunt.Help  (deerHuntHelp)
import           Scenarios.TopBuy         (topBuy, topBuyDisplay)
import           Scenarios.Diner          (diner, dinerDisplay)
import           Scenarios.DinerMaya      (dinerMaya, dinerMayaDisplay)
import           Scenarios.Customer       (customer)

main :: IO ()
main = runLauncher
  [ ScenarioEntry
      { entryLabel        = "Deer Hunt"
      , entryTagline      = "Mid-November. Southern Manitoba. One square mile. One buck."
      , entryScenarioName = deerHuntName
      , entryDisplay      = deerHuntDisplay
      , entryMake         = deerHunt
      , entryHowToPlay    = Just deerHuntHelp
      , entryOnEnd        = Just deerHuntOnEnd
      }
  , ScenarioEntry
      { entryLabel        = "Top Buy"
      , entryTagline      = "A retail ethics dilemma. Your coworker is stealing."
      , entryScenarioName = "Top Buy"
      , entryDisplay      = topBuyDisplay
      , entryMake         = \seed you _progress _pubkey -> topBuy seed you
      , entryHowToPlay    = Nothing
      , entryOnEnd        = Nothing
      }
  , ScenarioEntry
      { entryLabel        = "Late Night Diner"
      , entryTagline      = "2 AM. Can't sleep. A diner, a server, a stranger."
      , entryScenarioName = "Late Night Diner"
      , entryDisplay      = dinerDisplay
      , entryMake         = \seed you _progress _pubkey -> diner seed you
      , entryHowToPlay    = Nothing
      , entryOnEnd        = Nothing
      }
  , ScenarioEntry
      { entryLabel        = "Diner: Maya"
      , entryTagline      = "The same night. Behind the counter."
      , entryScenarioName = "Late Night Diner: Maya"
      , entryDisplay      = dinerMayaDisplay
      , entryMake         = \seed you _progress _pubkey -> dinerMaya seed you
      , entryHowToPlay    = Nothing
      , entryOnEnd        = Nothing
      }
  , ScenarioEntry
      { entryLabel        = "Customer"
      , entryTagline      = "Walking through a store. (Prototype)"
      , entryScenarioName = "customer"
      , entryDisplay      = defaultDisplay
      , entryMake         = \seed you _progress _pubkey -> customer seed you
      , entryHowToPlay    = Nothing
      , entryOnEnd        = Nothing
      }
  ]
