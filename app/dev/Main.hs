-- | Multi-scenario dev launcher.  Ships every scenario the project
-- currently has.  For Steam bundles, a single-scenario executable like
-- 'DeerHuntMain' is the right entry point.
module Main where

import           SDL.Launcher             (ScenarioEntry(..), runLauncher)
import           SDL.Layout               (defaultDisplay)

import           Scenarios.DeerHunt       (deerHunt, deerHuntDisplay)
import           Scenarios.DeerHunt.Help  (deerHuntHelp)
import           Scenarios.TopBuy         (topBuy, topBuyDisplay)
import           Scenarios.Diner          (diner, dinerDisplay)
import           Scenarios.DinerMaya      (dinerMaya, dinerMayaDisplay)
import           Scenarios.Customer       (customer)

main :: IO ()
main = runLauncher
  [ ScenarioEntry "Deer Hunt"
      "Mid-November. Southern Manitoba. One square mile. One buck."
      deerHuntDisplay deerHunt (Just deerHuntHelp)
  , ScenarioEntry "Top Buy"
      "A retail ethics dilemma. Your coworker is stealing."
      topBuyDisplay topBuy Nothing
  , ScenarioEntry "Late Night Diner"
      "2 AM. Can't sleep. A diner, a server, a stranger."
      dinerDisplay diner Nothing
  , ScenarioEntry "Diner: Maya"
      "The same night. Behind the counter."
      dinerMayaDisplay dinerMaya Nothing
  , ScenarioEntry "Customer"
      "Walking through a store. (Prototype)"
      defaultDisplay customer Nothing
  ]
