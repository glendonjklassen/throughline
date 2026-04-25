-- | Player-facing how-to-play copy for the Deer Hunt scenario.
-- Engine owns the standard controls block; this module supplies the
-- hunt's framing line.
module Scenarios.DeerHunt.Help (deerHuntHelp) where

import Engine.Author.Help (helpScreen)

deerHuntHelp :: [String]
deerHuntHelp = helpScreen
  [ "Mid-November. You have a tag to fill." ]
