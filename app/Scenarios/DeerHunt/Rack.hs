-- | Per-buck antler variation.  Each day's buck has a unique rack
-- derived deterministically from the hunt seed and the current day
-- number — no state to track, no extra storage, just a pure function
-- the kill beat and journal can consult.
--
-- The rack is flavor rather than mechanic: the kill journal names the
-- specific rack, so over a season the hunter ends up with a string of
-- distinct bucks they can page back through instead of one generic
-- kill line repeating.
module Scenarios.DeerHunt.Rack
  ( Rack (..)
  , RackMass (..)
  , currentRack
  , describeRack
  ) where

import           System.Random (mkStdGen, randomR)

import           Scenarios.DeerHunt.World (HuntWorld, hwSeed)

-- | A buck's antlers.  Tines are the countable points; spread is the
-- outside-to-outside width (inches); mass is a qualitative descriptor
-- that shades the adjective in prose ("heavy", "tall", "thin", "clean").
data Rack = Rack
  { rackTines  :: !Int
  , rackSpread :: !Int
  , rackMass   :: !RackMass
  }
  deriving (Show, Eq)

data RackMass = Thin | Clean | Heavy | Tall
  deriving (Show, Eq, Enum, Bounded)

-- | The rack of the buck on day @dayN@ of this hunt section.  Same
-- seed + same day always produces the same rack; a new day produces
-- a new rack because the buck is a different animal.
currentRack :: HuntWorld -> Int -> Rack
currentRack hw dayN =
  let g0            = mkStdGen (hwSeed hw * 10007 + dayN * 29 + 5)
      (tines, g1)   = randomR (6, 10 :: Int) g0
      (spread, g2)  = randomR (13, 22 :: Int) g1
      (massIdx, _)  = randomR (0, 3 :: Int) g2
      mass          = toEnum massIdx :: RackMass
  in Rack tines spread mass

-- | Prose form: "8-point, 18-inch heavy rack".  Drops cleanly into
-- the kill journal entry.
describeRack :: Rack -> String
describeRack (Rack tines spread mass) =
  show tines <> "-point, " <> show spread <> "-inch " <> massWord mass <> " rack"
  where
    massWord Thin  = "thin"
    massWord Clean = "clean"
    massWord Heavy = "heavy"
    massWord Tall  = "tall"
