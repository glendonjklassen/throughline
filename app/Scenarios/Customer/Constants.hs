module Scenarios.Customer.Constants where

import qualified Data.Map.Strict    as Map
import           Engine.CRDT.ORSet  (orEmpty)
import           GameTypes
import           Scenarios.TopBuy.Locations

initialWorld :: Int -> CharId -> GameWorld
initialWorld seed you = GameWorld
  { worldCharacters    = Map.fromList
      [ (you, Character you "Customer" [] orEmpty) ]
  , worldGraph         = Map.empty
  , worldLocations     = Map.fromList [(you, parkingLot)]
  , worldActiveEffects = []
  , worldClock         = LamportClock 0 (PlayerId "init")
  , worldTags          = orEmpty
  , worldLocationGraph = emptyLocationGraph
  , worldSeed          = seed
  , worldLocationHistory = Map.empty
  , worldLocationVisits  = Map.empty
  }
