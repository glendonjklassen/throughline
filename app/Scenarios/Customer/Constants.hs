module Scenarios.Customer.Constants where

import qualified Data.Map.Strict    as Map
import           Engine.Author.DSL  (emptyTags)
import           GameTypes
import           Scenarios.TopBuy.Locations

initialWorld :: Int -> CharId -> GameWorld
initialWorld seed you = GameWorld
  { worldCharacters    = Map.fromList
      [ (you, Character you "Customer" [] emptyTags) ]
  , worldGraph         = Map.empty
  , worldLocations     = Map.fromList [(you, parkingLot)]
  , worldActiveEffects = []
  , worldClock         = LamportClock 0 (PlayerId "init")
  , worldTags          = emptyTags
  , worldLocationGraph = emptyLocationGraph
  , worldSeed          = seed
  , worldLocationHistory = Map.empty
  , worldLocationVisits  = Map.empty
  , worldJournal         = []
  , worldDayNumber       = 1
  }
