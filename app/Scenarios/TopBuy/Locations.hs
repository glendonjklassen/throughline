module Scenarios.TopBuy.Locations where

import           GameTypes (Location(..))

-- | Physical locations inside the Top Buy store and its immediate surroundings.
-- Imported by any scenario set in this store so location strings share a
-- single source of truth for merge correctness.

parkingLot :: Location
parkingLot = Location "Top Buy: Parking Lot"

entrance :: Location
entrance = Location "Top Buy: Entrance"

salesFloor :: Location
salesFloor = Location "Top Buy: Sales Floor"

electronics :: Location
electronics = Location "Top Buy: Electronics"

backOffice :: Location
backOffice = Location "Top Buy: Back Office"
