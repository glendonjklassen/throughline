module Scenarios.DeerHunt.Probability
  ( experience
  , spookChance
  , spookChanceSitting
  , shotAccuracy
  , friendlyFireChance
  , doesDeerSpook
  , doesShotHit
  , isFriendlyFire
  , terrainSpookModifier
  , effectiveNoise
  , windAlignment
  , windSpookModifier
  , cardinalLabel
  , stillnessSpookModifier
  , stillnessShotModifier
  , getStillness
  ) where

import qualified Data.Map.Strict as Map
import           Data.Maybe              (fromMaybe)
import           Engine.Author.DSL         (hasTag)
import           Engine.Author.Random      (rollCheck)
import           Engine.Core.Conditions    (getCharStat)
import           Engine.Core.World         (getWeather)
import           GameTypes
import           Scenarios.DeerHunt.Constants (movingFast, saltSpook, saltShot, saltFriendlyFire)
import           Scenarios.DeerHunt.Terrain    (TerrainNoise(..), TerrainVisibility(..),
                                                 classNoise, classVisibility)
import           Scenarios.DeerHunt.World      (HuntWorld, hwClass)

-- ---------------------------------------------------------------------------
-- Experience & probability helpers
-- ---------------------------------------------------------------------------

-- | Experience is the Understanding stat on Truth -> player.
experience :: CharId -> GameWorld -> Int
experience cid world = fromMaybe 2 (getCharStat cid (Capacity Understanding) world)

-- | Spook probability when moving into the deer's node.
spookChance :: GameWorld -> CharId -> Double
spookChance world you =
  let exp' = experience you world
      fast = hasTag world movingFast
      base | fast      = 0.70
           | otherwise = 0.40
      expBonus = fromIntegral (min exp' 8) * 0.04
  in max 0.05 (base - expBonus)

-- | Spook probability when sitting still and the deer walks in.
spookChanceSitting :: GameWorld -> CharId -> Double
spookChanceSitting world you =
  let exp' = experience you world
      base = 0.15
      expBonus = fromIntegral (min exp' 8) * 0.02
  in max 0.02 (base - expBonus)

-- | Shot accuracy: probability of a clean kill.
-- Modified by stillness: long sitting = cold hands = slightly worse accuracy.
shotAccuracy :: GameWorld -> CharId -> Double
shotAccuracy world you =
  let exp' = experience you world
      base = min 0.85 (0.35 + fromIntegral exp' * 0.07)
      still = getStillness you world
  in max 0.10 (base + stillnessShotModifier still)

-- | Probability of hitting another hunter instead of the deer.
friendlyFireChance :: Double
friendlyFireChance = 0.10

-- | Does the deer spook on this tick?
doesDeerSpook :: GameWorld -> CharId -> Bool
doesDeerSpook world you = rollCheck world saltSpook (spookChance world you)

-- | Does the shot connect?
doesShotHit :: GameWorld -> CharId -> Bool
doesShotHit world you = rollCheck world saltShot (shotAccuracy world you)

-- | Is it friendly fire?
isFriendlyFire :: GameWorld -> Bool
isFriendlyFire world = rollCheck world saltFriendlyFire friendlyFireChance

-- ---------------------------------------------------------------------------
-- Terrain modifiers
-- ---------------------------------------------------------------------------

-- | Weather-adjusted noise level. Snow and wind muffle sound (one step quieter).
-- Frozen ground in clear cold makes dense bush slightly louder.
effectiveNoise :: HuntWorld -> Location -> GameWorld -> TerrainNoise
effectiveNoise hw loc world =
  let base = classNoise (hwClass hw loc)
      weather = getWeather world
  in case weather of
       Just (WeatherDesc "Light Snow") -> quieter base
       Just (WeatherDesc "Windy")      -> quieter base
       _                               -> base
  where
    quieter Loud     = Moderate
    quieter Moderate = Quiet
    quieter Quiet    = Quiet

-- | Additive spook modifier from terrain at player and deer locations.
-- Noise at the player's location (positive = louder = easier to detect).
-- Visibility at the deer's location (open = deer sees you coming).
-- Only applies noise when the player is moving (not sitting).
terrainSpookModifier :: HuntWorld -> Location -> Location -> Bool -> GameWorld -> Double
terrainSpookModifier hw playerLoc deerLoc isMoving world =
  let noiseAdd | not isMoving = 0.0
               | otherwise = case effectiveNoise hw playerLoc world of
                   Loud     ->  0.15
                   Moderate ->  0.0
                   Quiet    -> -0.05
      visAdd = case classVisibility (hwClass hw deerLoc) of
                 Open    ->  0.10
                 Partial ->  0.0
                 Dense   -> -0.10
  in noiseAdd + visAdd

-- ---------------------------------------------------------------------------
-- Wind modifiers
-- ---------------------------------------------------------------------------

-- | Compute the alignment between wind vector and player-to-deer vector.
-- Returns -1.0 (perfectly downwind = scent away from deer) to
-- +1.0 (perfectly upwind = scent blown toward deer).
-- Requires coordinates for both locations.
windAlignment :: Location -> Location -> Double -> GameWorld -> Double
windAlignment playerLoc deerLoc windAngle world =
  let coords = lgCoords (worldLocationGraph world)
  in case (Map.lookup playerLoc coords, Map.lookup deerLoc coords) of
       (Just (px, py), Just (dx, dy)) ->
         let -- Vector from player to deer
             rawX = dx - px
             rawY = dy - py
             mag  = sqrt (rawX * rawX + rawY * rawY)
             (pdx, pdy) | mag < 0.001 = (0.0, 0.0)  -- same location
                         | otherwise   = (rawX / mag, rawY / mag)
             -- Wind direction vector (direction wind is blowing toward)
             rad = windAngle * pi / 180.0
             wx  = sin rad
             wy  = cos rad
             -- Dot product: positive means wind blows toward deer
         in pdx * wx + pdy * wy
       _ -> 0.0  -- no coordinates available, no wind effect

-- | Multiplicative spook modifier from wind.
-- alignment: -1.0 (downwind) to +1.0 (upwind)
-- strength: 0.0 (calm) to 1.0 (strong)
-- Result: 0.1 to 2.0 (clamped)
windSpookModifier :: Double -> Double -> Double
windSpookModifier alignment strength =
  let raw = 1.0 + alignment * strength
  in max 0.1 (min 2.0 raw)

-- | Convert a continuous angle (0-360) to the nearest cardinal/intercardinal label.
cardinalLabel :: Double -> String
cardinalLabel deg =
  let labels = ["N","NE","E","SE","S","SW","W","NW"]
      normalized = deg - fromIntegral (floor (deg / 360.0) * 360 :: Int)
      idx = round (normalized / 45.0) `mod` 8
  in labels !! idx

-- ---------------------------------------------------------------------------
-- Stillness modifiers
-- ---------------------------------------------------------------------------

-- | Read the player's current Stillness stat (0–10).
getStillness :: CharId -> GameWorld -> Int
getStillness cid world = fromMaybe 0 (getCharStat cid (Capacity Stillness) world)

-- | Additive spook modifier from stillness.
-- Higher stillness = harder to detect when deer walks into your location.
stillnessSpookModifier :: Int -> Double
stillnessSpookModifier s
  | s <= 0    =  0.0
  | s <= 2    = -0.02
  | s <= 5    = -0.05
  | s <= 8    = -0.08
  | otherwise = -0.10

-- | Additive shot accuracy modifier from stillness.
-- Long sitting = cold, stiff hands = slightly worse accuracy.
stillnessShotModifier :: Int -> Double
stillnessShotModifier s
  | s <= 3    =  0.0
  | s <= 6    = -0.02
  | otherwise = -0.05
