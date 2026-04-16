-- | Lamport-clock-seeded PRNG helpers for scenario authors.
--
-- All randomness is deterministic: seeded from the world clock tick combined
-- with a per-use salt integer. Different salts produce independent random
-- streams for the same tick. No engine signatures need to change.
--
-- Seed derivation uses SHA-256 (via crypton) to mix the tick and salt into
-- a well-distributed 32-bit seed for System.Random. This replaces the
-- earlier ad-hoc prime multiplication.
module Engine.Author.Random
  ( scenarioGen
  , scenarioSeed
  , rollD
  , rollDice
  , rollCheck
  , rollChoice
  ) where

import           Crypto.Hash       (SHA256(..), hashWith)
import           Data.Bits         (shiftR, (.&.))
import qualified Data.ByteArray    as BA
import qualified Data.ByteString   as BS
import           Data.Word         (Word8)
import           System.Random     (StdGen, mkStdGen, randomR)
import           GameTypes         (GameWorld(..), LamportClock(..))

-- | Derive a deterministic seed from a clock tick and a caller-chosen salt.
-- Uses SHA-256 to mix the inputs, then extracts 4 bytes as an Int seed
-- for System.Random.  This guarantees uniform distribution across the seed
-- space regardless of how close the tick/salt values are.
scenarioSeed :: Int -> Int -> Int
scenarioSeed tick salt =
  let input = BS.pack (intToBytes tick ++ intToBytes salt)
      digest = BA.unpack (hashWith SHA256 input) :: [Word8]
  in case digest of
       (b0:b1:b2:b3:_) -> fromIntegral b0 + fromIntegral b1 * 256
                         + fromIntegral b2 * 65536 + fromIntegral b3 * 16777216
       _ -> 0  -- unreachable: SHA-256 always produces 32 bytes

-- | Encode an Int as 8 big-endian bytes for hashing.
intToBytes :: Int -> [Word8]
intToBytes n =
  [ fromIntegral (shiftR n 56 .&. 0xFF)
  , fromIntegral (shiftR n 48 .&. 0xFF)
  , fromIntegral (shiftR n 40 .&. 0xFF)
  , fromIntegral (shiftR n 32 .&. 0xFF)
  , fromIntegral (shiftR n 24 .&. 0xFF)
  , fromIntegral (shiftR n 16 .&. 0xFF)
  , fromIntegral (shiftR n  8 .&. 0xFF)
  , fromIntegral ( n           .&. 0xFF)
  ]

-- | Base PRNG seeded from the session seed, current clock tick, and a caller-chosen salt.
-- The session seed makes each playthrough different. The tick + salt make
-- each roll within a playthrough independent and deterministic.
scenarioGen :: GameWorld -> Int -> StdGen
scenarioGen world salt = mkStdGen (scenarioSeed (worldSeed world + lcTick (worldClock world)) salt)

-- | Random Double in [0, 1) from the world state and a salt.
rollD :: GameWorld -> Int -> Double
rollD world salt = fst $ randomR (0.0 :: Double, 1.0) (scenarioGen world salt)

-- | Roll a die with the given number of sides. Returns a value in 1..sides.
rollDice :: GameWorld -> Int -> Int -> Int
rollDice world salt sides = fst $ randomR (1, sides) (scenarioGen world salt)

-- | Roll against a probability threshold. Returns True approximately
-- @prob@ fraction of the time (e.g. 0.7 succeeds ~70%).
rollCheck :: GameWorld -> Int -> Double -> Bool
rollCheck world salt prob = rollD world salt < prob

-- | Pick a random element from a list. Errors on empty input.
rollChoice :: GameWorld -> Int -> [a] -> a
rollChoice _     _    []   = error "rollChoice: empty list"
rollChoice world salt xs   = xs !! idx
  where (idx, _) = randomR (0, length xs - 1) (scenarioGen world salt)
