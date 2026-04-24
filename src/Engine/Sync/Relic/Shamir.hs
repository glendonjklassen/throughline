-- | Shamir's Secret Sharing over GF(256).  Self-contained
-- implementation used by both ends of the Tier-3 relic protocol:
-- the oracle splits a secret into @n@ shares at genesis; the client
-- reconstructs by combining @k@ shares (k-of-n threshold).
--
-- Why a handwritten SSS?  The Haskell ecosystem's SSS packages are
-- either unmaintained or bind to a C crypto lib we don't otherwise
-- need.  The primitive is small (~GF(256) + Lagrange interpolation
-- at x=0) and the serialization format has to match the OpenAPI
-- spec byte-for-byte.  Rolling it here keeps the wire shape and the
-- arithmetic in one place.
--
-- ⚠️  This is a *correctness* implementation, not a *side-channel
-- resistant* one.  The lookup tables and the byte-at-a-time mul
-- are fine for a 32-byte secret used once per combine, but don't
-- reach for this module from a hot path that handles adversarial
-- inputs.  For relic combines the threat model is "someone handed
-- me a tampered share", which the set-hash check on the server
-- catches anyway.
module Engine.Sync.Relic.Shamir
  ( -- * Shares
    Share (..)
  , splitSecret
  , combineShares
    -- * Low-level GF(256) — exposed for tests
  , gfAdd
  , gfMul
  , gfInv
  ) where

import qualified Data.ByteString    as BS
import           Data.Bits          (shiftL, xor, (.&.))
import           Data.Word          (Word8)
import           System.Random      (StdGen, randomR)

-- | One share of a secret.  The secret is @'splitSecret' secret k n@
-- → @n@ shares; any @k@ of them reconstruct via 'combineShares'.
--
-- The layout is intentionally boring: an x-coordinate in [1..255]
-- (zero is reserved for the secret itself), and a ByteString of
-- y-values — one y per byte of the original secret.  Share size
-- grows linearly with secret size; no ciphering, no compression.
data Share = Share
  { shareX     :: !Word8
  , shareBytes :: !BS.ByteString
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Splitting
-- ---------------------------------------------------------------------------

-- | Split a secret into @n@ shares, any @k@ of which reconstruct.
-- Uses the provided 'StdGen' so callers can seed the oracle's
-- splitting deterministically from a genesis seed (the oracle
-- *must* be deterministic so repeated share derivation stays stable).
--
-- Returns shares with x-coordinates 1..n.  @k@ and @n@ must satisfy
-- @1 <= k <= n <= 255@; violations produce an empty list.
splitSecret :: StdGen -> Int -> Int -> BS.ByteString -> [Share]
splitSecret gen0 k n secret
  | k < 1 || n < k || n > 255 = []
  | otherwise =
      let -- For each secret byte, draw (k-1) random coefficients.
          -- The polynomial is then
          --     p(x) = secret_byte + c1*x + c2*x^2 + ... + c_{k-1}*x^{k-1}
          -- evaluated via Horner at each share's x.
          (coefMatrix, _) = drawCoefficients gen0 (BS.length secret) (k - 1)
          xs              = [1 .. fromIntegral n :: Word8]
      in [ Share x (shareBytesFor x coefMatrix secret) | x <- xs ]

-- | Build the (byteCount × (k-1)) matrix of random coefficients.
-- Shape: @[[Word8]]@ outer-indexed by secret-byte, inner-indexed by
-- polynomial-coefficient (from c1 up to c_{k-1}).
drawCoefficients :: StdGen -> Int -> Int -> ([[Word8]], StdGen)
drawCoefficients gen0 nBytes nCoefs = go gen0 nBytes []
  where
    go g 0 acc = (reverse acc, g)
    go g i acc =
      let (row, g') = drawRow g nCoefs []
      in go g' (i - 1) (row : acc)

    drawRow g 0 acc = (reverse acc, g)
    drawRow g i acc =
      let (w, g') = randomR (0, 255 :: Int) g
      in drawRow g' (i - 1) (fromIntegral w : acc)

-- | Compute one share's bytes by Horner-evaluating each byte's
-- polynomial at @x@.
shareBytesFor :: Word8 -> [[Word8]] -> BS.ByteString -> BS.ByteString
shareBytesFor x coefMatrix secret =
  BS.pack (zipWith (evalByte x) coefMatrix (BS.unpack secret))
  where
    -- Horner on the full polynomial
    --   p(t) = secret_byte + a_1·t + a_2·t^2 + ... + a_{k-1}·t^{k-1}.
    -- Iterate from the highest coefficient down to the constant
    -- term (which is the secret byte).  Each step: acc ← c + acc·t.
    evalByte :: Word8 -> [Word8] -> Word8 -> Word8
    evalByte x' coefs secretByte =
      let highToLow = reverse coefs ++ [secretByte]
      in foldl (\acc c -> gfAdd c (gfMul acc x')) 0 highToLow

-- ---------------------------------------------------------------------------
-- Combining
-- ---------------------------------------------------------------------------

-- | Reconstruct a secret from a list of shares.  Returns 'Nothing'
-- on obvious malformed input (empty list, mismatched share lengths,
-- duplicate x-coordinates).  Returns a possibly-wrong secret if the
-- shares themselves are tampered with — that's the oracle's set-hash
-- check to catch, not ours.
--
-- Lagrange interpolation at @x = 0@:
--     secret = Σ y_i · Π_{j≠i} (0 - x_j) / (x_i - x_j)
-- rearranged in GF(256) (subtraction = addition = xor).
combineShares :: [Share] -> Maybe BS.ByteString
combineShares [] = Nothing
combineShares shares@(first:_)
  | not (allSameLength shares)   = Nothing
  | hasDuplicateXs shares        = Nothing
  | otherwise = Just (BS.pack [ combineByte i | i <- [0 .. len - 1] ])
  where
    len = BS.length (shareBytes first)
    allSameLength xs = all (\s -> BS.length (shareBytes s) == len) xs
    hasDuplicateXs xs =
      let xs' = map shareX xs
      in length xs' /= length (dedup xs')
      where
        dedup = foldr (\x acc -> if x `elem` acc then acc else x:acc) []

    -- For each byte-position across all shares, compute the
    -- Lagrange sum at x=0.
    combineByte :: Int -> Word8
    combineByte byteIx =
      let yPairs = [ (shareX s, BS.index (shareBytes s) byteIx) | s <- shares ]
      in lagrangeAtZero yPairs

-- | Lagrange interpolation at @x = 0@ for a list of @(x_i, y_i)@ pairs.
-- Everything is GF(256).
lagrangeAtZero :: [(Word8, Word8)] -> Word8
lagrangeAtZero pts =
  foldr gfAdd 0 [ gfMul y (basisAtZero i) | (i, (_, y)) <- zip [0 ..] pts ]
  where
    xs = map fst pts
    -- L_i(0) = Π_{j≠i} (0 - x_j) / (x_i - x_j)
    --        = Π_{j≠i} x_j / (x_i + x_j)   -- "minus" is "xor" in GF(2^8), so 0 - a = a.
    basisAtZero :: Int -> Word8
    basisAtZero i =
      let xi = xs !! i
          others = [ (j, xj) | (j, xj) <- zip [0 ..] xs, j /= i ]
      in foldr (\(_, xj) acc -> gfMul acc (gfMul xj (gfInv (xi `xor` xj))))
               1
               others

-- ---------------------------------------------------------------------------
-- GF(256) arithmetic
-- ---------------------------------------------------------------------------
--
-- Field polynomial: x^8 + x^4 + x^3 + x + 1 (= 0x11b), the same one
-- AES uses.  Generator: 0x03.  Pre-computed exp/log tables make
-- multiply and invert O(1).

-- | Addition in GF(2^8) is xor.
gfAdd :: Word8 -> Word8 -> Word8
gfAdd = xor

-- | Multiplication in GF(2^8) via exp/log tables.
gfMul :: Word8 -> Word8 -> Word8
gfMul 0 _ = 0
gfMul _ 0 = 0
gfMul a b =
  let la = fromIntegral (logTable `BS.index` fromIntegral a) :: Int
      lb = fromIntegral (logTable `BS.index` fromIntegral b) :: Int
      s  = (la + lb) `mod` 255
  in expTable `BS.index` s

-- | Multiplicative inverse in GF(2^8).  Inverse of 0 is 0 (sentinel;
-- never legitimately produced during Lagrange since we skip the
-- @i == j@ term).
gfInv :: Word8 -> Word8
gfInv 0 = 0
gfInv a =
  let la = fromIntegral (logTable `BS.index` fromIntegral a) :: Int
      s  = (255 - la) `mod` 255
  in expTable `BS.index` s

-- | @expTable ! i@ = generator^i in GF(2^8).  Index 0 = 1.
expTable :: BS.ByteString
expTable = BS.pack (go 1 [])
  where
    go _ acc | length acc == 256 = reverse acc
    go x acc =
      -- generator is 0x03; multiply by it byte-by-byte in GF(2^8)
      -- with the AES reduction polynomial 0x11b.
      let acc' = x : acc
          x'   = xtime x `xor` x  -- equivalent to x * 3 in GF(2^8)
      in go x' acc'

    -- "xtime": multiply by x in GF(2^8) with reduction by 0x11b.
    xtime :: Word8 -> Word8
    xtime v =
      let shifted = v `shiftL` 1
          reduce  = if v .&. 0x80 /= 0 then 0x1b else 0
      in shifted `xor` reduce

-- | @logTable ! x@ = i such that generator^i = x.  @logTable ! 0@
-- is a sentinel (0) and callers must never look it up — 'gfMul' and
-- 'gfInv' short-circuit the zero case.
logTable :: BS.ByteString
logTable = BS.pack (go 0 (replicate 256 0))
  where
    go 255 acc = acc
    go i acc   =
      let v = fromIntegral (expTable `BS.index` i) :: Int
          acc' = take v acc ++ [fromIntegral i] ++ drop (v + 1) acc
      in go (i + 1) acc'
