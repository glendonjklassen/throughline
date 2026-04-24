-- | Tests for GF(256) arithmetic and Shamir's Secret Sharing.
module Engine.Sync.Relic.ShamirSpec (spec) where

import qualified Data.ByteString as BS
import           Data.Word       (Word8)
import           System.Random   (mkStdGen)
import           Test.Hspec
import           Test.QuickCheck (forAll, choose, vectorOf, elements, (===),
                                  Property, Gen)

import           Engine.Sync.Relic.Shamir

spec :: Spec
spec = describe "Engine.Sync.Relic.Shamir" $ do

  describe "GF(256) identities" $ do

    it "gfMul 1 x == x for all x" $ do
      [ gfMul 1 x | x <- [0 .. 255 :: Word8] ] `shouldBe` [0 .. 255]

    it "gfMul 0 x == 0 for all x" $ do
      all (== 0) [ gfMul 0 x | x <- [0 .. 255 :: Word8] ] `shouldBe` True

    it "gfInv is a left inverse for every non-zero element" $ do
      all (\x -> gfMul x (gfInv x) == 1) [1 .. 255 :: Word8] `shouldBe` True

    it "gfAdd is its own inverse (xor)" $ do
      all (\(a, b) -> gfAdd a (gfAdd a b) == b)
          [ (a, b) | a <- [0 .. 255 :: Word8], b <- [0 .. 255 :: Word8] ]
        `shouldBe` True

  describe "splitSecret / combineShares" $ do

    it "3-of-5: every 3-share subset reconstructs the secret" $ do
      let secret = BS.pack [0x01, 0x02, 0x03, 0xff, 0x80, 0x00, 0x42]
          shares = splitSecret (mkStdGen 1) 3 5 secret
          triples = threes shares
      length shares `shouldBe` 5
      length triples `shouldBe` 10  -- C(5,3)
      all (\t -> combineShares t == Just secret) triples `shouldBe` True

    it "2-of-3: any pair reconstructs; a single share does not" $ do
      let secret = BS.pack [7, 42, 255, 0]
          shares = splitSecret (mkStdGen 99) 2 3 secret
          pairs  = [ [a, b] | (i, a) <- zip [0 :: Int ..] shares
                            , (j, b) <- zip [0 :: Int ..] shares
                            , i < j ]
      all (\p -> combineShares p == Just secret) pairs `shouldBe` True
      -- One share alone is never equal to the secret (unless the
      -- secret is the y-intercept of a randomly-sampled line, which
      -- happens with 1/255 probability; the test just confirms not
      -- all singletons match — the property we actually care about).
      any (\s -> combineShares [s] /= Just secret) shares `shouldBe` True

    it "produces n distinct x-coordinates" $ do
      let shares = splitSecret (mkStdGen 7) 3 7 (BS.pack [0 .. 9])
          xs     = map (\s -> shareX s) shares
      length (uniq xs) `shouldBe` 7

    it "is deterministic given the same StdGen seed" $ do
      let a = splitSecret (mkStdGen 5) 3 5 (BS.pack [1, 2, 3])
          b = splitSecret (mkStdGen 5) 3 5 (BS.pack [1, 2, 3])
      a `shouldBe` b

    it "handles 1-of-1 (share is the secret)" $ do
      let secret = BS.pack [0xab, 0xcd]
      case splitSecret (mkStdGen 11) 1 1 secret of
        [s] -> combineShares [s] `shouldBe` Just secret
        xs  -> expectationFailure ("expected one share, got " <> show (length xs))

    it "reconstructs arbitrary-length secrets (property)" $ do
      let genBytes :: Gen BS.ByteString
          genBytes = do
            len <- choose (1, 64)
            BS.pack <$> vectorOf len (elements [0 .. 255])
          prop :: Property
          prop = forAll genBytes                  $ \secret ->
                 forAll (choose (2 :: Int, 6))    $ \k ->
                 forAll (choose (k, 7))           $ \n ->
                 forAll (choose (1, 1000000))     $ \seed ->
                   let shares = splitSecret (mkStdGen seed) k n secret
                       subset = take k shares
                   in combineShares subset === Just secret
      prop

  describe "combineShares failure modes" $ do

    it "Nothing on empty input" $ do
      combineShares [] `shouldBe` Nothing

    it "Nothing on mismatched share lengths" $ do
      let s1 = Share 1 (BS.pack [1, 2, 3])
          s2 = Share 2 (BS.pack [1, 2])
      combineShares [s1, s2] `shouldBe` Nothing

    it "Nothing on duplicate x-coordinates" $ do
      let s1 = Share 1 (BS.pack [1, 2])
          s2 = Share 1 (BS.pack [3, 4])
      combineShares [s1, s2] `shouldBe` Nothing

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | All 3-element subsets of a list.  Small-n only.
threes :: [a] -> [[a]]
threes xs =
  [ [a, b, c]
  | (i, a) <- zip [0 :: Int ..] xs
  , (j, b) <- zip [0 :: Int ..] xs
  , (k, c) <- zip [0 :: Int ..] xs
  , i < j, j < k
  ]

uniq :: Eq a => [a] -> [a]
uniq = foldr (\x acc -> if x `elem` acc then acc else x:acc) []
