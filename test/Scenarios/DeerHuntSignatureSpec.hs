-- | Tests for the Tier-1 per-hunt signature find: the deterministic
-- placement and rendering of one unique discovery per seed.
module Scenarios.DeerHuntSignatureSpec (spec) where

import           Data.List                      (isPrefixOf)
import qualified Data.Map.Strict as Map
import           Test.Hspec

import           Engine.CRDT.ORSet              (orMember, orToList)
import           GameTypes                      (Location(..), worldTags)

import           Scenarios.DeerHunt.Constants   (initialWorld)
import           Scenarios.DeerHunt.Discoveries (Discovery(..), DiscoveryKind(..),
                                                 discoveryTag)
import           Scenarios.DeerHunt.Signature
import           Scenarios.DeerHunt.World       (huntWorld, hwSignature,
                                                 hwSignatureLoc, hwByClass)

import           GameTypes                      (CharId(..))

spec :: Spec
spec = describe "Scenarios.DeerHunt.Signature" $ do

  describe "buildSignature" $ do

    it "is deterministic — same seed, same find" $ do
      buildSignature 12345 `shouldBe` buildSignature 12345

    it "varies across seeds" $ do
      let seeds   = [1 .. 40 :: Int]
          names   = map (sigName . buildSignature) seeds
          unique  = length (filter id (zipWith (/=) names (drop 1 names)))
      -- Any seed sweep should turn up multiple distinct signatures —
      -- the guard against a degenerate constant-output build.
      unique `shouldSatisfy` (> 5)

    it "every signature has at least one detail line" $ do
      let sigs = map buildSignature [1 .. 20 :: Int]
      all (not . null . sigDetail) sigs `shouldBe` True

    it "every signature's name has an archetype prefix" $ do
      let sigs = map buildSignature [1 .. 20 :: Int]
          prefixes = ["shed ", "cairn ", "carving ", "skull "]
          ok s = any (`isPrefixOf` sigName s) prefixes
      all ok sigs `shouldBe` True

  describe "placeSignature" $ do

    it "returns Nothing when no candidates exist" $ do
      let sig = buildSignature 42
      placeSignature 42 sig Map.empty `shouldBe` Nothing

    it "returns a location within the seeded candidates" $ do
      let hw   = huntWorld 123
          locs = concat (Map.elems (hwByClass hw))
      case hwSignatureLoc hw of
        Nothing -> expectationFailure "expected a signature location on a generated map"
        Just l  -> locs `shouldSatisfy` (l `elem`)

  describe "huntWorld wiring" $ do

    it "seeds a signature into HuntWorld deterministically" $ do
      let hw1 = huntWorld 777
          hw2 = huntWorld 777
      sigName (hwSignature hw1) `shouldBe` sigName (hwSignature hw2)
      hwSignatureLoc hw1 `shouldBe` hwSignatureLoc hw2

  describe "Discovery integration" $ do

    it "signature discovery tag is distinct from rare-find tag" $ do
      let sig     = buildSignature 11
          sigTag  = discoveryTag (Discovery Signature (sigName sig))
          findTag = discoveryTag (Discovery Find "shed antler")
      sigTag `shouldNotBe` findTag

  describe "World-tag wire format" $ do

    it "parseSignatureLocTag round-trips a location" $ do
      let loc = Location "North Bush"
      parseSignatureLocTag (signatureLocTag loc) `shouldBe` Just loc

    it "parseSignatureArchetypeTag round-trips every archetype" $ do
      let archs = [minBound .. maxBound :: SignatureArchetype]
          rounds = [ parseSignatureArchetypeTag (signatureArchetypeTag a) | a <- archs ]
      rounds `shouldBe` map Just archs

    it "parseSignatureLocTag rejects unrelated tags" $ do
      parseSignatureLocTag (signatureArchetypeTag SigAntler) `shouldBe` Nothing
      parseSignatureLocTag signatureFoundTag `shouldBe` Nothing

  describe "initialWorld emits signature tags" $ do

    it "tags the signature's archetype and location at init" $ do
      let hw   = huntWorld 4242
          you  = Named "you"
          w    = initialWorld hw you
          ts   = orToList (worldTags w)
          mLoc = [l | t <- ts, Just l <- [parseSignatureLocTag t]]
          mArc = [a | t <- ts, Just a <- [parseSignatureArchetypeTag t]]
      mLoc `shouldBe` maybe [] pure (hwSignatureLoc hw)
      mArc `shouldBe` [sigArchetype (hwSignature hw)]

    it "does not pre-set signatureFoundTag" $ do
      let hw  = huntWorld 4242
          you = Named "you"
          w   = initialWorld hw you
      orMember signatureFoundTag (worldTags w) `shouldBe` False
