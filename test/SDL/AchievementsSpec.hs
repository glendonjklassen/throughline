module SDL.AchievementsSpec (spec) where

import qualified Data.Map.Strict  as Map

import           Test.Hspec

import           GameTypes        (ClockTag(..), EngineTag(..), Tag(..),
                                   WorldDiff(..))
import           SDL.Achievements

spec :: Spec
spec = describe "SDL.Achievements" $ do

  describe "checkEarnAgainstDiff" $ do

    -- An empty diff can never earn anything.  Acts as a sanity
    -- check that the function doesn't short-circuit.
    it "awards nothing for an empty diff" $ do
      let diff = WorldDiff [] [] [] [] [] [] [] [] 0
      checkEarnAgainstDiff [] emptyEarnedMap diff `shouldBe` []

    it "awards nothing when the required tag isn't in diffWorldTagsAdded" $ do
      let diff = WorldDiff [] [] [] [] [tagA] [] [] [] 0
          cat  = [achievementFor "ach.b" "B" tagB]
      checkEarnAgainstDiff cat emptyEarnedMap diff `shouldBe` []

    it "awards an achievement when its required tag is added" $ do
      let diff = WorldDiff [] [] [] [] [tagA] [] [] [] 0
          a    = achievementFor "ach.a" "A" tagA
      map achId (checkEarnAgainstDiff [a] emptyEarnedMap diff)
        `shouldBe` ["ach.a"]

    -- Once earned, the same tag addition in a later diff must not
    -- grant the achievement again.  One-shot is the contract.
    it "does not re-award an already-earned achievement" $ do
      let diff   = WorldDiff [] [] [] [] [tagA] [] [] [] 0
          a      = achievementFor "ach.a" "A" tagA
          earned = Map.singleton "ach.a" "2026-04-22T00:00:00Z"
      checkEarnAgainstDiff [a] earned diff `shouldBe` []

-- Two distinct, deterministic engine tags.  Engine tags carry no
-- user-visible semantics here — we just need two Tag values we can
-- put in diffs and compare.
tagA, tagB :: Tag
tagA = EngineTag (Clock (TimeOfDay 1))
tagB = EngineTag (Clock (TimeOfDay 2))

achievementFor :: String -> String -> Tag -> Achievement
achievementFor aid name tag = Achievement
  { achId          = aid
  , achDisplayName = name
  , achDescription = "test"
  , achRequiredTag = tag
  }
