module Engine.Core.TimeSpec (spec) where

import           Test.Hspec

import           Engine.Core.Time

spec :: Spec
spec = describe "Engine.Core.Time" $ do

  describe "timeOfDayPhase" $ do
    it "classifies representative hours" $ do
      timeOfDayPhase 0   `shouldBe` DeepNight
      timeOfDayPhase 3   `shouldBe` PreDawn
      timeOfDayPhase 7   `shouldBe` Dawn
      timeOfDayPhase 9   `shouldBe` Morning
      timeOfDayPhase 12  `shouldBe` Midday
      timeOfDayPhase 15  `shouldBe` Afternoon
      timeOfDayPhase 17  `shouldBe` GoldenHour
      timeOfDayPhase 19  `shouldBe` Dusk
      timeOfDayPhase 22  `shouldBe` Night
      timeOfDayPhase 23  `shouldBe` DeepNight

    it "wraps modulo 24 for out-of-range inputs" $ do
      timeOfDayPhase 24 `shouldBe` timeOfDayPhase 0
      timeOfDayPhase 48 `shouldBe` timeOfDayPhase 0
      timeOfDayPhase 25 `shouldBe` timeOfDayPhase 1

    it "partitions the full day across every phase exactly once" $
      -- Every hour maps somewhere, and every phase is reachable.
      let covered = [ timeOfDayPhase h | h <- [0..23] ]
      in do
        length covered `shouldBe` 24
        -- Each phase in TimePhase shows up in the 24-hour partition.
        mapM_ (\p -> (p `elem` covered) `shouldBe` True)
              [ minBound .. maxBound :: TimePhase ]

    it "is monotonic through the day apart from the DeepNight wrap" $
      -- Walking from 3 to 22 should produce a non-decreasing phase
      -- sequence (the wrap lives outside this range).
      let phases = [ timeOfDayPhase h | h <- [3..22] ]
      in and (zipWith (<=) phases (drop 1 phases)) `shouldBe` True
