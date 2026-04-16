module Engine.Core.WorldSpec (spec) where

import qualified Data.Map.Strict as Map
import           Test.Hspec

import           Engine.CRDT.ORSet  (orFromList)
import           Engine.Core.World
import           GameTypes
import           TestFixtures  (emptyWorld)

spec :: Spec
spec = describe "Engine.Core.World" $ do

  -- -------------------------------------------------------------------------
  -- formatHour
  -- -------------------------------------------------------------------------

  describe "formatHour" $ do
    it "0 = 12:00 AM (midnight)"  $ formatHour 0  `shouldBe` "12:00 AM"
    it "1 = 1:00 AM"              $ formatHour 1  `shouldBe` "1:00 AM"
    it "11 = 11:00 AM"            $ formatHour 11 `shouldBe` "11:00 AM"
    it "12 = 12:00 PM (noon)"     $ formatHour 12 `shouldBe` "12:00 PM"
    it "13 = 1:00 PM"             $ formatHour 13 `shouldBe` "1:00 PM"
    it "23 = 11:00 PM"            $ formatHour 23 `shouldBe` "11:00 PM"

  -- -------------------------------------------------------------------------
  -- dayOfWeekName
  -- -------------------------------------------------------------------------

  describe "dayOfWeekName" $ do
    it "0 = Monday"    $ dayOfWeekName 0 `shouldBe` "Monday"
    it "1 = Tuesday"   $ dayOfWeekName 1 `shouldBe` "Tuesday"
    it "2 = Wednesday" $ dayOfWeekName 2 `shouldBe` "Wednesday"
    it "3 = Thursday"  $ dayOfWeekName 3 `shouldBe` "Thursday"
    it "4 = Friday"    $ dayOfWeekName 4 `shouldBe` "Friday"
    it "5 = Saturday"  $ dayOfWeekName 5 `shouldBe` "Saturday"
    it "6 = Sunday"    $ dayOfWeekName 6 `shouldBe` "Sunday"
    it "fallback"      $ dayOfWeekName 9 `shouldBe` "Day 9"

  -- -------------------------------------------------------------------------
  -- seasonName
  -- -------------------------------------------------------------------------

  describe "seasonName" $ do
    it "0 = Spring"   $ seasonName 0 `shouldBe` "Spring"
    it "1 = Summer"   $ seasonName 1 `shouldBe` "Summer"
    it "2 = Autumn"   $ seasonName 2 `shouldBe` "Autumn"
    it "3 = Winter"   $ seasonName 3 `shouldBe` "Winter"
    it "fallback"     $ seasonName 4 `shouldBe` "Season 4"

  -- -------------------------------------------------------------------------
  -- lunarPhaseName (sparse: only named phases return Just)
  -- -------------------------------------------------------------------------

  describe "lunarPhaseName" $ do
    it "0 = New Moon"         $ lunarPhaseName 0  `shouldBe` Just "New Moon"
    it "4 = Waxing Crescent"  $ lunarPhaseName 4  `shouldBe` Just "Waxing Crescent"
    it "8 = First Quarter"    $ lunarPhaseName 8  `shouldBe` Just "First Quarter"
    it "11 = Waxing Gibbous"  $ lunarPhaseName 11 `shouldBe` Just "Waxing Gibbous"
    it "15 = Full Moon"       $ lunarPhaseName 15 `shouldBe` Just "Full Moon"
    it "19 = Waning Gibbous"  $ lunarPhaseName 19 `shouldBe` Just "Waning Gibbous"
    it "22 = Last Quarter"    $ lunarPhaseName 22 `shouldBe` Just "Last Quarter"
    it "26 = Waning Crescent" $ lunarPhaseName 26 `shouldBe` Just "Waning Crescent"
    it "unnamed phases return Nothing" $ do
      lunarPhaseName 1  `shouldBe` Nothing
      lunarPhaseName 14 `shouldBe` Nothing
      lunarPhaseName 28 `shouldBe` Nothing

  -- -------------------------------------------------------------------------
  -- lunarPhaseLabel (range-based: every day 0-28 maps to a name)
  -- -------------------------------------------------------------------------

  describe "lunarPhaseLabel" $ do
    it "0-3 = New Moon"         $ do lunarPhaseLabel 0 `shouldBe` "New Moon"
                                     lunarPhaseLabel 3 `shouldBe` "New Moon"
    it "4-7 = Waxing Crescent"  $ do lunarPhaseLabel 4 `shouldBe` "Waxing Crescent"
                                     lunarPhaseLabel 7 `shouldBe` "Waxing Crescent"
    it "8-10 = First Quarter"   $ do lunarPhaseLabel 8  `shouldBe` "First Quarter"
                                     lunarPhaseLabel 10 `shouldBe` "First Quarter"
    it "11-14 = Waxing Gibbous" $ do lunarPhaseLabel 11 `shouldBe` "Waxing Gibbous"
                                     lunarPhaseLabel 14 `shouldBe` "Waxing Gibbous"
    it "15-18 = Full Moon"      $ do lunarPhaseLabel 15 `shouldBe` "Full Moon"
                                     lunarPhaseLabel 18 `shouldBe` "Full Moon"
    it "19-21 = Waning Gibbous" $ do lunarPhaseLabel 19 `shouldBe` "Waning Gibbous"
                                     lunarPhaseLabel 21 `shouldBe` "Waning Gibbous"
    it "22-25 = Last Quarter"   $ do lunarPhaseLabel 22 `shouldBe` "Last Quarter"
                                     lunarPhaseLabel 25 `shouldBe` "Last Quarter"
    it "26+ = Waning Crescent"  $ do lunarPhaseLabel 26 `shouldBe` "Waning Crescent"
                                     lunarPhaseLabel 28 `shouldBe` "Waning Crescent"

  -- -------------------------------------------------------------------------
  -- World tag queries
  -- -------------------------------------------------------------------------

  describe "getHour" $ do
    it "Nothing when no time tag" $
      getHour emptyWorld `shouldBe` Nothing
    it "Just hour when tag present" $
      let w = emptyWorld { worldTags = orFromList [EngineTag (Clock (TimeOfDay 14))] }
      in getHour w `shouldBe` Just 14

  describe "getDayOfWeek" $ do
    it "Nothing when absent" $
      getDayOfWeek emptyWorld `shouldBe` Nothing
    it "Just day when present" $
      let w = emptyWorld { worldTags = orFromList [EngineTag (Clock (DayOfWeek 3))] }
      in getDayOfWeek w `shouldBe` Just 3

  describe "getWeather" $ do
    it "Nothing when absent" $
      getWeather emptyWorld `shouldBe` Nothing
    it "Just description when present" $
      let w = emptyWorld { worldTags = orFromList [EngineTag (Weather (WeatherDesc "rain"))] }
      in getWeather w `shouldBe` Just (WeatherDesc "rain")

  describe "getSeason" $ do
    it "Nothing when absent" $
      getSeason emptyWorld `shouldBe` Nothing
    it "Just season index when present" $
      let w = emptyWorld { worldTags = orFromList [EngineTag (Clock (Season 2))] }
      in getSeason w `shouldBe` Just 2

  describe "getDayNumber" $ do
    it "Nothing when absent" $
      getDayNumber emptyWorld `shouldBe` Nothing
    it "Just day number when present" $
      let w = emptyWorld { worldTags = orFromList [EngineTag (Clock (DayNumber 42))] }
      in getDayNumber w `shouldBe` Just 42

  describe "getLunarPhase" $ do
    it "Nothing when absent" $
      getLunarPhase emptyWorld `shouldBe` Nothing
    it "Just phase index when present" $
      let w = emptyWorld { worldTags = orFromList [EngineTag (Clock (LunarPhase 15))] }
      in getLunarPhase w `shouldBe` Just 15

  -- -------------------------------------------------------------------------
  -- engineStatusLine
  -- -------------------------------------------------------------------------

  describe "engineStatusLine" $ do
    let you = Named "you"

    it "Nothing when player has no location" $
      engineStatusLine you emptyWorld `shouldBe` Nothing

    it "Just location when only location is set (no calendar tags)" $
      let w = emptyWorld { worldLocations = Map.singleton you (Location "home") }
      in engineStatusLine you w `shouldBe` Just "home"

    it "full status line when location + all calendar tags present" $
      let w = emptyWorld
                { worldLocations = Map.singleton you (Location "office")
                , worldTags = orFromList
                    [ EngineTag (Clock (DayOfWeek 0))    -- Monday
                    , EngineTag (Clock (Season 0))       -- Spring
                    , EngineTag (Clock (LunarPhase 0))   -- New Moon
                    ]
                }
      in engineStatusLine you w
           `shouldBe` Just "office — Monday — Spring — New Moon"

    it "falls back to location-only when some calendar tags are missing" $
      let w = emptyWorld
                { worldLocations = Map.singleton you (Location "office")
                , worldTags = orFromList [EngineTag (Clock (DayOfWeek 0))]
                }
      in engineStatusLine you w `shouldBe` Just "office"
