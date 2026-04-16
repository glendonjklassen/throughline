-- =============================================================================
-- Engine.Core.SystemAxiomsSpec
--
-- System axioms are engine-owned rules that run automatically in every
-- scenario alongside any scenario-specific axioms. They handle universal
-- simulation concerns -- movement narration, calendar advancement -- so
-- scenario authors never have to wire these up manually.
--
-- Currently tested:
--   locationTransitionAxiom -- narrates when characters move
--   dayAdvanceAxiom         -- advances calendar counters at midnight
-- =============================================================================
module Engine.Core.SystemAxiomsSpec (spec) where

import           Test.Hspec

import           Engine.Core.Axioms
import           Engine.Author.DSL
import           Engine.CRDT.ORSet
import           GameTypes
import           TestFixtures

spec :: Spec
spec = describe "Engine.Core.SystemAxioms" $ do

  -- =========================================================================
  -- locationTransitionAxiom (system axiom)
  --
  -- A system axiom that runs in every scenario. When the diff contains
  -- LocationDelta entries (characters moved this tick), it produces a
  -- Narrate effect for each one: "characterId -> newLocation".
  -- This gives the player prose feedback about movement without any
  -- scenario author having to wire it up manually.
  -- =========================================================================

  describe "locationTransitionAxiom" $ do
    it "narrates when a character moves" $
      -- The diff says "player" moved from A to B. The system axiom should
      -- produce a narration. Note: we pass [] for scenario axioms because
      -- locationTransitionAxiom is a system axiom -- it runs automatically.
      let diff = emptyDiff { diffLocations = [LocationDelta player (Location "A") (Location "B")] }
      in runAxioms [] emptyWorld [] diff
           `shouldBe` [immediate (Narrate "player \8594 B")]

    it "produces no narration when no location changes" $
      -- Empty diff = nobody moved = no narration.
      runAxioms [] emptyWorld [] emptyDiff `shouldBe` []

    it "narrates each moving character separately" $
      -- Two characters moved in the same tick. Each gets their own
      -- narration effect -- one per LocationDelta in the diff.
      let diff = emptyDiff { diffLocations = [ LocationDelta player (Location "A") (Location "B")
                                             , LocationDelta npc    (Location "X") (Location "Y") ] }
          effects = runAxioms [] emptyWorld [] diff
      in length effects `shouldBe` 2

  -- =========================================================================
  -- dayAdvanceAxiom (system axiom)
  --
  -- Fires when midnight appears in the diff -- specifically when
  -- timeTag 0 (hour 0 = midnight) is in diffWorldTagsAdded. This means
  -- the clock just rolled over to a new day.
  --
  -- When it fires, it advances all calendar counters:
  --   dayNumberTag N   -> N+1  (days since game start, never wraps)
  --   dayOfWeekTag N   -> (N+1) mod 7   (0=Sunday .. 6=Saturday)
  --   lunarPhaseTag N  -> (N+1) mod 29  (0=New Moon, 15=Full Moon)
  --   seasonTag N      -> changes every 91 days (0=Spring .. 3=Winter)
  --
  -- It also narrates significant celestial events:
  --   Season changes produce solstice/equinox narration.
  --   Specific lunar phases (Full=15, New=0, Quarter=7, Third=22)
  --   produce moon narration.
  -- =========================================================================

  describe "dayAdvanceAxiom" $ do
    -- A diff with timeTag 0 in the added world tags, simulating the
    -- clock action having just set the hour to midnight.
    let midnightDiff  = emptyDiff { diffWorldTagsAdded = [timeTag 0] }
        -- A world at day 0 with all calendar counters initialized.
        -- This represents the start of the game: day 0, Sunday,
        -- new moon, spring.
        calendarWorld = emptyWorld { worldTags = orFromList
            [ dayNumberTag 0, dayOfWeekTag 0, lunarPhaseTag 0, seasonTag 0 ] }

    it "does not fire when midnight is absent from the diff" $
      -- The diff is empty, so midnight did not just occur.
      -- The axiom should stay silent even though calendar tags exist.
      runAxioms [] calendarWorld [] emptyDiff `shouldBe` []

    it "advances DayNumber by 1" $
      -- Day 0 + midnight crossing = day 1. DayNumber never wraps --
      -- it's a monotonic counter of days since game start.
      let effects = runAxioms [] calendarWorld [] midnightDiff
      in immediate (AddWorldTag (dayNumberTag 1)) `elem` effects `shouldBe` True

    it "advances DayOfWeek, wrapping at 7" $
      -- DayOfWeek 6 (Saturday) + 1 = 0 (Sunday). The week wraps at 7.
      let w = emptyWorld { worldTags = orFromList
                [ dayNumberTag 6, dayOfWeekTag 6, lunarPhaseTag 6, seasonTag 0 ] }
          effects = runAxioms [] w [] midnightDiff
      in immediate (AddWorldTag (dayOfWeekTag 0)) `elem` effects `shouldBe` True

    it "advances LunarPhase, wrapping at 29" $
      -- LunarPhase 28 + 1 = 0 (new moon again). The lunar cycle is
      -- 29 days, matching the approximate real synodic month.
      let w = emptyWorld { worldTags = orFromList
                [ dayNumberTag 28, dayOfWeekTag 0, lunarPhaseTag 28, seasonTag 0 ] }
          effects = runAxioms [] w [] midnightDiff
      in immediate (AddWorldTag (lunarPhaseTag 0)) `elem` effects `shouldBe` True

    it "adds a new Season tag when the season changes" $
      -- Day 90 -> 91 crosses the Spring(0)->Summer(1) boundary.
      -- Seasons change every 91 days: seasonTag = (dayNumber+1) `div` 91.
      -- 91 `div` 91 = 1 = Summer.
      let w = emptyWorld { worldTags = orFromList
                [ dayNumberTag 90, dayOfWeekTag 6, lunarPhaseTag 6, seasonTag 0 ] }
          effects = runAxioms [] w [] midnightDiff
      in immediate (AddWorldTag (seasonTag 1)) `elem` effects `shouldBe` True

    it "does not add a Season tag when the season is unchanged" $
      -- Day 0 -> 1, still in Spring. No season transition = no
      -- seasonTag effect. The axiom only emits a seasonTag when
      -- the computed season differs from the current one.
      let effects = runAxioms [] calendarWorld [] midnightDiff
      in any (\e -> case effectBody e of
                AddWorldTag (EngineTag (Clock (Season _))) -> True
                _                                  -> False) effects
           `shouldBe` False

    it "narrates the solstice or equinox when the season changes" $
      -- Crossing into Summer triggers the summer solstice narration.
      -- Each season boundary has its own prose: equinoxes for
      -- Spring/Autumn, solstices for Summer/Winter.
      let w = emptyWorld { worldTags = orFromList
                [ dayNumberTag 90, dayOfWeekTag 6, lunarPhaseTag 6, seasonTag 0 ] }
          effects = runAxioms [] w [] midnightDiff
      in immediate (Narrate "The summer solstice. The longest day of the year.")
           `elem` effects `shouldBe` True

    it "narrates a significant moon phase" $
      -- Day 14 -> 15 means lunarPhase advances to 15 = Full Moon.
      -- The axiom narrates named lunar phases:
      --   0 = New Moon, 7 = First Quarter, 15 = Full Moon, 22 = Third Quarter.
      let w = emptyWorld { worldTags = orFromList
                [ dayNumberTag 14, dayOfWeekTag 0, lunarPhaseTag 14, seasonTag 0 ] }
          effects = runAxioms [] w [] midnightDiff
      in immediate (Narrate "The Full Moon.") `elem` effects `shouldBe` True

    it "produces no moon narration for an unremarkable lunar day" $
      -- Day 0 -> 1 means lunarPhase 0 -> 1. Phase 1 has no special
      -- name, so no moon narration should appear in the effects.
      let effects = runAxioms [] calendarWorld [] midnightDiff
          narrations = [ s | e <- effects, Narrate s <- [effectBody e] ]
      in any (\s -> "Moon" `elem` words s) narrations `shouldBe` False
