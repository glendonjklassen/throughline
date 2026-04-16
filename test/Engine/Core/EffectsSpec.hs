{-# LANGUAGE DataKinds #-}
module Engine.Core.EffectsSpec (spec) where

import           Test.Hspec
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as Map

import           Engine.Author.DSL
import           Engine.Core.World    (getDayNumber, getDayOfWeek, getLunarPhase, getSeason, getWeather)
import           Engine.Core.Effects
import           Engine.CRDT.ORSet
import           GameTypes
import           GameTypes.Types (Action(..))
import           TestFixtures

testTag :: Tag
testTag = ScenarioTag (MkScenarioTag "test-tag")

-- | Wrap an Effect as a LiveEffect for testing, with birthClock at tick 0.
mkLive :: Effect -> LiveEffect
mkLive e = LiveEffect { liveId = initToken e, liveEffect = e, liveBirthClock = LamportClock 0 (PlayerId "test") }

-- | Wrap an Effect as a LiveEffect born at the given clock.
mkLiveAt :: LamportClock -> Effect -> LiveEffect
mkLiveAt clk e = LiveEffect { liveId = initToken e, liveEffect = e, liveBirthClock = clk }

-- | World after one tick increment — birthClock=0 effects are in their first tick.
tick1World :: GameWorld
tick1World = emptyWorld { worldClock = LamportClock 1 (PlayerId "test") }

spec :: Spec
spec = describe "Engine.Core.Effects" $ do

  -- -------------------------------------------------------------------------
  -- executeBody
  -- -------------------------------------------------------------------------

  describe "AddWorldTag" $ do
    it "adds a tag to the world" $ do
      w <- runEffect (AddWorldTag testTag) emptyWorld
      orMember testTag (worldTags w) `shouldBe` True
    it "is idempotent" $ do
      w <- runEffect (AddWorldTag testTag) emptyWorld
      w' <- runEffect (AddWorldTag testTag) w
      length (orToList (worldTags w')) `shouldBe` 1

  describe "RemoveWorldTag" $ do
    it "removes a tag from the world" $ do
      let worldWithTag = emptyWorld { worldTags = orSingleton testTag }
      w <- runEffect (RemoveWorldTag testTag) worldWithTag
      orMember testTag (worldTags w) `shouldBe` False
    it "is a no-op when the tag is absent" $ do
      w <- runEffect (RemoveWorldTag testTag) emptyWorld
      worldTags w `shouldBe` worldTags emptyWorld

  describe "Weather deduplication" $ do
    it "replaces existing weather when a new weather tag is added" $ do
      let worldWithSun = emptyWorld { worldTags = orSingleton (weatherTag (WeatherDesc "sun")) }
      w <- runEffect (AddWorldTag (weatherTag (WeatherDesc "rain"))) worldWithSun
      getWeather w `shouldBe` Just (WeatherDesc "rain")
    it "keeps only one weather tag active at a time" $ do
      let worldWithSun = emptyWorld { worldTags = orSingleton (weatherTag (WeatherDesc "sun")) }
      w <- runEffect (AddWorldTag (weatherTag (WeatherDesc "rain"))) worldWithSun
      length (orToList (worldTags w)) `shouldBe` 1

  describe "DayOfWeek deduplication" $ do
    it "replaces existing day-of-week when a new one is added" $ do
      let w0 = emptyWorld { worldTags = orSingleton (dayOfWeekTag 0) }
      w <- runEffect (AddWorldTag (dayOfWeekTag 3)) w0
      getDayOfWeek w `shouldBe` Just 3
    it "keeps only one DayOfWeek tag active at a time" $ do
      let w0 = emptyWorld { worldTags = orSingleton (dayOfWeekTag 0) }
      w <- runEffect (AddWorldTag (dayOfWeekTag 3)) w0
      length (filter isDayOfWeekTag (orToList (worldTags w))) `shouldBe` 1

  describe "LunarPhase deduplication" $ do
    it "replaces existing lunar phase when a new one is added" $ do
      let w0 = emptyWorld { worldTags = orSingleton (lunarPhaseTag 0) }
      w <- runEffect (AddWorldTag (lunarPhaseTag 15)) w0
      getLunarPhase w `shouldBe` Just 15
    it "keeps only one LunarPhase tag active at a time" $ do
      let w0 = emptyWorld { worldTags = orSingleton (lunarPhaseTag 0) }
      w <- runEffect (AddWorldTag (lunarPhaseTag 15)) w0
      length (filter isLunarPhaseTag (orToList (worldTags w))) `shouldBe` 1

  describe "Season deduplication" $ do
    it "replaces existing season when a new one is added" $ do
      let w0 = emptyWorld { worldTags = orSingleton (seasonTag 0) }
      w <- runEffect (AddWorldTag (seasonTag 2)) w0
      getSeason w `shouldBe` Just 2
    it "keeps only one Season tag active at a time" $ do
      let w0 = emptyWorld { worldTags = orSingleton (seasonTag 0) }
      w <- runEffect (AddWorldTag (seasonTag 2)) w0
      length (filter isSeasonTag (orToList (worldTags w))) `shouldBe` 1

  describe "DayNumber deduplication" $ do
    it "replaces existing day number when a new one is added" $ do
      let w0 = emptyWorld { worldTags = orSingleton (dayNumberTag 5) }
      w <- runEffect (AddWorldTag (dayNumberTag 6)) w0
      getDayNumber w `shouldBe` Just 6
    it "keeps only one DayNumber tag active at a time" $ do
      let w0 = emptyWorld { worldTags = orSingleton (dayNumberTag 5) }
      w <- runEffect (AddWorldTag (dayNumberTag 6)) w0
      length (filter isDayNumberTag (orToList (worldTags w))) `shouldBe` 1

  describe "AddTag" $
    it "adds a tag to a character" $ do
      w <- runEffect (AddTag player testTag) twoCharWorld
      orMember testTag (charTags (worldCharacters w Map.! player)) `shouldBe` True

  describe "RemoveTag" $
    it "removes a tag from a character" $ do
      let worldWithCharTag = twoCharWorld
            { worldCharacters = Map.adjust (\c -> c { charTags = orSingleton testTag }) player
                (worldCharacters twoCharWorld)
            }
      w <- runEffect (RemoveTag player testTag) worldWithCharTag
      orMember testTag (charTags (worldCharacters w Map.! player)) `shouldBe` False

  describe "SetLocation" $ do
    it "sets a character's location" $ do
      let w0 = emptyWorld { worldLocations = Map.fromList [(player, Location "Start")] }
      w <- runEffect (SetLocation player (Location "End")) w0
      Map.lookup player (worldLocations w) `shouldBe` Just (Location "End")
    it "creates a location entry for an untracked character" $ do
      w <- runEffect (SetLocation player (Location "Somewhere")) emptyWorld
      Map.lookup player (worldLocations w) `shouldBe` Just (Location "Somewhere")

  describe "ModifyRelation (truth stats)" $ do
    it "increases Intelligence by n" $ do
      w <- runEffect (ModifyRelation Truth player (Capacity Intelligence) 3) twoCharWorld
      getRelStat (Capacity Intelligence) (worldGraph w Map.! Truth Map.! player) `shouldBe` 8
    it "decreases Intelligence by n" $ do
      w <- runEffect (ModifyRelation Truth player (Capacity Intelligence) (-2)) twoCharWorld
      getRelStat (Capacity Intelligence) (worldGraph w Map.! Truth Map.! player) `shouldBe` 3
    it "modifies Strength" $ do
      w <- runEffect (ModifyRelation Truth player (Capacity Strength) 2) twoCharWorld
      getRelStat (Capacity Strength) (worldGraph w Map.! Truth Map.! player) `shouldBe` 7
    it "modifies Charisma" $ do
      w <- runEffect (ModifyRelation Truth player (Capacity Charisma) (-1)) twoCharWorld
      getRelStat (Capacity Charisma) (worldGraph w Map.! Truth Map.! player) `shouldBe` 4
    it "modifies Understanding" $ do
      w <- runEffect (ModifyRelation Truth player (Capacity Understanding) 1) twoCharWorld
      getRelStat (Capacity Understanding) (worldGraph w Map.! Truth Map.! player) `shouldBe` 6

  describe "ModifyRelation" $ do
    it "adds a delta to an existing relation stat" $ do
      w <- runEffect (ModifyRelation player npc Trust 2) twoCharWorld
      getRelStat Trust (worldGraph w Map.! player Map.! npc) `shouldBe` 7
    it "treats a missing stat as 0" $ do
      w <- runEffect (ModifyRelation player npc (Perceived Intelligence) 3) twoCharWorld
      getRelStat (Perceived Intelligence) (worldGraph w Map.! player Map.! npc) `shouldBe` 3

  describe "Say" $
    it "does not modify world state" $ do
      w <- runEffect (Say player [] "Hello") emptyWorld
      worldTags w `shouldBe` worldTags emptyWorld

  describe "DoNothing" $
    it "does not modify world state" $ do
      w <- runEffect DoNothing emptyWorld
      worldTags w `shouldBe` worldTags emptyWorld

  -- -------------------------------------------------------------------------
  -- tickLive and expireLive
  -- -------------------------------------------------------------------------

  describe "tickLive" $ do
    it "keeps a timed effect with remaining lifetime > 0" $ do
      (remaining, _) <- runApp' tick1World (tickLive (mkLive (timed 3 DoNothing)))
      length remaining `shouldBe` 1

    it "expires a timed effect at lifetime 1 (no child)" $ do
      (remaining, _) <- runApp' tick1World (tickLive (mkLive (timed 1 DoNothing)))
      remaining `shouldBe` []

    it "keeps an eternal effect alive" $ do
      (remaining, _) <- runApp' tick1World (tickLive (mkLive (eternal DoNothing)))
      length remaining `shouldBe` 1

    it "effect lifetime is relative to its birth tick, not tick 0" $ do
      -- born at tick 5 with lifetime 3: alive at tick 7 (remaining=1), expired at tick 8 (remaining=0)
      let birthClock = LamportClock 5 (PlayerId "test")
          fx         = mkLiveAt birthClock (timed 3 DoNothing)
          worldAt7   = emptyWorld { worldClock = LamportClock 7 (PlayerId "test") }
          worldAt8   = emptyWorld { worldClock = LamportClock 8 (PlayerId "test") }
      (alive,   _) <- runApp' worldAt7 (tickLive fx)
      (expired, _) <- runApp' worldAt8 (tickLive fx)
      length alive   `shouldBe` 1
      length expired `shouldBe` 0

    it "spawns the OnExpire child when a timed effect expires" $ do
      let child = eternal DoNothing
      (remaining, _) <- runApp' tick1World (tickLive (mkLive (timed 1 (OnExpire DoNothing child))))
      case remaining of
        [le] -> liveEffect le `shouldBe` child
        _    -> expectationFailure ("expected exactly one child, got: " <> show (length remaining))

  describe "expireLive" $ do
    it "returns a LiveEffect wrapping the child for an OnExpire body" $ do
      let child = eternal DoNothing
      (remaining, _) <- runApp' tick1World (expireLive (mkLive (timed 1 (OnExpire DoNothing child))))
      case remaining of
        [le] -> liveEffect le `shouldBe` child
        _    -> expectationFailure ("expected exactly one child, got: " <> show (length remaining))

    it "returns empty for a non-OnExpire body" $ do
      (remaining, _) <- runApp' tick1World (expireLive (mkLive (timed 1 DoNothing)))
      remaining `shouldBe` []

    it "CycleMany: rotates to the next body on expiry" $ do
      -- A two-body cycle: [b1, b2]. Expiring should spawn a live effect
      -- whose head is b2 (the list has been rotated).
      let b1 = DoNothing
          b2 = AddWorldTag testTag
          e  = timed 1 (CycleMany 1 (b1 :| [b2]))
      (remaining, _) <- runApp' tick1World (expireLive (mkLive e))
      case remaining of
        [le] -> case effectBody (liveEffect le) of
                  CycleMany _ (h :| _) -> h `shouldBe` b2
                  other -> expectationFailure ("expected CycleMany, got: " <> show other)
        _ -> expectationFailure ("expected exactly one effect, got: " <> show (length remaining))

    it "CycleMany: wraps around after the last body" $ do
      -- A two-body cycle that has already rotated to [b2, b1].
      -- Expiring again should bring b1 back to the front.
      let b1 = DoNothing
          b2 = AddWorldTag testTag
          e  = timed 1 (CycleMany 1 (b2 :| [b1]))
      (remaining, _) <- runApp' tick1World (expireLive (mkLive e))
      case remaining of
        [le] -> case effectBody (liveEffect le) of
                  CycleMany _ (h :| _) -> h `shouldBe` b1
                  other -> expectationFailure ("expected CycleMany, got: " <> show other)
        _ -> expectationFailure ("expected exactly one effect, got: " <> show (length remaining))

    it "Cycle: alternates to the second body on expiry" $ do
      -- Cycle e1 e2: expiring should spawn a new effect where e2 is now current.
      let b1 = DoNothing
          b2 = AddWorldTag testTag
          e  = timed 1 (Cycle 1 b1 b2)
      (remaining, _) <- runApp' tick1World (expireLive (mkLive e))
      case remaining of
        [le] -> case effectBody (liveEffect le) of
                  Cycle _ current _ -> current `shouldBe` b2
                  other -> expectationFailure ("expected Cycle, got: " <> show other)
        _ -> expectationFailure ("expected exactly one effect, got: " <> show (length remaining))

    it "Cycle: alternates back to the first body on the next expiry" $ do
      -- After one rotation (e2 is now current), expiring again brings e1 back.
      let b1 = DoNothing
          b2 = AddWorldTag testTag
          e  = timed 1 (Cycle 1 b2 b1)
      (remaining, _) <- runApp' tick1World (expireLive (mkLive e))
      case remaining of
        [le] -> case effectBody (liveEffect le) of
                  Cycle _ current _ -> current `shouldBe` b1
                  other -> expectationFailure ("expected Cycle, got: " <> show other)
        _ -> expectationFailure ("expected exactly one effect, got: " <> show (length remaining))

    it "CycleMany: the new LiveEffect is born at the current clock" $ do
      -- The spawned effect's birthClock must equal the world clock at expiry,
      -- so its countdown starts fresh from that tick.
      let e       = timed 1 (CycleMany 1 (DoNothing :| [DoNothing]))
          worldAt5 = emptyWorld { worldClock = LamportClock 5 (PlayerId "test") }
      (remaining, _) <- runApp' worldAt5 (expireLive (mkLive e))
      case remaining of
        [le] -> lcTick (liveBirthClock le) `shouldBe` 5
        _    -> expectationFailure ("expected exactly one effect, got: " <> show (length remaining))

  -- -------------------------------------------------------------------------
  -- executeEffect
  -- -------------------------------------------------------------------------

  describe "executeEffect" $ do
    it "executes the inner body of an OnExpire effect" $ do
      let e = timed 2 (OnExpire (AddWorldTag testTag) (eternal DoNothing))
      (_, w) <- runApp' tick1World (executeEffect (mkLive e))
      orMember testTag (worldTags w) `shouldBe` True

    it "persists a timed effect with remaining lifetime > 0" $ do
      let e = timed 3 DoNothing
      (remaining, _) <- runApp' tick1World (executeEffect (mkLive e))
      length remaining `shouldBe` 1

    it "returns empty when a timed effect with no child expires" $ do
      let e = timed 1 DoNothing
      (remaining, _) <- runApp' tick1World (executeEffect (mkLive e))
      remaining `shouldBe` []

  -- -------------------------------------------------------------------------
  -- executeAction
  -- -------------------------------------------------------------------------

  describe "executeStep" $ do
    it "advances worldClock tick by 1" $ do
      let act = Action (ActionId "a") "A" Nothing unconditional [] :: Action 'Repeatable
      (_, w) <- runApp' emptyWorld (executeStep act)
      lcTick (worldClock w) `shouldBe` lcTick (worldClock emptyWorld) + 1

  describe "executeAction" $ do
    it "executes the effects of an action" $ do
      let action = Action (ActionId "a") "A" Nothing unconditional [immediate (AddWorldTag testTag)] :: Action 'Repeatable
      (_, w) <- runApp' emptyWorld (executeAction emptyWorld action [])
      orMember testTag (worldTags w) `shouldBe` True

    it "skips effects whose conditions fail" $ do
      let gated = immediateWhen (HasWorldTag (ScenarioTag (MkScenarioTag "gate"))) (AddWorldTag testTag)
          action = Action (ActionId "a") "A" Nothing unconditional [gated] :: Action 'Repeatable
      (_, w) <- runApp' emptyWorld (executeAction emptyWorld action [])
      orMember testTag (worldTags w) `shouldBe` False

    it "also executes effects from the world's active effect list" $ do
      let worldEffect = immediate (AddWorldTag testTag)
          action      = Action (ActionId "a") "A" Nothing unconditional [] :: Action 'Repeatable
      (_, w) <- runApp' emptyWorld (executeAction emptyWorld action [mkLive worldEffect])
      orMember testTag (worldTags w) `shouldBe` True

    it "drops an eternalWhen effect when its condition fails" $ do
      let gated  = eternalWhen (HasWorldTag (ScenarioTag (MkScenarioTag "gate"))) (AddWorldTag testTag)
          action = Action (ActionId "a") "A" Nothing unconditional [] :: Action 'Repeatable
      (remaining, _) <- runApp' emptyWorld (executeAction emptyWorld action [mkLive gated])
      remaining `shouldBe` []

    it "keeps an eternalWhen effect when its condition holds" $ do
      let gateTag = ScenarioTag (MkScenarioTag "gate")
          gated   = eternalWhen (HasWorldTag gateTag) DoNothing
          world   = emptyWorld { worldTags = orSingleton gateTag }
          action  = Action (ActionId "a") "A" Nothing unconditional [] :: Action 'Repeatable
      (remaining, _) <- runApp' world (executeAction world action [mkLive gated])
      length remaining `shouldBe` 1

    it "condition checks use the world snapshot before the action started" $ do
      -- effect1 adds a gate tag; effect2 is gated on that same tag.
      -- Both conditions are checked against worldBefore, so effect2 should be
      -- dropped even though effect1 would have added the tag.
      let gateTag = ScenarioTag (MkScenarioTag "gate")
          addGate = immediate (AddWorldTag gateTag)
          gated   = immediateWhen (HasWorldTag gateTag) (AddWorldTag testTag)
          action  = Action (ActionId "a") "A" Nothing unconditional [addGate, gated] :: Action 'Repeatable
      (_, w) <- runApp' emptyWorld (executeAction emptyWorld action [])
      orMember testTag (worldTags w) `shouldBe` False
