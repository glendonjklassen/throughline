{-# LANGUAGE DataKinds #-}
module Engine.ChainSpec (spec) where

import           Test.Hspec
import qualified Data.Map.Strict as Map

import           Control.Monad.State (get, modify)
import           Engine.Author.DSL
import           Engine.Core.Effects
import           Engine.Core.World        (setCharacterStat)
import           Engine.CRDT.ORSet
import           GameTypes
import           GameTypes.Types (Action(..))
import           TestFixtures

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

scrollingTag :: Tag
scrollingTag = ScenarioTag (MkScenarioTag "scrolling")

-- Simulate one game tick: run an action, carry forward active effects.
takeTurn :: Action 'Repeatable -> GameWorld -> IO GameWorld
takeTurn action world = fmap snd $ runApp' world $ do
  w0 <- get
  modify (\w -> w { worldClock = LamportClock (lcTick (worldClock w) + 1) (PlayerId "test") })
  remaining <- executeAction w0 action (worldActiveEffects w0)
  modify (\w -> w { worldActiveEffects = remaining })

waitAct :: Action 'Repeatable
waitAct = Action (ActionId "wait") "Wait" Nothing unconditional []

putAwayAct :: Action 'Repeatable
putAwayAct = Action (ActionId "putAway") "Put Away" Nothing unconditional
  [immediate (RemoveWorldTag scrollingTag)]

getInt :: GameWorld -> Int
getInt w = maybe 0 (getRelStat (Capacity Intelligence))
  (Map.lookup Truth (worldGraph w) >>= Map.lookup player)

baseWorld :: GameWorld
baseWorld = emptyWorld
  { worldCharacters = Map.singleton player (Character player "P" [] orEmpty)
  , worldGraph      = setCharacterStat player (Capacity Intelligence) 5 Map.empty
  }

-- ---------------------------------------------------------------------------
-- The chain under test
-- ---------------------------------------------------------------------------

-- A 3-tick conditioned chain: fires intelligence -1 after 3 ticks, but only
-- if scrollingTag is present at each step. Putting away the phone (removing
-- the tag) drops the effect mid-chain.
scrollChain :: Effect
scrollChain = ifItPersists 3 (HasWorldTag scrollingTag) stage3
  where
    stage3 = withNarrative "Your mind feels a little dimmer."
               (immediateWhen (HasWorldTag scrollingTag) (ModifyRelation Truth player (Capacity Intelligence) (-1)))

scrollAct :: Action 'Repeatable
scrollAct = Action (ActionId "scroll") "Scroll" Nothing unconditional
  [ immediate (AddWorldTag scrollingTag)
  , scrollChain
  ]

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "conditioned effect chain" $ do

  it "reduces intelligence after 3 ticks of continuous scrolling" $ do
    w1 <- takeTurn scrollAct  baseWorld  -- starts chain; stage1 enters activeEffects
    w2 <- takeTurn waitAct    w1         -- stage1 → stage2
    w3 <- takeTurn waitAct    w2         -- stage2 → stage3
    w4 <- takeTurn waitAct    w3         -- stage3 fires
    getInt w4 `shouldBe` 4

  it "does not reduce intelligence if phone is put away before chain completes" $ do
    w1 <- takeTurn scrollAct  baseWorld  -- starts chain
    w2 <- takeTurn waitAct    w1         -- stage1 → stage2
    w3 <- takeTurn putAwayAct w2         -- removes scrollingTag; stage2 dropped
    w4 <- takeTurn waitAct    w3
    w5 <- takeTurn waitAct    w4
    getInt w5 `shouldBe` 5

  it "does not reduce intelligence if phone is put away on the very next tick" $ do
    w1 <- takeTurn scrollAct  baseWorld  -- starts chain; stage1 enters activeEffects
    w2 <- takeTurn putAwayAct w1         -- scrollingTag removed; stage2 will fail condition
    w3 <- takeTurn waitAct    w2
    w4 <- takeTurn waitAct    w3
    getInt w4 `shouldBe` 5

  it "reduces intelligence if phone is put away only after the final effect is already active" $ do
    -- After 3 full ticks the final stage is in worldActiveEffects; putting the
    -- phone away on that same tick does not help — worldBefore still has the tag.
    w1 <- takeTurn scrollAct  baseWorld  -- stage1 enters activeEffects
    w2 <- takeTurn waitAct    w1         -- stage1 → stage2
    w3 <- takeTurn waitAct    w2         -- stage2 → stage3 (final) enters activeEffects
    w4 <- takeTurn putAwayAct w3         -- too late: worldBefore still has scrollingTag
    getInt w4 `shouldBe` 4
