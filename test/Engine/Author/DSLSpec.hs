module Engine.Author.DSLSpec (spec) where

import           Test.Hspec
import           Data.List.NonEmpty (NonEmpty(..))

import           Engine.Core.Conditions
import           Engine.Author.DSL
import           Engine.Core.Effects
import           Engine.CRDT.ORSet
import           GameTypes
import           TestFixtures

testTag :: Tag
testTag = ScenarioTag (MkScenarioTag "test-tag")

shop :: Location
shop = Location "shop"

spec :: Spec
spec = describe "Engine.Author.DSL" $ do

  -- -------------------------------------------------------------------------
  -- Effect builders
  -- -------------------------------------------------------------------------

  describe "immediate" $
    it "has lifetime Just 1" $
      effectLifetime (immediate DoNothing) `shouldBe` Just 1

  describe "timed" $
    it "has the specified lifetime" $
      effectLifetime (timed 5 DoNothing) `shouldBe` Just 5

  describe "eternal" $
    it "has lifetime Nothing" $
      effectLifetime (eternal DoNothing) `shouldBe` Nothing

  describe "immediateWhen" $
    it "has lifetime Just 1 and the given condition" $ do
      let e = immediateWhen (HasWorldTag testTag) DoNothing
      effectLifetime e `shouldBe` Just 1
      effectCondition e `shouldBe` HasWorldTag testTag

  describe "timedWhen" $
    it "has the specified lifetime and condition" $ do
      let e = timedWhen 4 (HasWorldTag testTag) DoNothing
      effectLifetime e `shouldBe` Just 4
      effectCondition e `shouldBe` HasWorldTag testTag

  describe "eternalWhen" $
    it "has no lifetime and the given condition" $ do
      let e = eternalWhen (HasWorldTag testTag) DoNothing
      effectLifetime e `shouldBe` Nothing
      effectCondition e `shouldBe` HasWorldTag testTag

  describe "withNarrative" $
    it "sets the effectNarrative field" $
      effectNarrative (withNarrative "some prose" (immediate DoNothing))
        `shouldBe` Just "some prose"

  describe "immediateNarrated" $
    it "has lifetime Just 1 and the given narrative" $ do
      let e = immediateNarrated "prose" DoNothing
      effectLifetime e `shouldBe` Just 1
      effectNarrative e `shouldBe` Just "prose"

  describe "timedNarrated" $
    it "has the specified lifetime and narrative" $ do
      let e = timedNarrated 3 "prose" DoNothing
      effectLifetime e `shouldBe` Just 3
      effectNarrative e `shouldBe` Just "prose"

  describe "eternalNarrated" $
    it "has no lifetime and the given narrative" $ do
      let e = eternalNarrated "prose" DoNothing
      effectLifetime e `shouldBe` Nothing
      effectNarrative e `shouldBe` Just "prose"

  -- -------------------------------------------------------------------------
  -- onceAction
  -- -------------------------------------------------------------------------

  describe "onceAction" $ do
    let action = onceAction (ActionId "test-action") "Do the thing" unconditional []
    it "is available before being taken" $
      checkCondition emptyWorld (actionCondition action) `shouldBe` True
    it "is unavailable after being taken" $
      let worldAfter = emptyWorld { worldTags = orSingleton (actionTaken (ActionId "test-action")) }
      in checkCondition worldAfter (actionCondition action) `shouldBe` False
    it "appends an AddWorldTag effect to mark itself taken" $
      any (\e -> effectBody e == AddWorldTag (actionTaken (ActionId "test-action"))) (actionEffects action)
        `shouldBe` True
    it "sets the actionTaken tag when executed, making it unavailable" $ do
      (_, w) <- runApp' emptyWorld (executeAction emptyWorld action [])
      checkCondition w (actionCondition action) `shouldBe` False

  -- -------------------------------------------------------------------------
  -- togglePair
  -- -------------------------------------------------------------------------

  describe "repeatableAction" $ do
    let action = repeatableAction (ActionId "ra") "Do it" (HasWorldTag testTag) []
    it "uses the given id and label" $ do
      actionId    action `shouldBe` ActionId "ra"
      actionLabel action `shouldBe` "Do it"
    it "uses the given condition directly" $
      actionCondition action `shouldBe` HasWorldTag testTag
    it "appends no hidden effects" $
      length (actionEffects action) `shouldBe` 0

  describe "effectCycle" $ do
    let cycle' = effectCycle DoNothing (AddWorldTag testTag) 2
    it "has the specified lifetime" $
      effectLifetime cycle' `shouldBe` Just 2
    it "stores current and next body in Cycle constructor" $
      case effectBody cycle' of
        Cycle _ DoNothing (AddWorldTag _) -> pure ()
        other -> expectationFailure ("expected Cycle _ DoNothing (AddWorldTag _), got: " <> show other)

  describe "togglePair" $ do
    let lightTag = scenarioTag ("LightOn" :: String)
        (on, off) = togglePair (ActionId "light")
                     lightTag
                     unconditional
                     "Turn on the light"  []
                     "Turn off the light" []
        worldOn = emptyWorld { worldTags = orSingleton lightTag }

    it "activate is available when state is off" $
      checkCondition emptyWorld (actionCondition on) `shouldBe` True
    it "deactivate is unavailable when state is off" $
      checkCondition emptyWorld (actionCondition off) `shouldBe` False
    it "activate is unavailable when state is on" $
      checkCondition worldOn (actionCondition on) `shouldBe` False
    it "deactivate is available when state is on" $
      checkCondition worldOn (actionCondition off) `shouldBe` True

  -- -------------------------------------------------------------------------
  -- ifItPersists
  -- -------------------------------------------------------------------------

  describe "ifItPersists" $ do
    let cond  = HasWorldTag testTag
        final = immediate DoNothing
        chain = ifItPersists 3 cond final

    it "outer effect is unconditional" $
      effectCondition chain `shouldBe` All []

    it "outer effect has lifetime 1" $
      effectLifetime chain `shouldBe` Just 1

    it "outer body is OnExpire whose child carries the condition" $
      case effectBody chain of
        OnExpire DoNothing child -> effectCondition child `shouldBe` cond
        other                    -> expectationFailure ("expected OnExpire DoNothing, got: " <> show other)

  -- -------------------------------------------------------------------------
  -- effectCycleMany
  -- -------------------------------------------------------------------------

  describe "effectCycleMany" $ do
    it "creates a timed effect with the specified lifetime" $
      effectLifetime (effectCycleMany 3 (DoNothing :| [DoNothing])) `shouldBe` Just 3

    it "stores the full rotation in CycleMany constructor" $
      case effectBody (effectCycleMany 2 (DoNothing :| [AddWorldTag testTag])) of
        CycleMany _ (DoNothing :| _) -> pure ()
        other                       -> expectationFailure ("expected CycleMany _ (DoNothing:_), got: " <> show other)

  -- -------------------------------------------------------------------------
  -- dialogueChain
  -- -------------------------------------------------------------------------

  describe "dialogueChain" $ do
    it "produces exactly three effects" $
      length (dialogueChain (ActionId "d") ((player, [], "Hello") :| [])) `shouldBe` 3

    it "first effect adds the ActionTaken world tag" $
      case dialogueChain (ActionId "d") ((player, [], "Hello") :| []) of
        (e:_) -> effectBody e `shouldBe` AddWorldTag (actionTaken (ActionId "d"))
        []    -> error "dialogueChain returned empty list"

    it "second effect adds the DialogueInProgress tag" $
      effectBody (dialogueChain (ActionId "d") ((player, [], "Hello") :| []) !! 1)
        `shouldBe` AddWorldTag dialogueInProgress

    it "third effect is a timed OnExpire wrapping the Say" $
      case effectBody (dialogueChain (ActionId "d") ((player, [], "Hello") :| []) !! 2) of
        OnExpire (Say p _ _) terminal -> do
          p `shouldBe` player
          effectBody terminal `shouldBe` RemoveWorldTag dialogueInProgress
        other -> expectationFailure ("unexpected body: " <> show other)

  -- -------------------------------------------------------------------------
  -- dialogueAction
  -- -------------------------------------------------------------------------

  describe "dialogueAction" $ do
    let da = dialogueAction (ActionId "test-da") "Talk" ((player, [], "Hello") :| [])
    it "is gated on actionTaken" $
      actionCondition da `shouldBe` Not (HasWorldTag (actionTaken (ActionId "test-da")))
    it "effects match dialogueChain output" $
      actionEffects da `shouldBe` dialogueChain (ActionId "test-da") ((player, [], "Hello") :| [])

  -- -------------------------------------------------------------------------
  -- whileItPersists
  -- -------------------------------------------------------------------------

  describe "whileItPersists" $ do
    let cond  = HasWorldTag testTag
        chain = whileItPersists 1 cond DoNothing
    it "outer effect is unconditional" $
      effectCondition chain `shouldBe` All []
    it "final effect carries the condition (fully interruptible)" $
      case effectBody chain of
        OnExpire DoNothing final -> effectCondition final `shouldBe` cond
        other -> expectationFailure ("expected OnExpire DoNothing, got: " <> show other)

  -- -------------------------------------------------------------------------
  -- Relationship helpers
  -- -------------------------------------------------------------------------

  describe "modifyTrust" $
    it "produces a ModifyRelation Trust effect" $
      effectBody (modifyTrust player npc 3)
        `shouldBe` ModifyRelation player npc Trust 3

  describe "trustAbove" $
    it "is RelationAbove from to Trust" $
      trustAbove player npc 5 `shouldBe` RelationAbove player npc Trust 5

  describe "statAbove" $
    it "is RelationAbove Truth for the given character and stat" $
      statAbove player (Capacity Intelligence) 4
        `shouldBe` RelationAbove Truth player (Capacity Intelligence) 4

  describe "modifyStat" $
    it "produces a ModifyRelation Truth effect for the given stat" $
      effectBody (modifyStat player (Capacity Strength) (-1))
        `shouldBe` ModifyRelation Truth player (Capacity Strength) (-1)

  describe "atLocation" $
    it "is AtLocation for the given character and place" $
      atLocation player shop `shouldBe` AtLocation player shop

  -- -------------------------------------------------------------------------
  -- atScene
  -- -------------------------------------------------------------------------

  describe "atScene" $ do
    let factory = anyAction (repeatableAction (ActionId "go") "Go" unconditional [])
    it "adds the location condition" $
      case atScene player shop [factory] of
        [gated] -> anyActionCondition gated `shouldBe` All [AtLocation player shop, unconditional]
        _       -> expectationFailure "expected exactly one gated action"
    it "does not change the action id" $
      case atScene player shop [factory] of
        [gated] -> anyActionId gated `shouldBe` ActionId "go"
        _       -> expectationFailure "expected exactly one gated action"

  -- -------------------------------------------------------------------------
  -- effectsIfTagAdded
  -- -------------------------------------------------------------------------

  describe "effectsIfTagAdded" $ do
    let diff = emptyDiff { diffWorldTagsAdded = [testTag] }
    it "returns the effects when the tag was added this tick" $
      effectsIfTagAdded testTag diff [immediate DoNothing]
        `shouldBe` [immediate DoNothing]
    it "returns empty when the tag was not added" $
      effectsIfTagAdded testTag emptyDiff [immediate DoNothing]
        `shouldBe` []
