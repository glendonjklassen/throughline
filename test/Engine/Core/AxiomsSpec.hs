{-# LANGUAGE DataKinds #-}
-- =============================================================================
-- Engine.Core.AxiomsSpec
--
-- Axioms are world rules that fire automatically after every player action.
--
-- The engine evaluates axioms by giving each one three things:
--   1. The current GameWorld snapshot (post-action)
--   2. The list of available actions this tick
--   3. A WorldDiff describing what changed in this tick
--
-- Critically, all axioms see the SAME post-action snapshot. No axiom's
-- output influences another's within the same tick -- they evaluate in
-- parallel over a frozen world, and their effects are collected afterward.
--
-- System axioms (locationTransition, dayAdvance, etc.) are engine-owned
-- and run in every scenario alongside any scenario-specific axioms.
-- All axioms are sorted by priority (ascending) before evaluation.
--
-- These tests cover diffing, axiom evaluation, and tracing.
-- System axiom tests live in Engine.Core.SystemAxiomsSpec.
-- =============================================================================
module Engine.Core.AxiomsSpec (spec) where

import           Test.Hspec
import qualified Data.Map.Strict as Map

import           Engine.Core.Axioms
import           Engine.Core.Conditions
import           Engine.Author.DSL
import           Engine.Core.World        (setCharacterStat, setRelStat)
import           Engine.CRDT.ORSet
import           GameTypes
import           GameTypes.Types (Action(..))
import           TestFixtures

-- | A scenario-level tag used throughout these tests. ScenarioTag means
-- it belongs to a specific scenario and won't leak into cross-scenario
-- shared state (unlike EngineTag, which is globally visible).
testTag :: Tag
testTag = ScenarioTag (MkScenarioTag "test-tag")

-- | A minimal axiom that always fires and produces the given effects,
-- regardless of world state, available actions, or diff contents.
-- Priority 0 = highest priority (fires first among equals).
-- Used to test axiom plumbing without conditional logic.
mockAxiom :: [Effect] -> Axiom
mockAxiom effects = Axiom
  { axiomId       = ScenarioAxiom "mock"
  , axiomPriority = 0
  , axiomEvaluate = \_ _ _ -> effects
  }

-- | Predicate to distinguish scenario axiom traces from system axiom traces.
-- System axioms (locationTransition, dayAdvance) use SystemAxiom IDs;
-- scenario axioms use ScenarioAxiom IDs. This lets us filter traces to
-- only the axioms we registered in a test.
isScenarioTrace :: AxiomTrace -> Bool
isScenarioTrace (AxiomTrace (ScenarioAxiom _) _ _) = True
isScenarioTrace _                                   = False

spec :: Spec
spec = describe "Engine.Core.Axioms" $ do

  -- =========================================================================
  -- diffWorlds
  --
  -- Compares two GameWorld snapshots and produces a WorldDiff listing
  -- everything that changed: stats, relations, tags (character and world),
  -- and locations. The PlayerId parameter is stamped onto stat and relation
  -- deltas for PNCounter attribution -- this is how the CRDT merge layer
  -- knows which player caused each numeric change.
  -- =========================================================================

  describe "diffWorlds" $ do

    describe "stat changes" $ do
      it "records a stat delta when a stat changes" $
        -- twoCharWorld has player with Intelligence 5 (from TestFixtures).
        -- We bump Intelligence to 8 and diff the before/after snapshots.
        let afterWorld = twoCharWorld
              { worldGraph = setCharacterStat player (Capacity Intelligence) 8 (worldGraph twoCharWorld) }
            -- The PlayerId "test" is stamped onto each delta for attribution.
            -- In production this would be the acting player's real ID.
            diff = diffWorlds (PlayerId "test") twoCharWorld afterWorld
        -- The delta should capture who changed, what stat, old value, new
        -- value, and which player caused the change.
        in diffStats diff `shouldBe`
             [StatDelta player (Capacity Intelligence) 5 8 (PlayerId "test")]

      it "records no stat deltas when nothing changes" $
        -- Diffing a world against itself should produce zero stat deltas.
        diffStats (diffWorlds (PlayerId "test") twoCharWorld twoCharWorld) `shouldBe` []

    describe "relation changes" $ do
      it "records a relation delta when trust changes" $
        -- twoCharWorld has player->npc Trust at 5 (from TestFixtures).
        -- We change it to 9 and diff.
        let afterWorld = twoCharWorld
              { worldGraph = setRelStat player npc Trust 9 (worldGraph twoCharWorld) }
            diff = diffWorlds (PlayerId "test") twoCharWorld afterWorld
        -- RelationDelta captures: from, to, stat type, old value, new value,
        -- and the PlayerId responsible. Direction matters -- player->npc
        -- Trust is a different edge than npc->player Trust.
        in diffRelations diff `shouldBe`
             [RelationDelta player npc Trust 5 9 (PlayerId "test")]

      it "records no relation deltas when nothing changes" $
        diffRelations (diffWorlds (PlayerId "test") twoCharWorld twoCharWorld) `shouldBe` []

    describe "character tag changes" $ do
      it "records added character tags" $
        -- Give the player a tag they didn't have before and diff.
        let afterWorld = twoCharWorld
              { worldCharacters = Map.adjust
                  (\c -> c { charTags = orSingleton testTag }) player (worldCharacters twoCharWorld)
              }
            diff = diffWorlds (PlayerId "test") twoCharWorld afterWorld
        -- The diff records which character gained which tag.
        in diffTagsAdded diff `shouldBe` [(player, testTag)]

      it "records removed character tags" $
        -- Start with the tag present, then diff against the world without it.
        let beforeWorld = twoCharWorld
              { worldCharacters = Map.adjust
                  (\c -> c { charTags = orSingleton testTag }) player (worldCharacters twoCharWorld)
              }
            diff = diffWorlds (PlayerId "test") beforeWorld twoCharWorld
        -- Tag was on the character in "before" but gone in "after".
        in diffTagsRemoved diff `shouldBe` [(player, testTag)]

      it "records no character tag changes when world is unchanged" $
        diffTagsAdded (diffWorlds (PlayerId "test") twoCharWorld twoCharWorld) `shouldBe` []

    describe "world tags" $ do
      it "records added world tags" $
        -- World tags are global (not attached to any character).
        -- Adding testTag to the world-level tag set should appear in the diff.
        let afterWorld = emptyWorld { worldTags = orSingleton testTag }
        in diffWorldTagsAdded (diffWorlds (PlayerId "test") emptyWorld afterWorld) `shouldBe` [testTag]

      it "records removed world tags" $
        -- The reverse: tag was present in "before", gone in "after".
        let beforeWorld = emptyWorld { worldTags = orSingleton testTag }
        in diffWorldTagsRemoved (diffWorlds (PlayerId "test") beforeWorld emptyWorld) `shouldBe` [testTag]

      it "records no tag changes when world is unchanged" $
        -- Diffing emptyWorld against itself should produce the canonical
        -- emptyDiff -- no deltas of any kind.
        diffWorlds (PlayerId "test") emptyWorld emptyWorld `shouldBe` emptyDiff

    describe "location changes" $ do
      it "records a location delta when a character moves" $
        -- Player was at location A, now at location B.
        let w1 = emptyWorld { worldLocations = Map.fromList [(player, Location "A")] }
            w2 = emptyWorld { worldLocations = Map.fromList [(player, Location "B")] }
        -- LocationDelta captures: who moved, where from, where to.
        in diffLocations (diffWorlds (PlayerId "test") w1 w2) `shouldBe` [LocationDelta player (Location "A") (Location "B")]

      it "records no location delta when nothing changes" $
        let world = emptyWorld { worldLocations = Map.fromList [(player, Location "A")] }
        in diffLocations (diffWorlds (PlayerId "test") world world) `shouldBe` []

      it "records deltas for multiple characters moving in the same tick" $
        -- Both player and npc move simultaneously in this tick.
        let w1 = emptyWorld { worldLocations = Map.fromList [(player, Location "A"), (npc, Location "X")] }
            w2 = emptyWorld { worldLocations = Map.fromList [(player, Location "B"), (npc, Location "Y")] }
        -- Each moving character gets its own LocationDelta entry.
        in length (diffLocations (diffWorlds (PlayerId "test") w1 w2)) `shouldBe` 2

  -- =========================================================================
  -- runAxioms
  --
  -- Evaluates all registered axioms against the current world snapshot,
  -- available actions, and world diff. Axioms are sorted by priority
  -- (lowest number = highest priority) before evaluation. Each axiom
  -- sees the same frozen world -- no axiom's output affects another.
  -- System axioms (locationTransition, dayAdvance) always run alongside
  -- any scenario axioms passed in.
  -- =========================================================================

  describe "runAxioms" $ do
    it "returns effects produced by an axiom" $
      -- A single axiom that unconditionally adds a world tag.
      -- runAxioms should collect and return its effect.
      runAxioms [mockAxiom [immediate (AddWorldTag testTag)]] emptyWorld [] emptyDiff
        `shouldBe` [immediate (AddWorldTag testTag)]

    it "returns empty when no axioms are registered" $
      -- With no scenario axioms and an empty diff (so system axioms
      -- have nothing to react to), zero effects should be produced.
      runAxioms [] emptyWorld [] emptyDiff `shouldBe` []

    it "runs axioms in ascending priority order" $
      -- Two axioms registered out of order: priority 10 first, then 1.
      -- The engine should sort them so priority 1 fires before priority 10.
      let first  = Axiom (ScenarioAxiom "first")  1  (\_ _ _ -> [immediate (AddWorldTag (ScenarioTag (MkScenarioTag "first")))])
          second = Axiom (ScenarioAxiom "second") 10 (\_ _ _ -> [immediate (AddWorldTag (ScenarioTag (MkScenarioTag "second")))])
      -- Despite "second" being listed first in the input, its higher
      -- priority number means its effects appear after "first".
      in runAxioms [second, first] emptyWorld [] emptyDiff
           `shouldBe` [ immediate (AddWorldTag (ScenarioTag (MkScenarioTag "first")))
                      , immediate (AddWorldTag (ScenarioTag (MkScenarioTag "second")))
                      ]

    it "axiom can observe world state" $
      -- The axiom reads the world's tag set. If testTag is present,
      -- it fires. This verifies the world snapshot is actually threaded
      -- through to the axiom's evaluate function.
      let worldWithTag = emptyWorld { worldTags = orSingleton testTag }
          observer = Axiom (ScenarioAxiom "observer") 0 $ \world _ _ ->
            [immediate (AddWorldTag (ScenarioTag (MkScenarioTag "observed"))) | orMember testTag (worldTags world)]
      in runAxioms [observer] worldWithTag [] emptyDiff
           `shouldBe` [immediate (AddWorldTag (ScenarioTag (MkScenarioTag "observed")))]

    it "axiom can observe the world diff" $
      -- The axiom inspects the diff (what changed this tick), not the
      -- world itself. This is how axioms react to state transitions
      -- rather than just current state.
      let diff     = emptyDiff { diffWorldTagsAdded = [testTag] }
          observer = Axiom (ScenarioAxiom "observer") 0 $ \_ _ d ->
            [immediate (AddWorldTag (ScenarioTag (MkScenarioTag "saw-addition"))) | testTag `elem` diffWorldTagsAdded d]
      in runAxioms [observer] emptyWorld [] diff
           `shouldBe` [immediate (AddWorldTag (ScenarioTag (MkScenarioTag "saw-addition")))]

    it "axiom can observe available actions" $
      -- The axiom receives the list of actions available to the player
      -- this tick. This lets axioms react to what the player COULD do,
      -- not just what they did.
      let action   = AnyAction (Action (ActionId "a") "A" Nothing unconditional [] :: Action 'Repeatable)
          observer = Axiom (ScenarioAxiom "observer") 0 $ \_ actions _ ->
            [immediate (AddWorldTag (ScenarioTag (MkScenarioTag "saw-action"))) | any (\(AnyAction a) -> actionId a == ActionId "a") actions]
      in runAxioms [observer] emptyWorld [action] emptyDiff
           `shouldBe` [immediate (AddWorldTag (ScenarioTag (MkScenarioTag "saw-action")))]

    it "axiom fires conditionally based on world state" $
      -- This axiom uses checkCondition to gate on HasWorldTag. Since
      -- emptyWorld has no tags, the condition fails and the axiom
      -- produces no effects. This tests the axiom-as-conditional-rule
      -- pattern: world rules that only fire when preconditions are met.
      let gated = Axiom (ScenarioAxiom "gated") 0 $ \world _ _ ->
            [ immediate (AddWorldTag (ScenarioTag (MkScenarioTag "fired")))
            | checkCondition world (HasWorldTag testTag)
            ]
      in runAxioms [gated] emptyWorld [] emptyDiff `shouldBe` []

  -- =========================================================================
  -- runAxiomsTraced
  --
  -- Same evaluation as runAxioms but preserves provenance: each effect is
  -- wrapped in an AxiomTrace that records WHICH axiom produced it. This
  -- powers learning mode, where the UI shows players why things happened
  -- ("dayAdvanceAxiom advanced the calendar because midnight occurred").
  -- =========================================================================

  describe "runAxiomsTraced" $ do
    it "preserves axiom identity in traces" $
      -- The trace should record that ScenarioAxiom "mock" produced the
      -- effect, not just the effect itself.
      let ax = mockAxiom [immediate (AddWorldTag testTag)]
          traces = runAxiomsTraced [ax] emptyWorld [] emptyDiff
          -- Filter to scenario traces only (ignoring system axiom stubs).
          scenarioTraces = filter isScenarioTrace traces
      in map traceAxiomId scenarioTraces `shouldBe` [ScenarioAxiom "mock"]

    it "includes stub axioms with empty effects" $
      -- Even axioms that produce no effects still appear in the trace
      -- as stubs (traceEffects = []). This lets the UI show "these
      -- axioms were evaluated but had nothing to say."
      let traces = runAxiomsTraced [] emptyWorld [] emptyDiff
          stubs  = filter (null . traceEffects) traces
      in length stubs `shouldSatisfy` (> 0)

    it "agrees with runAxioms" $
      -- The traced version and the non-traced version must produce the
      -- same effects. Tracing is observational -- it must not change
      -- what effects are generated.
      let ax = mockAxiom [immediate (AddWorldTag testTag)]
      in runAxioms [ax] emptyWorld [] emptyDiff
           `shouldBe` concatMap traceEffects (runAxiomsTraced [ax] emptyWorld [] emptyDiff)
