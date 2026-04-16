{-# LANGUAGE DataKinds #-}
-- =============================================================================
-- Engine.Sync.CausalitySpec
--
-- The causal frontier answers one question: "did they know about us?"
--
-- Each player stamps a frontier (Map PlayerId EntryId) onto their log entries.
-- When we merge someone else's log, we compare their frontier against our log
-- to determine Provenance: Aware, Unaware, or Stale.
--
-- These tests verify that chain from frontier → provenance → MergeDiff → DSL
-- helpers, all in pure functions with no IO.
-- =============================================================================
module Engine.Sync.CausalitySpec (spec) where

import qualified Data.Map.Strict        as Map
import           Test.Hspec

import           Engine.Author.DSL      (unawareChanges, whenUnaware,
                                         hasUnawareRelation, hasUnawareArrival,
                                         immediate)
import           Engine.Sync.Causality  (computeProvenance, buildMergeDiff,
                                         emptyMergeDiff, runMergeAxioms)
import           Engine.Sync.EventLog   (mkLogEntry)
import           GameTypes
import           TestFixtures

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Build a LogEntry attributed to a player at a given tick with a given
-- frontier. The frontier is what makes this interesting — it records which
-- other players' entries this player had seen at the time they acted.
-- An empty frontier (Map.empty) means "I have never synced with anyone."
ourEntry :: PlayerId -> Int -> CausalFrontier -> LogEntry
ourEntry pid tick =
  mkLogEntry pid (LamportClock tick pid) (ActionId "act") emptyDiff

-- | Same as ourEntry but named for readability when constructing the
-- foreign player's log entries.
foreignEntry :: PlayerId -> Int -> CausalFrontier -> LogEntry
foreignEntry pid tick =
  mkLogEntry pid (LamportClock tick pid) (ActionId "act") emptyDiff

-- | Our player identity. Used as the "local" player in provenance checks.
us :: PlayerId
us = PlayerId "player-us"

-- | The foreign player whose log we're merging.
them :: PlayerId
them = PlayerId "player-them"

-- | Minimal world with one character placed at a location.
-- Used to create before/after worlds that differ by location.
worldWithLocation :: CharId -> Location -> GameWorld
worldWithLocation cid loc = emptyWorld
  { worldLocations = Map.singleton cid loc }

loc1 :: Location
loc1 = Location "location-1"

loc2 :: Location
loc2 = Location "location-2"

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "Engine.Sync.Causality" $ do

  -- =========================================================================
  -- computeProvenance
  --
  -- This is the core question: given a foreign player's frontier and our log,
  -- did they know about us when they acted?
  --
  -- The frontier is a map of PlayerId → last-seen EntryId. We look ourselves
  -- up in their frontier:
  --   - Not present at all → Unaware (they never synced with us)
  --   - Present, and the entry ID >= our latest → Aware (they saw everything)
  --   - Present, but an older entry → Stale (they saw some of our state)
  -- =========================================================================

  describe "computeProvenance" $ do

    it "empty frontier → Unaware (they never synced with us)" $ do
      -- Their frontier is completely empty — they have never synced with
      -- anyone, so they have no record of any other player's entries.
      let theirFrontier = Map.empty
          -- Meanwhile we have one entry in our log, so there IS state
          -- they could have known about but didn't.
          ourLog = [ourEntry us 1 Map.empty]
      computeProvenance ourLog theirFrontier us `shouldBe` Unaware

    it "frontier contains our latest entry → Aware" $ do
      -- We made two moves (tick 1 and tick 2).
      let e1 = ourEntry us 1 Map.empty  -- our first action
          e2 = ourEntry us 2 Map.empty  -- our second (latest) action
          -- Their frontier says "I last saw player-us at entry e2" —
          -- that's our latest, so they were fully caught up.
          theirFrontier = Map.singleton us (entryId e2)
          ourLog = [e1, e2]
      computeProvenance ourLog theirFrontier us `shouldBe` Aware

    it "frontier contains an older entry → Stale" $ do
      -- We made two moves, but they only synced after our first one.
      let e1 = ourEntry us 1 Map.empty  -- they saw this one
          e2 = ourEntry us 2 Map.empty  -- they did NOT see this one
          -- Their frontier records e1 as the last thing they saw from us.
          -- They knew about our tick-1 state but not tick-2.
          theirFrontier = Map.singleton us (entryId e1)
          ourLog = [e1, e2]
      computeProvenance ourLog theirFrontier us `shouldBe` Stale

    it "frontier has other players but not us → Unaware" $ do
      -- They synced with "player-other" but never with us. Having synced
      -- with someone else doesn't give them knowledge of our state.
      let otherPid   = PlayerId "player-other"
          otherEntry = ourEntry otherPid 1 Map.empty
          -- Their frontier has an entry for player-other but nothing for us.
          theirFrontier = Map.singleton otherPid (entryId otherEntry)
          -- We exist and have state they could have known about.
          ourLog = [ourEntry us 1 Map.empty]
      computeProvenance ourLog theirFrontier us `shouldBe` Unaware

    it "empty our log → Aware (nothing to have missed)" $ do
      -- Edge case: we haven't taken any actions yet. Our log is empty.
      -- If their frontier claims to know about us at all, there's literally
      -- nothing they could have missed — you can't be unaware of nothing.
      let theirFrontier = Map.singleton us "1-player-us"
          ourLog = []  -- we have no entries
      computeProvenance ourLog theirFrontier us `shouldBe` Aware

  -- =========================================================================
  -- buildMergeDiff
  --
  -- After merging worlds, buildMergeDiff computes what changed and annotates
  -- each delta with provenance. It diffs the pre-merge and post-merge worlds,
  -- then stamps every delta with the foreign player's provenance.
  --
  -- The provenance comes from the *last* foreign entry's frontier — that's
  -- the most recent information about what they knew.
  -- =========================================================================

  describe "buildMergeDiff" $ do

    it "foreign entries with empty frontier → all deltas Unaware" $ do
      -- Before merge: NPC is at loc1.
      let worldBefore = worldWithLocation npc loc1
          -- After merge: NPC moved to loc2 (the foreign player moved them).
          worldAfter  = worldWithLocation npc loc2
          -- The foreign player's entry has an empty frontier — they never
          -- synced with us, so they had no idea where we thought NPC was.
          fe     = foreignEntry them 1 Map.empty
          -- We have one entry in our log (so there was state to miss).
          ourLog = [ourEntry us 1 Map.empty]
          md     = buildMergeDiff us ourLog [fe] worldBefore worldAfter
      -- Every delta in this merge should be tagged Unaware.
      all (\d -> mdProvenance d == Unaware) (mergeLocations md) `shouldBe` True

    it "foreign entries with our latest in frontier → all deltas Aware" $ do
      -- We have one entry.
      let ourE = ourEntry us 1 Map.empty
          -- The foreign player's frontier explicitly includes our entry —
          -- they synced with us before acting, so they knew our state.
          frontierWithUs = Map.singleton us (entryId ourE)
          -- Their entry at tick 2 carries a frontier that references our entry.
          fe     = foreignEntry them 2 frontierWithUs
          ourLog = [ourE]
          -- Same location change as above, but this time it was informed.
          worldBefore = worldWithLocation npc loc1
          worldAfter  = worldWithLocation npc loc2
          md     = buildMergeDiff us ourLog [fe] worldBefore worldAfter
      -- Every delta should be Aware — they knew what they were doing.
      all (\d -> mdProvenance d == Aware) (mergeLocations md) `shouldBe` True

    it "origin PlayerId matches the foreign player" $ do
      -- The MergeDelta should record WHO caused the change, not just
      -- whether they knew about us.
      let fe     = foreignEntry them 1 Map.empty
          ourLog = [ourEntry us 1 Map.empty]
          worldBefore = worldWithLocation npc loc1
          worldAfter  = worldWithLocation npc loc2
          md     = buildMergeDiff us ourLog [fe] worldBefore worldAfter
      case mergeLocations md of
        []    -> expectationFailure "expected location deltas"
        -- The origin on each delta should be "them", the foreign player.
        (d:_) -> mdOrigin d `shouldBe` them

    it "empty foreign entries → empty MergeDiff" $ do
      -- No foreign entries means nothing was merged. Both worlds are the
      -- same (emptyWorld). The resulting MergeDiff should be completely empty.
      let md = buildMergeDiff us [] [] emptyWorld emptyWorld
      mergeLocations md `shouldBe` []
      mergeStats     md `shouldBe` []
      mergeRelations md `shouldBe` []

    it "multiple entries: last entry's frontier determines provenance" $ do
      -- We have two entries in our log.
      let ourE1  = ourEntry us 1 Map.empty
          ourE2  = ourEntry us 2 Map.empty
          ourLog = [ourE1, ourE2]
          -- Foreign player sent two entries. The first was made before any
          -- sync (empty frontier → would be Unaware on its own).
          fe1 = foreignEntry them 1 Map.empty
          -- But their second entry has a frontier that includes our latest!
          -- They caught up between their first and second action.
          fe2 = foreignEntry them 2 (Map.singleton us (entryId ourE2))
          worldBefore = worldWithLocation npc loc1
          worldAfter  = worldWithLocation npc loc2
          -- buildMergeDiff uses the LAST entry's frontier to determine
          -- provenance for the whole batch, because that's their most
          -- up-to-date knowledge state.
          md = buildMergeDiff us ourLog [fe1, fe2] worldBefore worldAfter
      all (\d -> mdProvenance d == Aware) (mergeLocations md) `shouldBe` True

  -- =========================================================================
  -- runMergeAxioms
  --
  -- Merge axioms fire once per merge (not per tick). They receive the merged
  -- world and the MergeDiff, and produce effects. They run in priority order.
  -- =========================================================================

  describe "runMergeAxioms" $ do

    it "no axioms → no effects" $ do
      runMergeAxioms [] emptyWorld emptyMergeDiff `shouldBe` []

    it "axioms fire in priority order (lowest number first)" $ do
      -- Three axioms registered in scrambled order (10, 1, 5).
      -- Each one adds a world tag as a marker so we can verify the order.
      let tagP1  = ScenarioTag (MkScenarioTag "priority-1")
          tagP5  = ScenarioTag (MkScenarioTag "priority-5")
          tagP10 = ScenarioTag (MkScenarioTag "priority-10")
          axiomAt p tag = MergeAxiom
            { mergeAxiomId       = ScenarioAxiom ("axiom-p" <> show p)
            , mergeAxiomPriority = p
            , mergeAxiomEvaluate = \_world _md -> [immediate (AddWorldTag tag)]
            }
          -- Registered out of order:
          axioms = [ axiomAt (10 :: Int) tagP10
                   , axiomAt (1  :: Int) tagP1
                   , axiomAt (5  :: Int) tagP5
                   ]
          effects = runMergeAxioms axioms emptyWorld emptyMergeDiff
      -- Effects should arrive in priority order: 1, 5, 10.
      map effectBody effects `shouldBe`
        [AddWorldTag tagP1, AddWorldTag tagP5, AddWorldTag tagP10]

    it "axiom receives the MergeDiff and can inspect it" $ do
      -- Build a real MergeDiff with an unaware location delta, then
      -- write an axiom that only fires when it sees unaware changes.
      -- This verifies the diff is actually threaded through to the axiom.
      let tag    = ScenarioTag (MkScenarioTag "unaware-tag")
          -- Foreign player never synced with us (empty frontier).
          fe     = foreignEntry them 1 Map.empty
          ourLog = [ourEntry us 1 Map.empty]
          -- NPC moved from loc1 to loc2 in the merge.
          md     = buildMergeDiff us ourLog [fe]
                     (worldWithLocation npc loc1)
                     (worldWithLocation npc loc2)
          -- This axiom checks: "are there any unaware location deltas?"
          -- If yes, add a tag. If no, stay silent.
          axiom = MergeAxiom
            { mergeAxiomId       = ScenarioAxiom "check-md"
            , mergeAxiomPriority = 0
            , mergeAxiomEvaluate = \_world diff ->
                [ immediate (AddWorldTag tag)
                | not (null (mergeLocations (unawareChanges diff))) ]
            }
      -- The axiom should fire because the diff has unaware deltas.
      map effectBody (runMergeAxioms [axiom] emptyWorld md)
        `shouldBe` [AddWorldTag tag]

    it "silent axiom contributes nothing" $ do
      -- An axiom that always returns [] should not produce phantom effects.
      let axiom = MergeAxiom
            { mergeAxiomId       = ScenarioAxiom "silent"
            , mergeAxiomPriority = 0
            , mergeAxiomEvaluate = \_world _diff -> []
            }
      runMergeAxioms [axiom] emptyWorld emptyMergeDiff `shouldBe` []

  -- =========================================================================
  -- DSL helpers: unawareChanges
  --
  -- Filters a MergeDiff down to only deltas with Unaware provenance.
  -- Aware and Stale deltas are dropped. Scenario authors use this
  -- to react specifically to "things that happened without knowledge of
  -- our timeline."
  -- =========================================================================

  describe "unawareChanges" $ do

    it "keeps only Unaware deltas" $ do
      -- Two location deltas in the same MergeDiff: one from an unaware
      -- source (they didn't know about us) and one from an aware source
      -- (they did know about us).
      let unawareDelta = MergeDelta (LocationDelta npc loc1 loc2) them Unaware
          awareDelta   = MergeDelta (LocationDelta npc loc2 loc1) them Aware
          md       = emptyMergeDiff { mergeLocations = [unawareDelta, awareDelta] }
          -- After filtering, only the unaware delta survives.
          filtered = unawareChanges md
      length (mergeLocations filtered) `shouldBe` 1
      case mergeLocations filtered of
        (d:_) -> mdProvenance d `shouldBe` Unaware
        []    -> expectationFailure "expected one Unaware delta but got none"

    it "empty result when no Unaware deltas exist" $ do
      -- Everything in this merge was from someone who knew about us.
      -- Filtering to Unaware gives us nothing.
      let awareDelta = MergeDelta (LocationDelta npc loc1 loc2) them Aware
          filtered   = unawareChanges (emptyMergeDiff { mergeLocations = [awareDelta] })
      mergeLocations filtered `shouldBe` []

    it "filters out Stale (only Unaware passes)" $ do
      -- Stale means "they knew SOME of our state but not all."
      -- That's NOT the same as Unaware. unawareChanges is strict —
      -- only fully Unaware deltas pass through.
      let mkDelta = MergeDelta (LocationDelta npc loc1 loc2) them
          md = emptyMergeDiff
            { mergeLocations = [ mkDelta Unaware     -- passes
                               , mkDelta Stale   -- filtered out
                               , mkDelta Unaware     -- passes
                               , mkDelta Aware       -- filtered out
                               ] }
      length (mergeLocations (unawareChanges md)) `shouldBe` 2

  -- =========================================================================
  -- DSL helpers: whenUnaware
  --
  -- A convenience gate: "fire these effects if ANYTHING in this merge
  -- came from someone who didn't know about us." Returns the effects
  -- unchanged or returns [], nothing in between.
  -- =========================================================================

  describe "whenUnaware" $ do

    it "returns effects when unaware deltas exist" $ do
      let tag  = ScenarioTag (MkScenarioTag "trigger")
          effs = [immediate (AddWorldTag tag)]
          -- This MergeDiff has one unaware location delta.
          md   = emptyMergeDiff
            { mergeLocations = [MergeDelta (LocationDelta npc loc1 loc2) them Unaware] }
      -- The gate opens — effects pass through unchanged.
      whenUnaware md effs `shouldBe` effs

    it "returns [] when all deltas are Aware" $ do
      let effs = [immediate (AddWorldTag (ScenarioTag (MkScenarioTag "trigger")))]
          -- Everything in this merge was from someone who knew about us.
          md   = emptyMergeDiff
            { mergeLocations = [MergeDelta (LocationDelta npc loc1 loc2) them Aware] }
      -- The gate stays closed — no effects.
      whenUnaware md effs `shouldBe` []

    it "returns [] on empty MergeDiff" $ do
      -- Nothing merged at all — no unaware deltas to trigger on.
      let effs = [immediate (AddWorldTag (ScenarioTag (MkScenarioTag "x")))]
      whenUnaware emptyMergeDiff effs `shouldBe` []

  -- =========================================================================
  -- DSL helpers: hasUnawareRelation
  --
  -- "Did a specific relationship change arrive from someone unaware of us?"
  -- This checks three things simultaneously:
  --   1. The from/to character pair matches
  --   2. The stat type matches
  --   3. The provenance is Unaware
  -- All three must be true for a match.
  -- =========================================================================

  describe "hasUnawareRelation" $ do

    it "matches unaware relation delta for exact from/to/stat" $ do
      -- A trust change from player→npc, caused by "them", and they
      -- were unaware of our state. All three criteria match.
      let rd = RelationDelta
                 { relationDeltaFrom   = player  -- who changed their feeling
                 , relationDeltaTo     = npc     -- toward whom
                 , relationDeltaStat   = Trust   -- what kind of feeling
                 , relationDeltaOld    = 3       -- trust was 3
                 , relationDeltaNew    = 5       -- now it's 5
                 , relationDeltaPlayer = them    -- caused by foreign player
                 }
          md = emptyMergeDiff { mergeRelations = [MergeDelta rd them Unaware] }
      hasUnawareRelation player npc Trust md `shouldBe` True

    it "rejects Aware provenance" $ do
      -- Same relation delta, but they knew about us. Not a blind change.
      let rd = RelationDelta player npc Trust 3 5 them
          md = emptyMergeDiff { mergeRelations = [MergeDelta rd them Aware] }
      hasUnawareRelation player npc Trust md `shouldBe` False

    it "rejects wrong direction" $ do
      -- The delta is player→npc, but we're asking about npc→player.
      -- Direction matters — trust from A to B is different from B to A.
      let rd = RelationDelta player npc Trust 3 5 them
          md = emptyMergeDiff { mergeRelations = [MergeDelta rd them Unaware] }
      hasUnawareRelation npc player Trust md `shouldBe` False

    it "rejects wrong stat type" $ do
      -- The delta is about Trust, but we're asking about Perceived Intelligence.
      -- Same characters, wrong relationship dimension.
      let rd = RelationDelta player npc Trust 3 5 them
          md = emptyMergeDiff { mergeRelations = [MergeDelta rd them Unaware] }
      hasUnawareRelation player npc (Perceived Intelligence) md `shouldBe` False

  -- =========================================================================
  -- DSL helpers: hasUnawareArrival
  --
  -- "Did a character arrive at a specific location from an unaware timeline?"
  -- Checks character ID, destination location, AND Unaware provenance.
  -- Note: it checks the DESTINATION (where they ended up), not the origin.
  -- =========================================================================

  describe "hasUnawareArrival" $ do

    it "matches character arriving at location with Unaware provenance" $ do
      -- NPC moved from loc1 to loc2, and the source was unaware of us.
      let ld = LocationDelta npc loc1 loc2  -- from loc1, arrived at loc2
          md = emptyMergeDiff { mergeLocations = [MergeDelta ld them Unaware] }
      -- We ask: "did NPC arrive at loc2 from an unaware source?" Yes.
      hasUnawareArrival npc loc2 md `shouldBe` True

    it "rejects Aware provenance" $ do
      -- Same movement, but they knew about us. Not a surprise arrival.
      let ld = LocationDelta npc loc1 loc2
          md = emptyMergeDiff { mergeLocations = [MergeDelta ld them Aware] }
      hasUnawareArrival npc loc2 md `shouldBe` False

    it "rejects wrong destination" $ do
      -- NPC arrived at loc2, but we're asking about loc1. loc1 is where
      -- they LEFT from, not where they arrived. Wrong destination.
      let ld = LocationDelta npc loc1 loc2
          md = emptyMergeDiff { mergeLocations = [MergeDelta ld them Unaware] }
      hasUnawareArrival npc loc1 md `shouldBe` False

    it "rejects wrong character" $ do
      -- NPC arrived at loc2, but we're asking about "player".
      -- Right location, wrong character.
      let ld = LocationDelta npc loc1 loc2
          md = emptyMergeDiff { mergeLocations = [MergeDelta ld them Unaware] }
      hasUnawareArrival player loc2 md `shouldBe` False
