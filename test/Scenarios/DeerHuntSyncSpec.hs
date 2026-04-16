-- =============================================================================
-- Scenarios.DeerHuntSyncSpec
--
-- Multiplayer merge integration tests for the deer hunt scenario.
-- Two hunters run independent sessions and merge. Tests cover:
--   1. Hunters who miss each other (different locations, no interaction)
--   2. Hunters at the same location (wave action available after merge)
--   3. One hunter kills the deer, the other inherits via merge
--   4. Both hunters at the deer's location, one shoots
--
-- Both merge paths are tested:
--   - Snapshot merge: CRDT union of two final GameWorlds (mergeWorlds)
--   - Log merge: merge event logs (mergeLogs), replay from base (replayFrom)
-- =============================================================================
module Scenarios.DeerHuntSyncSpec (spec) where

import           Data.List                  (isInfixOf)
import qualified Data.Map.Strict           as Map
import           Test.Hspec

import           Engine.Author.Scene       (edgeActionId)
import           Engine.Core.Axioms.Merge  (systemMergeAxioms)
import           Engine.Core.Conditions    (checkCondition)
import           Engine.Core.Effects       (mergeWorlds)
import           Engine.Core.World         (setCharacterStat)
import           Engine.CRDT.ORSet         (orFromList, orEmpty)
import           Engine.Headless           (runHeadlessScript)
import           Engine.Runtime            (RuntimeUI(..), offerMerge)
import           Engine.Sync.Causality     (buildMergeDiff, runMergeAxioms)
import           Engine.Sync.EventLog      (mergeLogs, nullLogStore, replayFrom)
import           GameTypes
import           MonadStack                (AppError)
import           Scenarios.DeerHunt        (deerHunt)
import           Scenarios.DeerHunt.Axioms (hunterArrivalMergeAxiom)
import           Scenarios.DeerHunt.Constants
import           Scenarios.DeerHunt.Locations
import           Scenarios.DeerHunt.Probability (doesShotHit)

-- ---------------------------------------------------------------------------
-- Player identities
-- ---------------------------------------------------------------------------

pidA, pidB :: PlayerId
pidA = PlayerId "hunter-alpha"
pidB = PlayerId "hunter-bravo"

youA, youB :: CharId
youA = Named (take 12 "hunter-alpha")  -- Named "hunter-alph"
youB = Named (take 12 "hunter-bravo")  -- Named "hunter-brav"

-- ---------------------------------------------------------------------------
-- Walk scripts
-- ---------------------------------------------------------------------------

-- | Walk from truckNorth to stubbleRows (north field): 3 steps.
walkToStubble :: [ActionId]
walkToStubble =
  [ edgeActionId truckNorth ditchNorth
  , edgeActionId ditchNorth nFieldEdge
  , edgeActionId nFieldEdge stubbleRows
  ]

-- | Walk from truckNorth to brushPile (bush edge): 5 steps.
walkToBrush :: [ActionId]
walkToBrush =
  [ edgeActionId truckNorth ditchNorth
  , edgeActionId ditchNorth nFieldEdge
  , edgeActionId nFieldEdge drainageDitch
  , edgeActionId drainageDitch thinPoplars
  , edgeActionId thinPoplars brushPile
  ]

-- ---------------------------------------------------------------------------
-- Merge helpers (same pattern as CoLocationSpec)
-- ---------------------------------------------------------------------------

-- | Snapshot merge: run both hunters independently, CRDT-merge final worlds.
snapshotMerge :: [ActionId] -> [ActionId] -> IO GameWorld
snapshotMerge scriptA scriptB = do
  Right (worldA, _) <- runHeadlessScript (deerHunt 0) pidA scriptA
  Right (worldB, _) <- runHeadlessScript (deerHunt 0) pidB scriptB
  pure (mergeWorlds worldA worldB)

-- | Log merge: merge initial worlds as base, merge logs, replay.
logMerge :: [ActionId] -> [ActionId] -> IO (Either AppError GameWorld)
logMerge scriptA scriptB = do
  let scenA = deerHunt 0 youA
      scenB = deerHunt 0 youB
      base  = mergeWorlds (scenarioInitial scenA) (scenarioInitial scenB)
  Right (_, logA) <- runHeadlessScript (deerHunt 0) pidA scriptA
  Right (_, logB) <- runHeadlessScript (deerHunt 0) pidB scriptB
  let (_, divergent) = mergeLogs logA logB
  replayFrom scenA base divergent

-- | Check if the wave action is available for a given hunter in the world.
canWave :: CharId -> GameWorld -> Bool
canWave you world =
  let scen    = deerHunt 0 you
      actions = scenarioActions scen
      waveActions = filter (\a -> anyActionId a == ActionId "wave") actions
  in any (checkCondition world . anyActionCondition) waveActions

-- ---------------------------------------------------------------------------
-- Pinned-deer scenario: remove deer movement and spook axioms so the deer
-- stays at stubbleRows and doesn't bolt when a hunter arrives.
-- ---------------------------------------------------------------------------

pinnedDeerHunt :: CharId -> Scenario
pinnedDeerHunt you =
  let base = deerHunt 0 you
      pinned = filter (\a -> axiomId a `notElem`
                  [ ScenarioAxiom "deerMovement"
                  , ScenarioAxiom "spook"
                  ]) (scenarioAxioms base)
      -- Pin the deer at stubbleRows so walk scripts that navigate there find it.
      w0 = scenarioInitial base
      w0pinned = w0 { worldLocations = Map.insert deer stubbleRows (worldLocations w0) }
  in base { scenarioAxioms = pinned, scenarioInitial = w0pinned }

-- | Like snapshotMerge but with pinned deer.
pinnedSnapshotMerge :: [ActionId] -> [ActionId] -> IO GameWorld
pinnedSnapshotMerge scriptA scriptB = do
  Right (worldA, _) <- runHeadlessScript pinnedDeerHunt pidA scriptA
  Right (worldB, _) <- runHeadlessScript pinnedDeerHunt pidB scriptB
  pure (mergeWorlds worldA worldB)

-- | Like logMerge but with pinned deer.
pinnedLogMerge :: [ActionId] -> [ActionId] -> IO (Either AppError GameWorld)
pinnedLogMerge scriptA scriptB = do
  let scenA = pinnedDeerHunt youA
      scenB = pinnedDeerHunt youB
      base  = mergeWorlds (scenarioInitial scenA) (scenarioInitial scenB)
  Right (_, logA) <- runHeadlessScript pinnedDeerHunt pidA scriptA
  Right (_, logB) <- runHeadlessScript pinnedDeerHunt pidB scriptB
  let (_, divergent) = mergeLogs logA logB
  replayFrom scenA base divergent

-- | Find a clock tick where the shot hits for a given player.
-- The shot is evaluated at tick + offset (accounting for steps taken).
findHitTick :: CharId -> Int -> Int
findHitTick you offset =
  case [ t | t <- [0..5000]
           , let w = GameWorld
                     { worldCharacters = Map.fromList
                         [ (you,  Character you  "You"      [] orEmpty)
                         , (deer, Character deer "The Deer" [] orEmpty)
                         ]
                     , worldGraph
                         = setCharacterStat you  (Capacity Intelligence)  5
                         . setCharacterStat you  (Capacity Strength)      6
                         . setCharacterStat you  (Capacity Understanding) 2
                         . setCharacterStat you  (Capacity Hunger)        8
                         . setCharacterStat deer (Capacity Intelligence)  3
                         . setCharacterStat deer (Capacity Strength)      6
                         $ Map.empty
                     , worldLocations = Map.fromList [(you, stubbleRows), (deer, stubbleRows)]
                     , worldActiveEffects = []
                     , worldClock = LamportClock (t + offset) (PlayerId "test")
                     , worldTags = orFromList
                         [ deerSpotted
                         , weatherTag (WeatherDesc "Clear and Cold")
                         , seasonTag 3, dayOfWeekTag 5, lunarPhaseTag 0
                         , dayNumberTag 0, timeTag 10
                         ]
                     , worldLocationGraph = emptyLocationGraph
                     , worldSeed = 0
                     }
           , doesShotHit w you
           ] of
    (t:_) -> t
    []    -> error "findHitTick: no tick in [0..5000] produced a hit"

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "DeerHunt multiplayer merge" $ do

  -- =========================================================================
  -- Test 1: Two hunters miss each other
  --
  -- Player A walks to the north field (stubbleRows).
  -- Player B walks to the bush edge (brushPile).
  -- After merge: both exist, no co-location, wave unavailable.
  -- =========================================================================

  describe "two hunters miss each other" $ do

    it "snapshot merge: both players have locations, wave unavailable" $ do
      w <- snapshotMerge walkToStubble walkToBrush
      -- Both hunters exist in the merged world
      Map.member youA (worldLocations w) `shouldBe` True
      Map.member youB (worldLocations w) `shouldBe` True
      -- Deer exists
      Map.member deer (worldLocations w) `shouldBe` True
      -- No terminal condition
      checkCondition w (HasWorldTag deerKilled) `shouldBe` False
      -- Wave not available (different locations)
      canWave youA w `shouldBe` False

    it "log merge: both players have locations" $ do
      result <- logMerge walkToStubble walkToBrush
      case result of
        Left err -> expectationFailure ("merge failed: " <> show err)
        Right w -> do
          Map.member youA (worldLocations w) `shouldBe` True
          Map.member youB (worldLocations w) `shouldBe` True

    it "snapshot and log merge agree on locations" $ do
      sw     <- snapshotMerge walkToStubble walkToBrush
      result <- logMerge      walkToStubble walkToBrush
      case result of
        Left err -> expectationFailure ("merge failed: " <> show err)
        Right lw -> do
          Map.lookup youA (worldLocations sw) `shouldBe` Map.lookup youA (worldLocations lw)
          Map.lookup youB (worldLocations sw) `shouldBe` Map.lookup youB (worldLocations lw)

  -- =========================================================================
  -- Test 2: Two hunters at the same location — wave available
  --
  -- Both walk to stubbleRows. After merge they're co-located and the wave
  -- action should be available.
  -- =========================================================================

  describe "two hunters at same location after merge" $ do

    it "snapshot merge: both at stubbleRows, wave available" $ do
      w <- pinnedSnapshotMerge walkToStubble walkToStubble
      -- Both at the same location
      Map.lookup youA (worldLocations w) `shouldBe` Just stubbleRows
      Map.lookup youB (worldLocations w) `shouldBe` Just stubbleRows
      -- Wave available for player A (sees player B)
      canWave youA w `shouldBe` True

    it "log merge: wave available" $ do
      result <- pinnedLogMerge walkToStubble walkToStubble
      case result of
        Left err -> expectationFailure ("merge failed: " <> show err)
        Right w  -> canWave youA w `shouldBe` True

    it "snapshot and log merge agree on wave availability" $ do
      sw     <- pinnedSnapshotMerge walkToStubble walkToStubble
      result <- pinnedLogMerge      walkToStubble walkToStubble
      case result of
        Left err -> expectationFailure ("merge failed: " <> show err)
        Right lw -> canWave youA sw `shouldBe` canWave youA lw

  -- =========================================================================
  -- Test 3: One hunter kills the deer, other inherits
  --
  -- Player A walks to deer, looks, shoots (deterministic hit).
  -- Player B walks to the bush edge (never near the deer).
  -- After merge: deerKilled propagates, terminal condition fires.
  -- =========================================================================

  describe "one hunter kills deer, other inherits" $ do

    -- Player A's kill script: walk to stubbleRows (3 steps) + look (1) + shoot.
    -- Need a tick where the shot hits at offset +4 (3 walks + 1 look).
    let tick = findHitTick youA 4
        killScript = walkToStubble ++ [ActionId "look", ActionId "takeTheShot"]
        bystander  = walkToBrush

        -- Override initial clock tick for deterministic shot.
        pinnedKillHunt :: CharId -> Scenario
        pinnedKillHunt you =
          let base = pinnedDeerHunt you
              w0   = scenarioInitial base
          in base { scenarioInitial = w0 { worldClock = LamportClock tick (PlayerId "test") } }

        killSnapshotMerge sA sB = do
          Right (wA, _) <- runHeadlessScript pinnedKillHunt pidA sA
          Right (wB, _) <- runHeadlessScript pinnedKillHunt pidB sB
          pure (mergeWorlds wA wB)

        killLogMerge sA sB = do
          let scenA = pinnedKillHunt youA
              scenB = pinnedKillHunt youB
              base  = mergeWorlds (scenarioInitial scenA) (scenarioInitial scenB)
          Right (_, logA) <- runHeadlessScript pinnedKillHunt pidA sA
          Right (_, logB) <- runHeadlessScript pinnedKillHunt pidB sB
          let (_, divergent) = mergeLogs logA logB
          replayFrom scenA base divergent

    it "snapshot merge: deerKilled propagates to merged world" $ do
      w <- killSnapshotMerge killScript bystander
      -- Both players exist
      Map.member youA (worldLocations w) `shouldBe` True
      Map.member youB (worldLocations w) `shouldBe` True
      -- The kill propagated
      checkCondition w (HasWorldTag deerKilled) `shouldBe` True

    it "log merge: deerKilled propagates" $ do
      result <- killLogMerge killScript bystander
      case result of
        Left err -> expectationFailure ("merge failed: " <> show err)
        Right w  -> checkCondition w (HasWorldTag deerKilled) `shouldBe` True

    it "terminal condition fires on merged world" $ do
      w <- killSnapshotMerge killScript bystander
      let terminal = scenarioTerminal (deerHunt 0 youB)
      checkCondition w terminal `shouldBe` True

  -- =========================================================================
  -- Test 4: Both hunters at the deer, one shoots
  --
  -- Both walk to stubbleRows (where the deer is pinned). Player A looks
  -- and shoots. After merge: deerKilled, both co-located.
  -- =========================================================================

  describe "co-located kill — both hunters near the deer" $ do

    let tick = findHitTick youA 4
        killScript   = walkToStubble ++ [ActionId "look", ActionId "takeTheShot"]
        nearbyScript = walkToStubble ++ [ActionId "look"]

        pinnedKillHunt :: CharId -> Scenario
        pinnedKillHunt you =
          let base = pinnedDeerHunt you
              w0   = scenarioInitial base
          in base { scenarioInitial = w0 { worldClock = LamportClock tick (PlayerId "test") } }

        colocKillSnapshotMerge sA sB = do
          Right (wA, _) <- runHeadlessScript pinnedKillHunt pidA sA
          Right (wB, _) <- runHeadlessScript pinnedKillHunt pidB sB
          pure (mergeWorlds wA wB)

        colocKillLogMerge sA sB = do
          let scenA = pinnedKillHunt youA
              scenB = pinnedKillHunt youB
              base  = mergeWorlds (scenarioInitial scenA) (scenarioInitial scenB)
          Right (_, logA) <- runHeadlessScript pinnedKillHunt pidA sA
          Right (_, logB) <- runHeadlessScript pinnedKillHunt pidB sB
          let (_, divergent) = mergeLogs logA logB
          replayFrom scenA base divergent

    it "snapshot merge: both co-located, deer killed" $ do
      w <- colocKillSnapshotMerge killScript nearbyScript
      Map.lookup youA (worldLocations w) `shouldBe` Just stubbleRows
      Map.lookup youB (worldLocations w) `shouldBe` Just stubbleRows
      checkCondition w (HasWorldTag deerKilled) `shouldBe` True

    it "log merge: deer killed with both present" $ do
      result <- colocKillLogMerge killScript nearbyScript
      case result of
        Left err -> expectationFailure ("merge failed: " <> show err)
        Right w  -> do
          checkCondition w (HasWorldTag deerKilled) `shouldBe` True
          Map.lookup youA (worldLocations w) `shouldBe` Just stubbleRows
          Map.lookup youB (worldLocations w) `shouldBe` Just stubbleRows

    it "snapshot and log merge agree" $ do
      sw     <- colocKillSnapshotMerge killScript nearbyScript
      result <- colocKillLogMerge      killScript nearbyScript
      case result of
        Left err -> expectationFailure ("merge failed: " <> show err)
        Right lw -> do
          checkCondition sw (HasWorldTag deerKilled) `shouldBe` checkCondition lw (HasWorldTag deerKilled)
          Map.lookup youA (worldLocations sw) `shouldBe` Map.lookup youA (worldLocations lw)
          Map.lookup youB (worldLocations sw) `shouldBe` Map.lookup youB (worldLocations lw)

  -- =========================================================================
  -- Test 5: offerMerge with a current snapshot (no trailing entries)
  --
  -- Player A plays, exits, saves a snapshot at the end of their log.
  -- Player B starts and calls offerMerge. Despite snapOffset == length logA
  -- producing zero entries to replay, the CRDT merge must still apply —
  -- player A's character and location must appear in B's world.
  -- =========================================================================

  describe "offerMerge with current snapshot" $ do

    let autoAcceptUI :: RuntimeUI
        autoAcceptUI = RuntimeUI
          { uiSetup       = pure ()
          , uiTeardown    = pure ()
          , uiGameLoop    = \_ w -> pure (Right ((), w))
          , uiOnEnd       = \_ -> pure ()
          , uiOnError     = \_ -> pure ()
          , uiOnWarn      = \_ -> pure ()
          , uiPromptMerge = \_ _ -> pure True
          }

    it "current snapshot merges foreign player's state" $ do
      -- Player A plays: walk to stubbleRows
      Right (worldA, logA) <- runHeadlessScript pinnedDeerHunt pidA walkToStubble
      -- A exits: snapshot covers entire log (no trailing entries)
      let snapA = Snapshot worldA (length logA) [] [] []

      -- Player B starts fresh — knows nothing about A
      let scenB  = pinnedDeerHunt youB
          worldB = scenarioInitial scenB
          store  = nullLogStore { lsForeignLogs = pure [(pidA, logA, Just snapA)] }

      -- offerMerge should apply the CRDT merge even with zero entries
      (merged, _, _, _) <- offerMerge autoAcceptUI store scenB pidB [] worldB

      -- Player A's character and location must be present
      Map.member youA (worldLocations merged) `shouldBe` True
      Map.lookup youA (worldLocations merged) `shouldBe` Just stubbleRows

    it "current snapshot without acceptance leaves world unchanged" $ do
      Right (worldA, logA) <- runHeadlessScript pinnedDeerHunt pidA walkToStubble
      let snapA = Snapshot worldA (length logA) [] [] []

      let scenB  = pinnedDeerHunt youB
          worldB = scenarioInitial scenB
          store  = nullLogStore { lsForeignLogs = pure [(pidA, logA, Just snapA)] }
          rejectUI = autoAcceptUI { uiPromptMerge = \_ _ -> pure False }

      (merged, _, _, _) <- offerMerge rejectUI store scenB pidB [] worldB

      -- Rejected: A's character should NOT be present
      Map.member youA (worldLocations merged) `shouldBe` False

  -- =========================================================================
  -- Test 6: Merge axiom — stranger arrival
  --
  -- Tests both "different locations" (system axiom only) and
  -- "same location" (system + scenario axiom) merge cases.
  -- =========================================================================

  describe "merge axiom fires on stranger arrival" $ do

    it "MergeDiff contains Unaware location deltas" $ do
      Right (worldA, logA) <- runHeadlessScript pinnedDeerHunt pidA walkToStubble
      Right (_worldB, logB) <- runHeadlessScript pinnedDeerHunt pidB walkToBrush

      let scenA = pinnedDeerHunt youA
          scenB = pinnedDeerHunt youB
          base  = mergeWorlds (scenarioInitial scenA) (scenarioInitial scenB)
          (_, divergent) = mergeLogs logA logB

      Right mergedWorld <- replayFrom scenA base divergent

      let md = buildMergeDiff pidA logA divergent worldA mergedWorld

      -- There should be Unaware location deltas (B arrived somewhere unknown to A)
      let unawareLocs = filter (\d -> mdProvenance d == Unaware) (mergeLocations md)
      unawareLocs `shouldSatisfy` (not . null)

    it "system axiom fires when hunters merge at different locations" $ do
      Right (worldA, logA) <- runHeadlessScript pinnedDeerHunt pidA walkToStubble
      Right (_worldB, logB) <- runHeadlessScript pinnedDeerHunt pidB walkToBrush

      let scenA = pinnedDeerHunt youA
          scenB = pinnedDeerHunt youB
          base  = mergeWorlds (scenarioInitial scenA) (scenarioInitial scenB)
          (_, divergent) = mergeLogs logA logB

      Right mergedWorld <- replayFrom scenA base divergent

      let md = buildMergeDiff pidA logA divergent worldA mergedWorld
          effects = runMergeAxioms systemMergeAxioms mergedWorld md

      -- System axiom fires (generic prose)
      effects `shouldSatisfy` (not . null)

    it "scenario axiom does NOT fire when hunters are at different locations" $ do
      Right (worldA, logA) <- runHeadlessScript pinnedDeerHunt pidA walkToStubble
      Right (_worldB, logB) <- runHeadlessScript pinnedDeerHunt pidB walkToBrush

      let scenA = pinnedDeerHunt youA
          scenB = pinnedDeerHunt youB
          base  = mergeWorlds (scenarioInitial scenA) (scenarioInitial scenB)
          (_, divergent) = mergeLogs logA logB

      Right mergedWorld <- replayFrom scenA base divergent

      let md = buildMergeDiff pidA logA divergent worldA mergedWorld
          effects = runMergeAxioms [hunterArrivalMergeAxiom youA] mergedWorld md

      -- Scenario axiom should NOT fire — they're on different sections
      effects `shouldBe` []

    it "scenario axiom fires when hunters merge at the SAME location" $ do
      Right (worldA, logA) <- runHeadlessScript pinnedDeerHunt pidA walkToStubble
      Right (_worldB, logB) <- runHeadlessScript pinnedDeerHunt pidB walkToStubble

      let scenA = pinnedDeerHunt youA
          scenB = pinnedDeerHunt youB
          base  = mergeWorlds (scenarioInitial scenA) (scenarioInitial scenB)
          (_, divergent) = mergeLogs logA logB

      Right mergedWorld <- replayFrom scenA base divergent

      let md = buildMergeDiff pidA logA divergent worldA mergedWorld
          allMergeAxioms = systemMergeAxioms ++ [hunterArrivalMergeAxiom youA]
          effects = runMergeAxioms allMergeAxioms mergedWorld md

      -- The scenario axiom should produce "another hunter" narration
      let isHunterNarrate (Effect (Narrate msg) _ _ _) = "another hunter" `isInfixOf` msg
          isHunterNarrate _                            = False
      any isHunterNarrate effects `shouldBe` True
