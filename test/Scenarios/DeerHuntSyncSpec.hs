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
import           Scenarios.DeerHunt.Generation (TerrainClass(..))
import           Scenarios.DeerHunt.Probability (doesShotHit)
import           Scenarios.DeerHuntTestFixtures

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
-- Walk scripts — derived from the canonical fixture map rather than
-- hand-authored location identifiers.
-- ---------------------------------------------------------------------------

-- | Walk path from the fixture start into a field location.  Returns
-- the list of ActionIds and the final location.
fieldWalk :: ([ActionId], Location)
fieldWalk =
  let (path, endLoc) = walkPath fixtureStart CField 3
  in (map (uncurry edgeActionId) path, endLoc)

-- | Walk path from the fixture start into a bush location.  Returns
-- the list of ActionIds and the final location.
bushWalk :: ([ActionId], Location)
bushWalk =
  let (path, endLoc) = walkPath fixtureStart CBush 3
  in (map (uncurry edgeActionId) path, endLoc)

walkToField :: [ActionId]
walkToField = fst fieldWalk

walkToBush :: [ActionId]
walkToBush = fst bushWalk

fieldEnd :: Location
fieldEnd = snd fieldWalk

-- ---------------------------------------------------------------------------
-- Merge helpers (same pattern as CoLocationSpec)
-- ---------------------------------------------------------------------------

-- | Snapshot merge: run both hunters independently, CRDT-merge final worlds.
snapshotMerge :: [ActionId] -> [ActionId] -> IO GameWorld
snapshotMerge scriptA scriptB = do
  Right (worldA, _) <- runHeadlessScript (deerHunt fixtureSeed) pidA scriptA
  Right (worldB, _) <- runHeadlessScript (deerHunt fixtureSeed) pidB scriptB
  pure (mergeWorlds worldA worldB)

-- | Log merge: merge initial worlds as base, merge logs, replay.
logMerge :: [ActionId] -> [ActionId] -> IO (Either AppError GameWorld)
logMerge scriptA scriptB = do
  let scenA = deerHunt fixtureSeed youA
      scenB = deerHunt fixtureSeed youB
      base  = mergeWorlds (scenarioInitial scenA) (scenarioInitial scenB)
  Right (_, logA) <- runHeadlessScript (deerHunt fixtureSeed) pidA scriptA
  Right (_, logB) <- runHeadlessScript (deerHunt fixtureSeed) pidB scriptB
  let (_, divergent) = mergeLogs logA logB
  replayFrom scenA base divergent

-- | Check if the wave action is available for a given hunter in the world.
canWave :: CharId -> GameWorld -> Bool
canWave you world =
  let scen    = deerHunt fixtureSeed you
      actions = scenarioActions scen
      waveActions = filter (\a -> anyActionId a == ActionId "wave") actions
  in any (checkCondition world . anyActionCondition) waveActions

-- ---------------------------------------------------------------------------
-- Pinned-deer scenario: remove deer movement and spook axioms so the deer
-- stays at fieldEnd and doesn't bolt when a hunter arrives.
-- ---------------------------------------------------------------------------

pinnedDeerHunt :: CharId -> Scenario
pinnedDeerHunt you =
  let base = deerHunt fixtureSeed you
      pinned = filter (\a -> axiomId a `notElem`
                  [ ScenarioAxiom "deerMovement"
                  , ScenarioAxiom "spook"
                  ]) (scenarioAxioms base)
      w0 = scenarioInitial base
      w0pinned = w0 { worldLocations = Map.insert deer fieldEnd (worldLocations w0) }
  in base { scenarioAxioms = pinned, scenarioInitial = w0pinned }

pinnedSnapshotMerge :: [ActionId] -> [ActionId] -> IO GameWorld
pinnedSnapshotMerge scriptA scriptB = do
  Right (worldA, _) <- runHeadlessScript pinnedDeerHunt pidA scriptA
  Right (worldB, _) <- runHeadlessScript pinnedDeerHunt pidB scriptB
  pure (mergeWorlds worldA worldB)

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
                     , worldLocations = Map.fromList [(you, fieldEnd), (deer, fieldEnd)]
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
                     , worldLocationHistory = Map.empty
                     , worldLocationVisits  = Map.empty
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

  describe "two hunters miss each other" $ do

    it "snapshot merge: both players have locations, wave unavailable" $ do
      w <- snapshotMerge walkToField walkToBush
      Map.member youA (worldLocations w) `shouldBe` True
      Map.member youB (worldLocations w) `shouldBe` True
      Map.member deer (worldLocations w) `shouldBe` True
      checkCondition w (HasWorldTag deerKilled) `shouldBe` False
      canWave youA w `shouldBe` False

    it "log merge: both players have locations" $ do
      result <- logMerge walkToField walkToBush
      case result of
        Left err -> expectationFailure ("merge failed: " <> show err)
        Right w -> do
          Map.member youA (worldLocations w) `shouldBe` True
          Map.member youB (worldLocations w) `shouldBe` True

    it "snapshot and log merge agree on locations" $ do
      sw     <- snapshotMerge walkToField walkToBush
      result <- logMerge      walkToField walkToBush
      case result of
        Left err -> expectationFailure ("merge failed: " <> show err)
        Right lw -> do
          Map.lookup youA (worldLocations sw) `shouldBe` Map.lookup youA (worldLocations lw)
          Map.lookup youB (worldLocations sw) `shouldBe` Map.lookup youB (worldLocations lw)

  describe "two hunters at same location after merge" $ do

    it "snapshot merge: both at same field spot, wave available" $ do
      w <- pinnedSnapshotMerge walkToField walkToField
      Map.lookup youA (worldLocations w) `shouldBe` Just fieldEnd
      Map.lookup youB (worldLocations w) `shouldBe` Just fieldEnd
      canWave youA w `shouldBe` True

    it "log merge: wave available" $ do
      result <- pinnedLogMerge walkToField walkToField
      case result of
        Left err -> expectationFailure ("merge failed: " <> show err)
        Right w  -> canWave youA w `shouldBe` True

    it "snapshot and log merge agree on wave availability" $ do
      sw     <- pinnedSnapshotMerge walkToField walkToField
      result <- pinnedLogMerge      walkToField walkToField
      case result of
        Left err -> expectationFailure ("merge failed: " <> show err)
        Right lw -> canWave youA sw `shouldBe` canWave youA lw

  describe "one hunter kills deer, other inherits" $ do

    let tick = findHitTick youA 4
        killScript = walkToField ++ [ActionId "look", ActionId "takeTheShot"]
        bystander  = walkToBush

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
      Map.member youA (worldLocations w) `shouldBe` True
      Map.member youB (worldLocations w) `shouldBe` True
      checkCondition w (HasWorldTag deerKilled) `shouldBe` True

    it "log merge: deerKilled propagates" $ do
      result <- killLogMerge killScript bystander
      case result of
        Left err -> expectationFailure ("merge failed: " <> show err)
        Right w  -> checkCondition w (HasWorldTag deerKilled) `shouldBe` True

    it "terminal condition fires on merged world" $ do
      w <- killSnapshotMerge killScript bystander
      let terminal = scenarioTerminal (deerHunt fixtureSeed youB)
      checkCondition w terminal `shouldBe` True

  describe "co-located kill — both hunters near the deer" $ do

    let tick = findHitTick youA 4
        killScript   = walkToField ++ [ActionId "look", ActionId "takeTheShot"]
        nearbyScript = walkToField ++ [ActionId "look"]

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
      Map.lookup youA (worldLocations w) `shouldBe` Just fieldEnd
      Map.lookup youB (worldLocations w) `shouldBe` Just fieldEnd
      checkCondition w (HasWorldTag deerKilled) `shouldBe` True

    it "log merge: deer killed with both present" $ do
      result <- colocKillLogMerge killScript nearbyScript
      case result of
        Left err -> expectationFailure ("merge failed: " <> show err)
        Right w  -> do
          checkCondition w (HasWorldTag deerKilled) `shouldBe` True
          Map.lookup youA (worldLocations w) `shouldBe` Just fieldEnd
          Map.lookup youB (worldLocations w) `shouldBe` Just fieldEnd

    it "snapshot and log merge agree" $ do
      sw     <- colocKillSnapshotMerge killScript nearbyScript
      result <- colocKillLogMerge      killScript nearbyScript
      case result of
        Left err -> expectationFailure ("merge failed: " <> show err)
        Right lw -> do
          checkCondition sw (HasWorldTag deerKilled) `shouldBe` checkCondition lw (HasWorldTag deerKilled)
          Map.lookup youA (worldLocations sw) `shouldBe` Map.lookup youA (worldLocations lw)
          Map.lookup youB (worldLocations sw) `shouldBe` Map.lookup youB (worldLocations lw)

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
      Right (worldA, logA) <- runHeadlessScript pinnedDeerHunt pidA walkToField
      let snapA = Snapshot worldA (length logA) [] [] []
      let scenB  = pinnedDeerHunt youB
          worldB = scenarioInitial scenB
          store  = nullLogStore { lsForeignLogs = pure [(pidA, logA, Just snapA)] }
      (merged, _, _, _) <- offerMerge autoAcceptUI store scenB pidB [] worldB
      Map.member youA (worldLocations merged) `shouldBe` True
      Map.lookup youA (worldLocations merged) `shouldBe` Just fieldEnd

    it "current snapshot without acceptance leaves world unchanged" $ do
      Right (worldA, logA) <- runHeadlessScript pinnedDeerHunt pidA walkToField
      let snapA = Snapshot worldA (length logA) [] [] []
      let scenB  = pinnedDeerHunt youB
          worldB = scenarioInitial scenB
          store  = nullLogStore { lsForeignLogs = pure [(pidA, logA, Just snapA)] }
          rejectUI = autoAcceptUI { uiPromptMerge = \_ _ -> pure False }
      (merged, _, _, _) <- offerMerge rejectUI store scenB pidB [] worldB
      Map.member youA (worldLocations merged) `shouldBe` False

  describe "merge axiom fires on stranger arrival" $ do

    it "MergeDiff contains Unaware location deltas" $ do
      Right (worldA, logA) <- runHeadlessScript pinnedDeerHunt pidA walkToField
      Right (_worldB, logB) <- runHeadlessScript pinnedDeerHunt pidB walkToBush
      let scenA = pinnedDeerHunt youA
          scenB = pinnedDeerHunt youB
          base  = mergeWorlds (scenarioInitial scenA) (scenarioInitial scenB)
          (_, divergent) = mergeLogs logA logB
      Right mergedWorld <- replayFrom scenA base divergent
      let md = buildMergeDiff pidA logA divergent worldA mergedWorld
      let unawareLocs = filter (\d -> mdProvenance d == Unaware) (mergeLocations md)
      unawareLocs `shouldSatisfy` (not . null)

    it "system axiom fires when hunters merge at different locations" $ do
      Right (worldA, logA) <- runHeadlessScript pinnedDeerHunt pidA walkToField
      Right (_worldB, logB) <- runHeadlessScript pinnedDeerHunt pidB walkToBush
      let scenA = pinnedDeerHunt youA
          scenB = pinnedDeerHunt youB
          base  = mergeWorlds (scenarioInitial scenA) (scenarioInitial scenB)
          (_, divergent) = mergeLogs logA logB
      Right mergedWorld <- replayFrom scenA base divergent
      let md = buildMergeDiff pidA logA divergent worldA mergedWorld
          effects = runMergeAxioms systemMergeAxioms mergedWorld md
      effects `shouldSatisfy` (not . null)

    it "scenario axiom does NOT fire when hunters are at different locations" $ do
      Right (worldA, logA) <- runHeadlessScript pinnedDeerHunt pidA walkToField
      Right (_worldB, logB) <- runHeadlessScript pinnedDeerHunt pidB walkToBush
      let scenA = pinnedDeerHunt youA
          scenB = pinnedDeerHunt youB
          base  = mergeWorlds (scenarioInitial scenA) (scenarioInitial scenB)
          (_, divergent) = mergeLogs logA logB
      Right mergedWorld <- replayFrom scenA base divergent
      let md = buildMergeDiff pidA logA divergent worldA mergedWorld
          effects = runMergeAxioms [hunterArrivalMergeAxiom youA] mergedWorld md
      effects `shouldBe` []

    it "scenario axiom fires when hunters merge at the SAME location" $ do
      Right (worldA, logA) <- runHeadlessScript pinnedDeerHunt pidA walkToField
      Right (_worldB, logB) <- runHeadlessScript pinnedDeerHunt pidB walkToField
      let scenA = pinnedDeerHunt youA
          scenB = pinnedDeerHunt youB
          base  = mergeWorlds (scenarioInitial scenA) (scenarioInitial scenB)
          (_, divergent) = mergeLogs logA logB
      Right mergedWorld <- replayFrom scenA base divergent
      let md = buildMergeDiff pidA logA divergent worldA mergedWorld
          allMergeAxioms = systemMergeAxioms ++ [hunterArrivalMergeAxiom youA]
          effects = runMergeAxioms allMergeAxioms mergedWorld md
      let isHunterNarrate (Effect (Narrate msg) _ _ _) = "another hunter" `isInfixOf` msg
          isHunterNarrate _                            = False
      any isHunterNarrate effects `shouldBe` True
