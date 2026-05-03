{-# LANGUAGE TupleSections  #-}
{-# OPTIONS_GHC -fno-hpc #-}
-- | Main game loop: session management, tick execution, axiom evaluation, and player I/O dispatch.
module Engine.Runtime
  ( RuntimeUI(..)
  , runScenario
  , runScenarioWith
  , runScenarioWithEnd
  , offerMerge
  , sessionsRootDir
  ) where

import           Control.Exception      (bracket_)
import           Control.Monad          (when)
import           Data.IORef             (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict        as Map
import           Data.Time.Clock.POSIX  (getPOSIXTime)
import           System.Environment     (getArgs)
import           Text.Read              (readMaybe)

import           Engine.Core.Axioms     (systemMergeAxioms)
import           Engine.Core.Conditions (checkCondition)
import           Engine.Core.Effects    (executeEffectOnce, mergeWorlds)
import           Engine.Sync.Causality  (buildMergeDiff, runMergeAxioms, runMergeRules)
import           Engine.Sync.EventLog   (augmentForeignLogs, fileLogStore,
                                         mergeLogs, nullLogStore, replayFrom)
import           Engine.Sync.Identity   (Identity(..), defaultIdentityPath,
                                         loadOrCreate, playerCharId, playerIdOf)
import           GameTypes
import           MonadStack

-- | Abstract UI callbacks so the engine doesn't depend on Terminal.
data RuntimeUI = RuntimeUI
  { uiSetup       :: IO ()                                  -- ^ terminal mode init
  , uiTeardown    :: IO ()                                  -- ^ terminal mode cleanup
  , uiGameLoop    :: Env -> GameWorld -> IO (AppResult ())   -- ^ main interactive loop
  , uiOnEnd       :: GameWorld -> IO ()                      -- ^ display end screen, wait
  , uiOnError     :: String -> IO ()                         -- ^ display fatal error
  , uiOnWarn      :: String -> IO ()                         -- ^ display warning
  , uiPromptMerge :: String -> Int -> IO Bool                -- ^ name, count -> accept?
  }

runScenario :: RuntimeUI -> (Int -> CharacterId -> Scenario) -> IO ()
runScenario ui = runScenarioWith ui (const (pure []))

-- | Like 'runScenario', but also folds an extra foreign-logs source
-- into the merge pipeline.  Used by the shared-folder feature: the
-- supplied action receives the scenario name and returns whatever
-- logs exist in the player's shared folder for that scenario.
-- 'runScenario' supplies a no-op to preserve the original behavior.
runScenarioWith
  :: RuntimeUI
  -> (String -> IO [(PlayerId, [LogEntry], Maybe Snapshot)])
  -> (Int -> CharacterId -> Scenario)
  -> IO ()
runScenarioWith ui extraForeign = runScenarioWithEnd ui extraForeign (\_ -> pure ())

-- | Like 'runScenarioWith', but also calls a post-scenario hook with
-- the final 'GameWorld' when the scenario reaches its terminal
-- condition.  The hook fires after the end screen and before the
-- snapshot save, and is the launcher's hook for end-of-hunt
-- bookkeeping (e.g. lifetime-find progress transitions).  The hook
-- does NOT fire when the player quits with @q@.
runScenarioWithEnd
  :: RuntimeUI
  -> (String -> IO [(PlayerId, [LogEntry], Maybe Snapshot)])
  -> (GameWorld -> IO ())
  -> (Int -> CharacterId -> Scenario)
  -> IO ()
runScenarioWithEnd ui extraForeign onTerminal mkScenario = do
  args <- getArgs
  let newSession  = "--new-session" `elem` args || "-ns" `elem` args
      sessionDir  = parseSessionDir args
  msgRef     <- newIORef []
  idPath     <- defaultIdentityPath
  ident      <- loadOrCreate idPath
  freshSeed  <- epochSeed args
  let playerId = playerIdOf ident
      you      = playerCharId ident
      -- Build once with the fresh seed so we know the scenario name
      -- (needed to locate the save file).  We'll rebuild below using
      -- the saved world's seed if a snapshot exists, so the generated
      -- map matches the saved state.
      scenarioName0 = scenarioName (mkScenario freshSeed you)
  let rawStore = fileLogStore sessionDir scenarioName0 playerId (Just ident)
      store    = augmentForeignLogs (extraForeign scenarioName0) rawStore
  when newSession $ lsReset store
  mSnap <- lsLoadSnap store
  -- Scenario map generation is seeded, so a mismatched seed on load
  -- would produce a different map than the one in the snapshot, and
  -- movement actions would reference locations that don't exist in
  -- the loaded world.  Prefer the snapshot's seed whenever we have
  -- one; fall back to the fresh seed for a brand-new hunt.
  let seed     = maybe freshSeed (worldSeed . snapWorld) mSnap
      scenario = mkScenario seed you
  debugRef    <- newIORef (scenarioDebugDefault scenario)
  traceRef    <- newIORef []
  frontierRef <- newIORef Map.empty
  let (baseWorld, logOffset) = maybe (scenarioInitial scenario, 0)
                                     (\s -> (snapWorld s, snapOffset s)) mSnap
  ownLog <- lsLoadOwn store
  resumeWorld <- case drop logOffset ownLog of
    []      -> pure baseWorld
    entries -> do
      r <- replayFrom scenario baseWorld entries
      case r of
        Left err -> do
          uiOnWarn ui ("Snapshot replay failed (" <> show err <> "), starting fresh.")
          pure (scenarioInitial scenario)
        Right w  -> pure w
  (mergedWorld, mActs, mRules, mMRules) <-
    offerMerge ui store scenario playerId ownLog resumeWorld
  -- Seed the frontier with all currently-known foreign log lengths so the
  -- live merge hook only picks up entries that arrive after this point.
  seedFrontier frontierRef store
  let liveMerge = mkLiveMerge ui store scenario playerId frontierRef
  let env = Env { envActions      = mActs
                , envAxioms       = scenarioAxioms scenario
                , envMergeAxioms  = scenarioMergeAxioms scenario
                , envRules        = mRules
                , envMergeRules   = mMRules
                , envLog          = putStrLn
                , envDebug        = debugRef
                , envTerminal     = scenarioTerminal scenario
                , envMessageLog   = msgRef
                , envPlayerId     = playerId
                , envPlayerCharId = you
                , envLogStore     = store
                , envAxiomTrace   = traceRef
                , envFrontier     = frontierRef
                , envLiveMerge    = liveMerge
                }
  let world = mergedWorld
        { worldCharacters = Map.adjust (\c -> c { charName = identityLabel ident }) you
            (worldCharacters mergedWorld) }
  bracket_ (uiSetup ui) (uiTeardown ui) $ do
    result <- uiGameLoop ui env world
    case result of
      Left err          -> uiOnError ui (show err)
      Right (_, finalW) -> do
        -- Only show end screen if the scenario reached a terminal condition.
        -- If the player quit (q), skip it — immediate exit.  The
        -- onTerminal hook is gated the same way: end-of-hunt
        -- bookkeeping (e.g. lifetime-find progress) only runs for
        -- hunts that actually ended, not for quit-mid-hunt sessions.
        when (checkCondition finalW (scenarioTerminal scenario)) $ do
          uiOnEnd ui finalW
          onTerminal finalW
        finalLog <- lsLoadOwn store
        lsSaveSnap store (Snapshot finalW (length finalLog)
                           (scenarioActions scenario)
                           (scenarioRules scenario)
                           (scenarioMergeRules scenario))

-- | Scan for foreign logs and, for each one with new entries, ask the player
-- whether to merge. Returns the world after applying accepted merges.
-- After each successful merge, computes a MergeDiff with provenance and
-- runs merge axioms.
offerMerge :: RuntimeUI -> LogStore -> Scenario -> PlayerId -> [LogEntry] -> GameWorld
           -> IO (GameWorld, [AnyAction], [AxiomRule], [MergeAxiomRule])
offerMerge ui store scenario myPid ownLog currentWorld = do
  foreignData <- lsForeignLogs store
  let initActs   = scenarioActions scenario
      initRules  = scenarioRules scenario
      initMRules = scenarioMergeRules scenario
  go currentWorld initActs initRules initMRules foreignData
  where
    go world acts rules mRules [] = pure (world, acts, rules, mRules)
    go world acts rules mRules ((PlayerId pid, theirLog, mForeignSnap):rest) = do
      let theirCharId = Named (take 12 pid)
          displayName = maybe (take 12 pid) charName
                          (Map.lookup theirCharId (worldCharacters world))
      mMerge <- case mForeignSnap of
        Just snap -> do
          let entries = drop (snapOffset snap) theirLog
          pure (Right (mergeWorlds world (snapWorld snap), entries))
        Nothing -> do
          let (commonLen, divergent) = mergeLogs ownLog theirLog
          mBase <- replayFrom scenario (scenarioInitial scenario) (take commonLen ownLog)
          pure (fmap (, divergent) mBase)
      -- Merge scenario data from the foreign snapshot (if present).
      let (acts', rules', mRules') = case mForeignSnap of
            Just snap -> ( mergeActions acts (snapActions snap)
                         , mergeRules rules (snapRules snap)
                         , mergeMergeRules mRules (snapMergeRules snap)
                         )
            Nothing   -> (acts, rules, mRules)
      case mMerge of
        Left err -> do
          uiOnWarn ui ("Merge failed: " <> show err <> " -- skipping.")
          go world acts rules mRules rest
        -- No snapshot and no divergent entries: nothing to merge.
        Right (_mergeBase, entries)
          | null entries, Nothing <- mForeignSnap -> go world acts rules mRules rest
        -- Snapshot with no new entries: CRDT merge only, no replay needed.
        -- This is the common case when a foreign player's snapshot is current.
        Right (mergeBase, entries)
          | null entries -> do
              accepted <- uiPromptMerge ui displayName 0
              if accepted
                then do
                  let md       = buildMergeDiff myPid ownLog [] world mergeBase
                      allMergeAxioms = systemMergeAxioms ++ scenarioMergeAxioms scenario
                      mEffects = runMergeAxioms allMergeAxioms mergeBase md
                                  ++ runMergeRules (scenarioMergeRules scenario) mergeBase md
                  finalWorld <- applyMergeEffects scenario mergeBase mEffects
                  go finalWorld acts' rules' mRules' rest
                else go world acts rules mRules rest
        -- Has entries to replay on top of the merge base.
        Right (mergeBase, entries) -> do
              accepted <- uiPromptMerge ui displayName (length entries)
              if accepted
                then do
                  result <- replayFrom scenario mergeBase entries
                  case result of
                    Left err       -> do
                      uiOnWarn ui ("Merge failed: " <> show err <> " -- skipping.")
                      go world acts rules mRules rest
                    Right newWorld -> do
                      -- Run merge axioms and rules with provenance
                      let md       = buildMergeDiff myPid ownLog entries world newWorld
                          allMergeAxioms = systemMergeAxioms ++ scenarioMergeAxioms scenario
                          mEffects = runMergeAxioms allMergeAxioms newWorld md
                                      ++ runMergeRules (scenarioMergeRules scenario) newWorld md
                      finalWorld <- applyMergeEffects scenario newWorld mEffects
                      go finalWorld acts' rules' mRules' rest
                else go world acts rules mRules rest

    applyMergeEffects :: Scenario -> GameWorld -> [Effect] -> IO GameWorld
    applyMergeEffects = applyMergeEffectsIO

-- ---------------------------------------------------------------------------
-- Live merge (between turns)
-- ---------------------------------------------------------------------------

-- | Record the current length of each foreign log in the frontier so the
-- live merge hook only picks up entries that arrive after this point.
seedFrontier :: IORef CausalFrontier -> LogStore -> IO ()
seedFrontier ref store = do
  foreignData <- lsForeignLogs store
  let frontier = Map.fromList
        [ (pid, entryId (last entries))
        | (pid, entries, _) <- foreignData
        , not (null entries)
        ]
  writeIORef ref frontier

-- | Build the between-turn live merge function.  Each call scans the session
-- directory for foreign logs, compares against the frontier to find new
-- entries, and silently applies them.  Returns the merged world and a list
-- of (displayName, entryCount) for narration.
mkLiveMerge :: RuntimeUI -> LogStore -> Scenario -> PlayerId
            -> IORef CausalFrontier -> GameWorld -> IO (GameWorld, [(String, Int)])
mkLiveMerge ui store scenario myPid frontierRef world = do
  foreignData <- lsForeignLogs store
  frontier    <- readIORef frontierRef
  go world frontier [] foreignData
  where
    go w _ acc [] = pure (w, reverse acc)
    go w frontier acc ((pid@(PlayerId pidStr), theirLog, _mSnap):rest)
      | null theirLog = go w frontier acc rest
      | otherwise = do
          let lastSeenId = Map.lookup pid frontier
              newEntries = case lastSeenId of
                Nothing  -> theirLog
                Just eid -> drop 1 (dropWhile (\e -> entryId e /= eid) theirLog)
          if null newEntries
            then go w frontier acc rest
            else do
              let theirCharId = Named (take 12 pidStr)
                  displayName = maybe (take 12 pidStr) charName
                                  (Map.lookup theirCharId (worldCharacters w))
              let frontier' = Map.insert pid (entryId (last newEntries)) frontier
              writeIORef frontierRef frontier'
              result <- replayFrom scenario w newEntries
              case result of
                Left err -> do
                  uiOnWarn ui ("Live merge failed: " <> show err <> " — skipping.")
                  go w frontier' acc rest
                Right newWorld -> do
                  ownLog <- lsLoadOwn store
                  let md = buildMergeDiff myPid ownLog newEntries w newWorld
                      allMergeAxioms = systemMergeAxioms ++ scenarioMergeAxioms scenario
                      mEffects = runMergeAxioms allMergeAxioms newWorld md
                                  ++ runMergeRules (scenarioMergeRules scenario) newWorld md
                  finalWorld <- applyMergeEffectsIO scenario newWorld mEffects
                  go finalWorld frontier' ((displayName, length newEntries) : acc) rest

-- | Execute merge axiom effects (standalone, used by live merge).
applyMergeEffectsIO :: Scenario -> GameWorld -> [Effect] -> IO GameWorld
applyMergeEffectsIO _scenario world [] = pure world
applyMergeEffectsIO scenario' world effs = do
  debugRef    <- newIORef Off
  msgRef      <- newIORef []
  traceRef    <- newIORef []
  frontierRef <- newIORef Map.empty
  let env = Env
        { envActions      = scenarioActions scenario'
        , envAxioms       = scenarioAxioms scenario'
        , envMergeAxioms  = scenarioMergeAxioms scenario'
        , envRules        = scenarioRules scenario'
        , envMergeRules   = scenarioMergeRules scenario'
        , envLog          = \_ -> pure ()
        , envDebug        = debugRef
        , envTerminal     = scenarioTerminal scenario'
        , envMessageLog   = msgRef
        , envPlayerId     = PlayerId "merge"
        , envPlayerCharId = scenarioPlayerCharId scenario'
        , envLogStore     = nullLogStore
        , envAxiomTrace   = traceRef
        , envFrontier     = frontierRef
        , envLiveMerge    = \w -> pure (w, [])
        }
  result <- runApp env world (mapM_ executeEffectOnce effs)
  case result of
    Left _       -> pure world
    Right (_, w) -> pure w

-- | Parse --session-dir from command-line args, defaulting to "sessions".
parseSessionDir :: [String] -> FilePath
parseSessionDir []                    = "sessions"
parseSessionDir ("--session-dir":d:_) = d
parseSessionDir (_:rest)              = parseSessionDir rest

sessionsRootDir :: FilePath
sessionsRootDir = "sessions"

-- ---------------------------------------------------------------------------
-- Session seed
-- ---------------------------------------------------------------------------

-- | Generate a session seed. Uses epoch seconds by default, or a fixed value
-- from @--seed N@ for deterministic replays.
epochSeed :: [String] -> IO Int
epochSeed args = case parseSeedArg args of
  Just n  -> pure n
  Nothing -> round <$> getPOSIXTime

parseSeedArg :: [String] -> Maybe Int
parseSeedArg []             = Nothing
parseSeedArg ("--seed":v:_) = readMaybe v
parseSeedArg (_:rest)       = parseSeedArg rest
