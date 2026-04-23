-- | Append-only event log with Lamport clock ordering and pluggable log-store backends.
module Engine.Sync.EventLog
  ( LogStore(..)
  , fileLogStore
  , augmentForeignLogs
  , nullLogStore
  , memoryLogStore
  , appendLogEntry
  , loadLog
  , logFileName
  , currentLogSchemaVersion
  , mkLogEntry
  , logAction
  , mergeLogs
  , replayFrom
  , scanForeignLogs
  , removeIfExists
  ) where

import           Control.Monad          (forM, forM_, when)
import           Control.Monad.Except   (throwError)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (asks)
import           Control.Monad.State    (get, gets, modify)
import           Data.IORef             (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict        as Map
import           Data.List              (isPrefixOf, sortBy)
import           Data.Maybe             (catMaybes, mapMaybe)
import           Data.Ord               (comparing)
import           System.Directory       (createDirectoryIfMissing, doesDirectoryExist,
                                         doesFileExist, listDirectory, removeFile)
import           System.FilePath        (takeDirectory, (</>))
import           System.IO.Error        (catchIOError)

import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as BLC

import           Engine.Core.Axioms     (diffWorlds, runAxioms)
import           Engine.Core.Conditions (checkCondition)
import           Engine.Core.Effects    (applyWorldDiff, executeEffectOnce)
import           Engine.Sync.Identity   (Identity, signEntry, verifyEntry)
import           Engine.Sync.Snapshot   (loadSnapshot, saveSnapshot, snapshotFileName)
import           GameTypes
import           MonadStack

-- ---------------------------------------------------------------------------
-- LogStore constructors
-- ---------------------------------------------------------------------------

-- | LogStore backed by the local filesystem sessions directory.
fileLogStore :: FilePath -> String -> PlayerId -> Maybe Identity -> LogStore
fileLogStore sessionsDir scenName playerId mIdent =
  let PlayerId playerStr = playerId
      logPath  = sessionsDir </> scenName </> playerStr </> logFileName
      snapPath = sessionsDir </> scenName </> playerStr </> snapshotFileName
  in LogStore
    { lsAppend      = \entry -> do
        let signed = maybe entry (`signEntry` entry) mIdent
        appendLogEntry logPath signed
    , lsLoadOwn     = loadLog logPath
    , lsForeignLogs = do
        foreignLogs <- scanForeignLogs sessionsDir scenName playerId
        forM foreignLogs $ \(pid, entries) -> do
          let PlayerId pidStr = pid
              foreignSnapPath = sessionsDir </> scenName </> pidStr </> snapshotFileName
          mSnap <- loadSnapshot foreignSnapPath
          pure (pid, entries, mSnap)
    , lsLoadSnap    = loadSnapshot snapPath
    , lsSaveSnap    = saveSnapshot snapPath
    , lsReset       = removeIfExists logPath >> removeIfExists snapPath
    }

-- | Wrap a 'LogStore' so its 'lsForeignLogs' also returns entries
-- from a secondary scanner.  Used by the shared-folder feature: the
-- local sessions dir still provides the canonical foreign logs, and
-- on top of that we fold in logs the player's friends dropped into
-- a shared folder like Dropbox.  The two sources are concatenated;
-- the sync layer's causal ordering handles the rest.
augmentForeignLogs
  :: IO [(PlayerId, [LogEntry], Maybe Snapshot)]
  -> LogStore
  -> LogStore
augmentForeignLogs extra base = base
  { lsForeignLogs = do
      baseLogs  <- lsForeignLogs base
      extraLogs <- extra
      pure (baseLogs ++ extraLogs)
  }

-- | LogStore that discards writes. For replay and test contexts where
-- no persistence is needed.
nullLogStore :: LogStore
nullLogStore = LogStore
  { lsAppend      = \_ -> pure ()
  , lsLoadOwn     = pure []
  , lsForeignLogs = pure []
  , lsLoadSnap    = pure Nothing
  , lsSaveSnap    = \_ -> pure ()
  , lsReset       = pure ()
  }

-- | LogStore that accumulates entries in an IORef (newest first).
-- Used by headless runners that need to inspect the log after a run.
memoryLogStore :: IORef [LogEntry] -> LogStore
memoryLogStore ref = nullLogStore
  { lsAppend = \entry -> modifyIORef' ref (entry :)
  }

-- ---------------------------------------------------------------------------
-- File utilities
-- ---------------------------------------------------------------------------

removeIfExists :: FilePath -> IO ()
removeIfExists path = do
  exists <- doesFileExist path
  when exists (removeFile path)

-- ---------------------------------------------------------------------------
-- Low-level log IO
-- ---------------------------------------------------------------------------

logFileName :: FilePath
logFileName = "events.jsonl"

-- | The schema version that new 'LogEntry' writes are stamped with.
-- Bump when the on-disk log format changes in a way that needs migration.
-- 'FromJSON' defaults missing versions to 1, so pre-versioning logs still
-- load.
currentLogSchemaVersion :: Int
currentLogSchemaVersion = 1

appendLogEntry :: FilePath -> LogEntry -> IO ()
appendLogEntry path entry = do
  createDirectoryIfMissing True (takeDirectory path)
  BLC.appendFile path (Aeson.encode entry <> BLC.singleton '\n')

loadLog :: FilePath -> IO [LogEntry]
loadLog path = do
  exists <- doesFileExist path
  if not exists
    then pure []
    else do
      contents <- BS.readFile path
      pure (mapMaybe Aeson.decode (filter (not . BLC.null) (BLC.lines (BLC.fromStrict contents))))

-- ---------------------------------------------------------------------------
-- Log entry construction
-- ---------------------------------------------------------------------------

-- | Build a log entry from an already-known clock value and causal frontier.
-- The clock should come from worldClock (advanced by executeStep).
mkLogEntry :: PlayerId -> LamportClock -> ActionId -> WorldDiff -> CausalFrontier -> LogEntry
mkLogEntry pid clock aid diff frontier =
  let tick = lcTick clock
      PlayerId p = pid
  in LogEntry
    { entryId            = show tick <> "-" <> p
    , entryClock         = clock { lcPlayerId = pid }
    , entryPlayerId      = pid
    , entryActionId      = aid
    , entryDiff          = diff
    , entrySignature     = Nothing
    , entryFrontier      = frontier
    , entrySchemaVersion = currentLogSchemaVersion
    }

-- ---------------------------------------------------------------------------
-- App helper: append current action to log (no-op if no log configured)
-- ---------------------------------------------------------------------------

logAction :: ActionId -> WorldDiff -> App ()
logAction aid diff = do
  if isNoOp diff then pure () else do
    pid      <- asks envPlayerId
    clock    <- gets worldClock
    store    <- asks envLogStore
    frontier <- liftIO . readIORef =<< asks envFrontier
    let entry = mkLogEntry pid clock aid diff frontier
    liftIO $ lsAppend store entry
  where
    isNoOp d = null (diffStats d) && null (diffRelations d)
            && null (diffTagsAdded d) && null (diffTagsRemoved d)
            && null (diffWorldTagsAdded d) && null (diffWorldTagsRemoved d)
            && null (diffLocations d)

-- ---------------------------------------------------------------------------
-- Replay
-- ---------------------------------------------------------------------------

-- | Replay a list of log entries starting from a given world state.
-- Applies each entry's stored diff then fires axioms — no action ID lookup.
replayFrom :: Scenario -> GameWorld -> [LogEntry] -> IO (Either AppError GameWorld)
replayFrom scenario world entries = do
  debugRef <- newIORef Off
  msgRef   <- newIORef []
  traceRef <- newIORef []
  frontierRef <- newIORef Map.empty
  let env = Env
        { envActions      = scenarioActions scenario
        , envAxioms       = scenarioAxioms scenario
        , envMergeAxioms  = scenarioMergeAxioms scenario
        , envRules        = scenarioRules scenario
        , envMergeRules   = scenarioMergeRules scenario
        , envLog          = \_ -> pure ()
        , envDebug        = debugRef
        , envTerminal     = scenarioTerminal scenario
        , envMessageLog   = msgRef
        , envPlayerId     = PlayerId "replay"
        , envPlayerCharId = scenarioPlayerCharId scenario
        , envLogStore     = nullLogStore
        , envAxiomTrace   = traceRef
        , envFrontier     = frontierRef
        , envLiveMerge    = \w -> pure (w, [])
        }
  result <- runApp env world (forM_ entries applyLogEntry)
  pure (fmap snd result)

-- ---------------------------------------------------------------------------
-- Merge
-- ---------------------------------------------------------------------------

-- | Merge two event logs. Returns the length of the common prefix and the
-- merged divergent tail sorted by Lamport clock (tick first, PlayerId as
-- tie-breaker, giving a deterministic total order on both sides).
--
-- The common prefix is the longest sequence of entries with matching entryIds
-- at the head of both logs — i.e. entries that were already synced between
-- the two players. Entries after the divergence point are from only one side
-- and are merged by Lamport order.
mergeLogs :: [LogEntry] -> [LogEntry] -> (Int, [LogEntry])
mergeLogs logA logB = (commonLen, merged)
  where
    commonLen  = length $ takeWhile (uncurry sameId) (zip logA logB)
    sameId a b = entryId a == entryId b
    divergentA = drop commonLen logA
    divergentB = drop commonLen logB
    merged     = sortBy (comparing entryClock) (divergentA ++ divergentB)

-- | Replay by applying the stored diff then firing axioms.
-- All entries — own and foreign — take this path. No action ID lookup.
applyLogEntry :: LogEntry -> App ()
applyLogEntry entry =
  if not (verifyEntry entry)
    then throwError (InvalidAction ("Signature verification failed for entry: " <> entryId entry))
    else do
      modify (\w -> w { worldClock = entryClock entry })
      worldBefore <- get
      let pid = entryPlayerId entry
      applyWorldDiff (entryDiff entry)
      worldAfter <- get
      axioms    <- asks envAxioms
      actions   <- asks envActions
      let available = filter (checkCondition worldBefore . anyActionCondition) actions
      mapM_ executeEffectOnce (runAxioms axioms worldAfter available (diffWorlds pid worldBefore worldAfter))

-- ---------------------------------------------------------------------------
-- Foreign log scan
-- ---------------------------------------------------------------------------

-- | Scan sessionsDir/<scenarioName>/ for subdirectories belonging to other
-- players and load their logs. Returns (PlayerId, [LogEntry]) for each
-- foreign player whose log file exists and parses.
scanForeignLogs :: FilePath -> String -> PlayerId -> IO [(PlayerId, [LogEntry])]
scanForeignLogs sessionsDir scenId (PlayerId ownId) = do
  let scenarioDir = sessionsDir </> scenId
  isDir <- doesDirectoryExist scenarioDir
  if not isDir
    then pure []
    else do
      subdirs <- listDirectory scenarioDir `catchIOError` \_ -> pure []
      let others = filter (\d -> d /= ownId && not ("." `isPrefixOf` d)) subdirs
      catMaybes <$> mapM loadForeign others
  where
    loadForeign pid = do
      let path = sessionsDir </> scenId </> pid </> logFileName
      exists <- doesFileExist path
      if not exists
        then pure Nothing
        else do
          entries <- loadLog path
          pure (Just (PlayerId pid, entries))

