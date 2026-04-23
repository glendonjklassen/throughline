-- | Shared-folder "text your friends" multiplayer.
--
-- The CRDT event log is already the whole story: every action is a
-- signed, Lamport-ordered entry in @sessions\/<scenario>\/<playerId>\/
-- events.jsonl@, and the sync layer already merges foreign logs it
-- finds at scenario start.  "Shared-folder multiplayer" just points
-- that sync mechanism at a folder two or more players agree to drop
-- their logs into (Dropbox, Google Drive, Syncthing, etc.) — no
-- server, no service.
--
-- This module adds the two missing pieces:
--
--   * 'broadcastLog': copy the current player's log into the shared
--     folder as @<sharedFolder>\/<playerId>\/events.jsonl@.  Called
--     from the journal when the player picks the "text your friends"
--     action.
--
--   * 'scanSharedLogs': walk @<sharedFolder>\/*@ for foreign-player
--     subdirectories and load every @events.jsonl@ that belongs to
--     the current scenario.  Returns the same shape
--     'lsForeignLogs' already produces, so the runtime can fold it
--     into the existing merge pipeline without new code paths.
--
-- The shape on disk looks like:
--
--   sharedFolder/
--     <playerId-a>/
--       events.jsonl
--       scenario.txt     -- name of the scenario this log targets
--     <playerId-b>/
--       events.jsonl
--       scenario.txt
--
-- 'scenario.txt' lets us filter out logs from other scenarios that
-- share the folder, so friends who play both \"Deer Hunt\" and some
-- future title can use the same Dropbox folder without stepping on
-- each other.
module SDL.SharedFolder
  ( broadcastLog
  , scanSharedLogs
  , expandSharedPath
  , sharedScenarioFile
  , SharedBroadcastResult(..)
  ) where

import           Control.Exception      (SomeException, try)
import           Control.Monad          (forM, when)
import           Data.Either            (fromRight)
import           Data.List              (isPrefixOf)
import           Data.Maybe             (catMaybes)
import           System.Directory       (copyFile, createDirectoryIfMissing,
                                         doesDirectoryExist, doesFileExist,
                                         getHomeDirectory, listDirectory)
import           System.FilePath        ((</>))

import           Engine.Runtime         (sessionsRootDir)
import           Engine.Sync.EventLog   (loadLog, logFileName)
import           Engine.Sync.Snapshot   (loadSnapshot, snapshotFileName)
import           GameTypes              (LogEntry, PlayerId(..), Snapshot)

-- | Sentinel file inside each player's shared dir that names the
-- scenario the log belongs to.  Lets 'scanSharedLogs' skip foreign
-- logs belonging to a different scenario, and lets a reader
-- double-check a log before replay.
sharedScenarioFile :: FilePath
sharedScenarioFile = "scenario.txt"

-- | What happened during a broadcast.  Reported back to the journal
-- UI so the player gets confirmation (or a clear error).
data SharedBroadcastResult
  = Broadcast          !FilePath  -- ^ path of the written copy
  | BroadcastNoLog                -- ^ local log is empty; nothing to send
  | BroadcastFailed    !String    -- ^ IO error message
  deriving (Show, Eq)

-- | Copy the current player's log (and snapshot, if present) into
-- the shared folder.  The destination is @\<folder\>\/\<playerId\>\/
-- events.jsonl@; a @scenario.txt@ marker is written alongside so
-- readers can filter by scenario.
--
-- The local log file is read-only from this side — broadcasting
-- never modifies the player's own save.
broadcastLog
  :: FilePath     -- ^ shared folder (e.g. the player's chosen Dropbox path)
  -> String       -- ^ scenario name (matches 'scenarioName')
  -> PlayerId
  -> IO SharedBroadcastResult
broadcastLog sharedDirRaw scenName (PlayerId pidStr) = do
  sharedDir <- expandSharedPath sharedDirRaw
  let localLog  = sessionsRootDir </> scenName </> pidStr </> logFileName
      localSnap = sessionsRootDir </> scenName </> pidStr </> snapshotFileName
      dstDir    = sharedDir </> pidStr
      dstLog    = dstDir </> logFileName
      dstSnap   = dstDir </> snapshotFileName
      dstMeta   = dstDir </> sharedScenarioFile
  haveLog <- doesFileExist localLog
  if not haveLog
    then pure BroadcastNoLog
    else do
      r <- try $ do
        createDirectoryIfMissing True dstDir
        copyFile localLog dstLog
        haveSnap <- doesFileExist localSnap
        when haveSnap (copyFile localSnap dstSnap)
        writeFile dstMeta scenName
      pure $ case r :: Either SomeException () of
        Left e  -> BroadcastFailed (show e)
        Right _ -> Broadcast dstLog

-- | Scan a shared folder for foreign players' logs belonging to the
-- given scenario.  Mirrors the shape of
-- 'Engine.Sync.EventLog.scanForeignLogs' so the runtime can fold the
-- result into the existing merge pipeline.  Logs without a valid
-- 'scenario.txt' marker are skipped — we don't guess.
scanSharedLogs
  :: FilePath            -- ^ shared folder
  -> String              -- ^ scenario name to filter on
  -> PlayerId            -- ^ the current player (own logs are ignored)
  -> IO [(PlayerId, [LogEntry], Maybe Snapshot)]
scanSharedLogs sharedDirRaw scenName (PlayerId ownId) = do
  sharedDir <- expandSharedPath sharedDirRaw
  isDir <- doesDirectoryExist sharedDir
  if not isDir
    then pure []
    else do
      r <- try (listDirectory sharedDir) :: IO (Either SomeException [FilePath])
      let subdirs = fromRight [] r
          -- Exclude own id and hidden entries (".DS_Store" etc.)
          others  = filter (\d -> d /= ownId && not ("." `isPrefixOf` d)) subdirs
      catMaybes <$> forM others (loadOne sharedDir scenName)
  where
    loadOne dir name pid = do
      let pDir   = dir </> pid
          logP   = pDir </> logFileName
          snapP  = pDir </> snapshotFileName
          metaP  = pDir </> sharedScenarioFile
      metaOk  <- scenarioMatches metaP name
      logOk   <- doesFileExist logP
      if not (metaOk && logOk)
        then pure Nothing
        else do
          entries <- loadLog logP
          mSnap   <- loadSnapshot snapP
          pure (Just (PlayerId pid, entries, mSnap))

-- | Verify the shared-dir marker names the expected scenario.
-- Missing or mismatched markers are treated as 'False' so a stray
-- folder dropped into Dropbox doesn't crash the merge.
scenarioMatches :: FilePath -> String -> IO Bool
scenarioMatches path expected = do
  e <- doesFileExist path
  if not e
    then pure False
    else do
      r <- try (readFile path) :: IO (Either SomeException String)
      pure $ case r of
        Left  _ -> False
        Right s -> trim s == trim expected
  where
    trim = reverse . dropWhile (`elem` (" \t\r\n" :: String))
         . reverse . dropWhile (`elem` (" \t\r\n" :: String))

-- | Expand a leading @~@ or @~\/@ to the player's home directory.
-- Anything else is returned unchanged.  Makes preset paths like
-- @~/Dropbox/throughline@ work on the user's real filesystem.
expandSharedPath :: FilePath -> IO FilePath
expandSharedPath p = case p of
  '~':'/':rest -> do
    home <- getHomeDirectory
    pure (home </> rest)
  "~"          -> getHomeDirectory
  _            -> pure p
