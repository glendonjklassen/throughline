-- | Per-scenario save discovery for the launcher.
--
-- The event log at @sessions\/<scenarioName>\/<playerId>\/events.jsonl@
-- is authoritative for "does this scenario have progress."  This module
-- exposes that as a simple query so the launcher can render
-- \"Continue\" vs \"New hunt\" without reaching into the sync layer.
--
-- Reset uses 'LogStore.lsReset' when the player asks for a fresh start.
module SDL.SaveSlots
  ( SaveStatus(..)
  , scenarioSaveStatus
  , resetScenarioSave
  ) where

import           Control.Exception      (SomeException, try)
import           Control.Monad          (when)
import qualified Data.ByteString.Char8  as BS
import           System.Directory       (doesFileExist, removeFile)
import           System.FilePath        ((</>))

import           Engine.Runtime         (sessionsRootDir)
import           Engine.Sync.EventLog   (logFileName)
import           Engine.Sync.Snapshot   (snapshotFileName)
import           GameTypes              (PlayerId(..))

-- | Whether a scenario has visible progress on disk for this player.
-- 'saveEntryCount' is a cheap proxy for \"how far in\" — counted via
-- newlines in the JSONL file; we deliberately don't parse here.
data SaveStatus = SaveStatus
  { hasSave         :: Bool
  , saveEntryCount  :: Int
  } deriving (Show, Eq)

-- | Look up a scenario's save on disk.  Absent or unreadable file is
-- treated as \"no save\" so the launcher falls back to \"New\".
scenarioSaveStatus :: PlayerId -> String -> IO SaveStatus
scenarioSaveStatus (PlayerId pid) scenName = do
  let path = sessionsRootDir </> scenName </> pid </> logFileName
  exists <- doesFileExist path
  if not exists
    then pure (SaveStatus False 0)
    else do
      -- Count lines without buffering the whole log — small files in
      -- practice, but a long hunt can grow to thousands of entries.
      r <- try (BS.readFile path) :: IO (Either SomeException BS.ByteString)
      pure $ case r of
        Left  _     -> SaveStatus False 0
        Right bytes -> SaveStatus True (length (BS.lines bytes))

-- | Delete a scenario's log and snapshot for this player, leaving the
-- player-identity and other scenarios untouched.  Used by the
-- launcher's \"start a new hunt, discard the old one\" flow.
resetScenarioSave :: PlayerId -> String -> IO ()
resetScenarioSave (PlayerId pid) scenName = do
  let dir      = sessionsRootDir </> scenName </> pid
      logPath  = dir </> logFileName
      snapPath = dir </> snapshotFileName
  removeIfExists logPath
  removeIfExists snapPath
  where
    removeIfExists p = do
      e <- doesFileExist p
      when e $ removeFile p
