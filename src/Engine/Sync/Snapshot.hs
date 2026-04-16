{-# OPTIONS_GHC -fno-hpc #-}
-- | World state snapshot serialization and merge for session recovery.
module Engine.Sync.Snapshot
  ( saveSnapshot
  , loadSnapshot
  , snapshotFileName
  ) where

import qualified Data.Aeson               as Aeson
import qualified Data.ByteString.Lazy     as BL
import           System.Directory         (createDirectoryIfMissing, doesFileExist)
import           System.FilePath          (takeDirectory)

import           GameTypes

snapshotFileName :: FilePath
snapshotFileName = "snapshot.json"

saveSnapshot :: FilePath -> Snapshot -> IO ()
saveSnapshot path snap = do
  createDirectoryIfMissing True (takeDirectory path)
  BL.writeFile path (Aeson.encode snap)

loadSnapshot :: FilePath -> IO (Maybe Snapshot)
loadSnapshot path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else Aeson.decode <$> BL.readFile path
