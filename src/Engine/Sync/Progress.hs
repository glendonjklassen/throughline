{-# LANGUAGE OverloadedStrings #-}

-- | Per-identity, local-only play-progress tracking.  Each 'Progress'
-- record stores an 'progressEpoch' (a rotation counter for long-lived
-- mechanics) and a 'progressHuntCount' (the number of hunts the
-- player has started under the current epoch).
--
-- This is Phase 2 scaffolding from the @unique-finds@ proposal — it
-- unlocks the Tier-2 lifetime find (the white stag), whose stature
-- reads off 'progressHuntCount' and whose rotation bumps
-- 'progressEpoch'.  Phase 1 signature finds do not need this module.
--
-- Storage is a single JSON file alongside the identity key file,
-- keyed by the player's public-key hex.  Safe to edit by hand; new
-- fields can be added with defaults without breaking older files.
module Engine.Sync.Progress
  ( Progress (..)
  , defaultProgress
  , defaultProgressPath
  , loadAll
  , saveAll
  , getProgress
  , recordHunt
  , rotateEpoch
  ) where

import qualified Data.Aeson              as Aeson
import           Data.Aeson              (FromJSON, ToJSON, (.=), (.:), (.:?),
                                          (.!=))
import qualified Data.Aeson              as A
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Map.Strict         as Map
import           Data.Map.Strict         (Map)
import           Data.Time.Clock         (UTCTime, getCurrentTime)
import           System.Directory        (createDirectoryIfMissing, doesFileExist,
                                          getHomeDirectory)
import           System.FilePath         ((</>), takeDirectory)

import           GameTypes               (PlayerId (..))

-- | Per-identity play progress.  Starts at 'defaultProgress' on first
-- play.  All counters are monotone under normal use; only
-- 'rotateEpoch' resets 'progressHuntCount' back to zero.
data Progress = Progress
  { progressEpoch      :: !Int
    -- ^ Rotation counter.  Starts at 1.  Bumped when a lifetime-find
    -- is claimed or the player explicitly requests a reset.
  , progressHuntCount  :: !Int
    -- ^ Hunts started under the current epoch.  Zero-initialized,
    -- incremented at each @recordHunt@ call, reset to zero on
    -- 'rotateEpoch'.
  , progressUpdatedAt  :: !UTCTime
    -- ^ Wall-clock timestamp of the last mutation.  Informational —
    -- not used for invariants, but surfaced in the progress file so
    -- the user can spot stale state.
  } deriving (Show, Eq)

instance ToJSON Progress where
  toJSON p = A.object
    [ "epoch"     .= progressEpoch p
    , "huntCount" .= progressHuntCount p
    , "updatedAt" .= progressUpdatedAt p
    ]

instance FromJSON Progress where
  parseJSON = A.withObject "Progress" $ \o -> Progress
    <$> o .:? "epoch"     .!= 1
    <*> o .:? "huntCount" .!= 0
    <*> o .:  "updatedAt"

-- | A brand-new record with epoch 1 and no hunts yet.  'progressUpdatedAt'
-- is the wall-clock time at the moment of construction so the file
-- reflects a real event, not a zero date.
defaultProgress :: IO Progress
defaultProgress = do
  now <- getCurrentTime
  pure Progress
    { progressEpoch     = 1
    , progressHuntCount = 0
    , progressUpdatedAt = now
    }

-- | Canonical on-disk location for the progress file: the same
-- directory as 'Engine.Sync.Identity.defaultIdentityPath'.  Created
-- on first write.
defaultProgressPath :: IO FilePath
defaultProgressPath = do
  home <- getHomeDirectory
  pure (home </> ".local" </> "share" </> "throughline" </> "progress.json")

-- ---------------------------------------------------------------------------
-- File I/O
-- ---------------------------------------------------------------------------

-- | Load every identity's progress record.  Missing file returns an
-- empty map; malformed file also returns empty (we don't corrupt
-- user state by failing to start).
loadAll :: FilePath -> IO (Map PlayerId Progress)
loadAll path = do
  exists <- doesFileExist path
  if not exists
    then pure Map.empty
    else do
      bs <- BL.readFile path
      case Aeson.eitherDecode bs of
        Left _            -> pure Map.empty
        Right (File recs) -> pure (Map.fromList [ (PlayerId k, v) | (k, v) <- recs ])

-- | Persist the full progress map.  Creates parent directories as
-- needed.  Atomic-enough for solo-local use: a single write, no
-- temp-file dance.
saveAll :: FilePath -> Map PlayerId Progress -> IO ()
saveAll path m = do
  createDirectoryIfMissing True (takeDirectory path)
  BL.writeFile path (Aeson.encode (File [ (k, v) | (PlayerId k, v) <- Map.toList m ]))

-- ---------------------------------------------------------------------------
-- Per-player helpers
-- ---------------------------------------------------------------------------

-- | Read one player's progress, returning a fresh default if the
-- identity has no record yet.  Does not persist anything.
getProgress :: PlayerId -> FilePath -> IO Progress
getProgress pid path = do
  all' <- loadAll path
  maybe defaultProgress pure (Map.lookup pid all')

-- | Increment the player's hunt count, persist, and return the new
-- value.  Creates the identity's record if none exists yet.  Call
-- this at hunt start — not scenario init — so aborted start flows
-- don't bump the counter.
recordHunt :: PlayerId -> FilePath -> IO Progress
recordHunt pid path = do
  now   <- getCurrentTime
  all'  <- loadAll path
  base  <- maybe defaultProgress pure (Map.lookup pid all')
  let next = base
        { progressHuntCount = progressHuntCount base + 1
        , progressUpdatedAt = now
        }
  saveAll path (Map.insert pid next all')
  pure next

-- | Bump the epoch, reset the hunt count, persist, return the new
-- value.  Typically called after the player claims their lifetime
-- find or manually requests a new epoch.
rotateEpoch :: PlayerId -> FilePath -> IO Progress
rotateEpoch pid path = do
  now   <- getCurrentTime
  all'  <- loadAll path
  base  <- maybe defaultProgress pure (Map.lookup pid all')
  let next = base
        { progressEpoch     = progressEpoch base + 1
        , progressHuntCount = 0
        , progressUpdatedAt = now
        }
  saveAll path (Map.insert pid next all')
  pure next

-- ---------------------------------------------------------------------------
-- File wrapper
-- ---------------------------------------------------------------------------

-- | Thin wrapper around the stored list so the top-level JSON has a
-- named \"records\" field and a \"version\" stamp — cheap insurance
-- against a future schema change.
newtype File = File [(String, Progress)]

instance ToJSON File where
  toJSON (File recs) = A.object
    [ "version" .= (1 :: Int)
    , "records" .= [ A.object ["playerId" .= k, "progress" .= p] | (k, p) <- recs ]
    ]

instance FromJSON File where
  parseJSON = A.withObject "ProgressFile" $ \o -> do
    rs <- o .:? "records" .!= []
    recs <- mapM parseRecord rs
    pure (File recs)
    where
      parseRecord = A.withObject "ProgressRecord" $ \o ->
        (,) <$> o .: "playerId" <*> o .: "progress"
