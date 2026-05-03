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
  , LifetimeFindState (..)
  , defaultProgress
  , defaultProgressPath
  , loadAll
  , saveAll
  , getProgress
  , recordHunt
  , rotateEpoch
  , recordLifetimeClaim
  , recordLifetimePass
  , recordLifetimeLost
  , recordLifetimeLinger
  , gammaThreshold
  , decayThreshold
  , lifetimeFindEligible
  ) where

import qualified Crypto.PubKey.Ed25519   as Ed25519
import qualified Data.Aeson              as Aeson
import           Data.Aeson              (FromJSON, ToJSON, (.=), (.:), (.:?),
                                          (.!=))
import qualified Data.Aeson              as A
import qualified Data.Aeson.Types        as AT
import qualified Data.ByteArray          as BA
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BL
import           Crypto.Hash             (Digest, SHA256, hash)
import qualified Data.Map.Strict         as Map
import           Data.Map.Strict         (Map)
import           Data.Time.Clock         (UTCTime, getCurrentTime)
import           Data.Word               (Word8)
import           System.Directory        (createDirectoryIfMissing, doesFileExist,
                                          getHomeDirectory)
import           System.FilePath         ((</>), takeDirectory)

import           GameTypes               (PlayerId (..))

-- | Per-identity play progress.  Starts at 'defaultProgress' on first
-- play.  All counters are monotone under normal use; only
-- 'rotateEpoch' resets 'progressHuntCount' back to zero.
data Progress = Progress
  { progressEpoch         :: !Int
    -- ^ Rotation counter.  Starts at 1.  Bumped when a lifetime-find
    -- is claimed or the player explicitly requests a reset.
  , progressHuntCount     :: !Int
    -- ^ Hunts started under the current epoch.  Zero-initialized,
    -- incremented at each @recordHunt@ call, reset to zero on
    -- 'rotateEpoch'.
  , progressLifetimeFind  :: !LifetimeFindState
    -- ^ State machine for the lifetime find (the white stag in
    -- DeerHunt, similar mechanics in other scenarios).  See
    -- 'LifetimeFindState' for transitions.
  , progressUpdatedAt     :: !UTCTime
    -- ^ Wall-clock timestamp of the last mutation.  Informational —
    -- not used for invariants, but surfaced in the progress file so
    -- the user can spot stale state.
  } deriving (Show, Eq)

-- | The lifetime find's per-identity state machine.  Transitions are
-- driven from end-of-hunt processing (in IO) by inspecting the world
-- tags written during the hunt.
data LifetimeFindState
  = FindPending
    -- ^ Never encountered.  Each hunt rolls gamma eligibility against
    -- the current 'progressHuntCount'; an eligible hunt that goes
    -- unencountered \"lingers\" — see 'recordLifetimeLinger'.
  | FindEncountered !Int !Int
    -- ^ @FindEncountered firstSeenAtHunt passes@ — the player has met
    -- the find at least once and chosen to pass (or had a forced
    -- pass via fail-claim).  Each subsequent eligible hunt rolls a
    -- decay-shaped re-encounter probability.
  | FindClaimed !Int !UTCTime
    -- ^ @FindClaimed claimedAtHunt at@ — locked in.  The next epoch
    -- will start fresh; this record persists as history.
  | FindLost !Int !UTCTime
    -- ^ @FindLost lostAtHunt at@ — too many passes/failures, the
    -- find is gone for this epoch.  Only a long-tail mercy roll or
    -- a manual epoch reset (after a real-world cooldown) brings it
    -- back.
  deriving (Show, Eq)

instance ToJSON Progress where
  toJSON p = A.object
    [ "epoch"        .= progressEpoch p
    , "huntCount"    .= progressHuntCount p
    , "lifetimeFind" .= progressLifetimeFind p
    , "updatedAt"    .= progressUpdatedAt p
    ]

instance FromJSON Progress where
  parseJSON = A.withObject "Progress" $ \o -> Progress
    <$> o .:? "epoch"        .!= 1
    <*> o .:? "huntCount"    .!= 0
    <*> o .:? "lifetimeFind" .!= FindPending
    <*> o .:  "updatedAt"

instance ToJSON LifetimeFindState where
  toJSON FindPending =
    A.object ["state" .= ("pending" :: String)]
  toJSON (FindEncountered n passes) =
    A.object [ "state" .= ("encountered" :: String)
             , "firstSeenAtHunt" .= n
             , "passes" .= passes
             ]
  toJSON (FindClaimed n at) =
    A.object [ "state" .= ("claimed" :: String)
             , "claimedAtHunt" .= n
             , "at" .= at
             ]
  toJSON (FindLost n at) =
    A.object [ "state" .= ("lost" :: String)
             , "lostAtHunt" .= n
             , "at" .= at
             ]

instance FromJSON LifetimeFindState where
  parseJSON = A.withObject "LifetimeFindState" $ \o -> do
    s <- o .: "state" :: AT.Parser String
    case s of
      "pending"     -> pure FindPending
      "encountered" -> FindEncountered <$> o .: "firstSeenAtHunt" <*> o .:? "passes" .!= 0
      "claimed"     -> FindClaimed <$> o .: "claimedAtHunt" <*> o .: "at"
      "lost"        -> FindLost <$> o .: "lostAtHunt" <*> o .: "at"
      _             -> fail ("Unknown LifetimeFindState: " <> s)

-- | A brand-new record with epoch 1 and no hunts yet.  'progressUpdatedAt'
-- is the wall-clock time at the moment of construction so the file
-- reflects a real event, not a zero date.
defaultProgress :: IO Progress
defaultProgress = do
  now <- getCurrentTime
  pure Progress
    { progressEpoch        = 1
    , progressHuntCount    = 0
    , progressLifetimeFind = FindPending
    , progressUpdatedAt    = now
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

-- | Bump the epoch, reset the hunt count, reset the lifetime find,
-- persist, return the new value.  Typically called after the player
-- claims their lifetime find or manually requests a new epoch.
rotateEpoch :: PlayerId -> FilePath -> IO Progress
rotateEpoch pid path = do
  now   <- getCurrentTime
  all'  <- loadAll path
  base  <- maybe defaultProgress pure (Map.lookup pid all')
  let next = base
        { progressEpoch        = progressEpoch base + 1
        , progressHuntCount    = 0
        , progressLifetimeFind = FindPending
        , progressUpdatedAt    = now
        }
  saveAll path (Map.insert pid next all')
  pure next

-- ---------------------------------------------------------------------------
-- Lifetime find transitions
-- ---------------------------------------------------------------------------

-- | The player claimed their lifetime find this hunt.  Marks the
-- find as claimed at the current 'progressHuntCount' and rotates
-- the epoch (which resets the hunt count for the next pursuit).
recordLifetimeClaim :: PlayerId -> FilePath -> IO Progress
recordLifetimeClaim pid path = do
  now  <- getCurrentTime
  all' <- loadAll path
  base <- maybe defaultProgress pure (Map.lookup pid all')
  let claimed = base
        { progressLifetimeFind = FindClaimed (progressHuntCount base) now
        , progressUpdatedAt    = now
        }
      rotated = claimed
        { progressEpoch        = progressEpoch claimed + 1
        , progressHuntCount    = 0
        , progressLifetimeFind = FindPending
        }
  saveAll path (Map.insert pid rotated all')
  pure rotated

-- | The player saw the find and let it pass (or fumbled the claim).
-- Bumps the pass counter; once it exceeds 'lostThreshold' the find
-- transitions to 'FindLost'.
recordLifetimePass :: PlayerId -> FilePath -> IO Progress
recordLifetimePass pid path = do
  now  <- getCurrentTime
  all' <- loadAll path
  base <- maybe defaultProgress pure (Map.lookup pid all')
  let n = progressHuntCount base
      next' = case progressLifetimeFind base of
        FindPending           -> FindEncountered n 1
        FindEncountered f p   ->
          let p' = p + 1
          in if p' >= lostThreshold
               then FindLost n now
               else FindEncountered f p'
        s@FindClaimed{}       -> s   -- already claimed; no-op
        s@FindLost{}          -> s   -- already lost; no-op
      next = base
        { progressLifetimeFind = next'
        , progressUpdatedAt    = now
        }
  saveAll path (Map.insert pid next all')
  pure next

-- | Force the find to 'FindLost' state.  Called when the long decay
-- has effectively dropped the re-encounter probability below the
-- single-hunt floor.
recordLifetimeLost :: PlayerId -> FilePath -> IO Progress
recordLifetimeLost pid path = do
  now  <- getCurrentTime
  all' <- loadAll path
  base <- maybe defaultProgress pure (Map.lookup pid all')
  let next = base
        { progressLifetimeFind = FindLost (progressHuntCount base) now
        , progressUpdatedAt    = now
        }
  saveAll path (Map.insert pid next all')
  pure next

-- | Roll back the hunt counter by one.  Used when a hunt was eligible
-- for the lifetime find but the player never crossed its path —
-- per the proposal, the find lingers and the hunt doesn't \"burn\" N.
recordLifetimeLinger :: PlayerId -> FilePath -> IO Progress
recordLifetimeLinger pid path = do
  now  <- getCurrentTime
  all' <- loadAll path
  base <- maybe defaultProgress pure (Map.lookup pid all')
  let next = base
        { progressHuntCount = max 0 (progressHuntCount base - 1)
        , progressUpdatedAt = now
        }
  saveAll path (Map.insert pid next all')
  pure next

-- | Pass count at which the find transitions to 'FindLost'.
lostThreshold :: Int
lostThreshold = 10

-- ---------------------------------------------------------------------------
-- Eligibility math
-- ---------------------------------------------------------------------------

-- | Per-thousand integer threshold for the gamma-shaped eligibility
-- distribution.  Centered around hunts 11–25.  Returns the
-- probability times 1000 that a hunt at counter @n@ is eligible
-- (assuming the find is 'FindPending').  Calibrated to roughly
-- match the proposal's per-bucket targets:
--
-- * @P(N = 1–3)  ~  4%@  (rare — Yearling, true bad luck)
-- * @P(N = 4–10) ~ 22%@  (Prime, early end)
-- * @P(N = 11–25) ~ 45%@ (Prime/Elder, the common case)
-- * @P(N = 26–50) ~ 23%@
-- * @P(N = 51–100) ~ 5%@
-- * @P(N > 100)   ~ 1%@
gammaThreshold :: Int -> Int
gammaThreshold n
  | n <= 0    = 0
  | n <= 3    = 14    -- ~1.4% per hunt for ~4% across 3 hunts
  | n <= 10   = 32    -- ~3.2% per hunt for ~22% across 7
  | n <= 25   = 41    -- ~4.1% per hunt for ~45% across 15
  | n <= 50   = 11    -- ~1.1% per hunt for ~23% across 25
  | n <= 100  = 1     -- ~0.1% per hunt for ~5% across 50
  | otherwise = 1     -- long tail

-- | Decay threshold (per-thousand) for re-encounter after a pass.
-- Compounds downward: 200, 160, 128, 102, 82, ... matching the
-- proposal's \"20% per subsequent hunt\" decay.
decayThreshold :: Int -> Int
decayThreshold passes
  | passes <= 0 = 0
  | otherwise   = max 1 (round (1000 * (0.2 :: Double) ^ passes))

-- | Decide whether this hunt is eligible to surface the lifetime find.
-- The check is deterministic given pubkey, epoch, hunt counter, and
-- find state — replays converge.
--
-- Branches:
--
-- * 'FindPending':       gamma roll based on @n@.
-- * 'FindEncountered':   decay-curve roll based on @passes@.
-- * 'FindClaimed':       never eligible (already claimed; epoch will
--   rotate at the next 'recordLifetimeClaim').
-- * 'FindLost':          long-tail mercy at 1-in-500 hunts.
lifetimeFindEligible :: Ed25519.PublicKey
                     -> Int    -- ^ epoch
                     -> Int    -- ^ current hunt counter (after increment)
                     -> LifetimeFindState
                     -> Bool
lifetimeFindEligible pubkey epoch n state =
  case state of
    FindPending           -> roll < gammaThreshold n
    FindEncountered _ p   -> roll < decayThreshold p
    FindClaimed{}         -> False
    FindLost{}            -> roll < 2  -- ~1-in-500 mercy reappearance
  where
    roll = eligibilityRoll pubkey epoch n

-- | Deterministic per-thousand draw from @hash(pubkey || epoch || n)@.
-- The first 4 bytes of the SHA-256 digest, taken mod 1000.
eligibilityRoll :: Ed25519.PublicKey -> Int -> Int -> Int
eligibilityRoll pubkey epoch n =
  let pk = BA.convert pubkey :: BS.ByteString
      input = pk
           <> BS.pack (intBytes epoch)
           <> BS.pack (intBytes n)
      digest :: Digest SHA256
      digest = hash input
      bs     = BA.convert digest :: BS.ByteString
      b0 = fromIntegral (BS.index bs 0) :: Int
      b1 = fromIntegral (BS.index bs 1) :: Int
      b2 = fromIntegral (BS.index bs 2) :: Int
      b3 = fromIntegral (BS.index bs 3) :: Int
      raw = (b0 * 0x1000000) + (b1 * 0x10000) + (b2 * 0x100) + b3
  in raw `mod` 1000

-- | Little-endian byte serialization for an Int.  Eight bytes is
-- enough for any realistic hunt count or epoch.  Used as input to
-- the eligibility hash so the roll is deterministic across machines.
intBytes :: Int -> [Word8]
intBytes x =
  [ fromIntegral (x `shiftR` (8 * i) .&. 0xff) | i <- [0..7 :: Int] ]
  where
    shiftR a b = a `div` (2 ^ b)
    (.&.) a b  = a `mod` (b + 1)

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
