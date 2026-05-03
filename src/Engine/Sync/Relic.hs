{-# LANGUAGE OverloadedStrings #-}

-- | Top-level client API for the Tier-3 relic protocol.
--
-- This module wraps the wire types in "Engine.Sync.Relic.Types"
-- with a pluggable HTTP transport and a JSON-file-backed local
-- store.  The transport is deliberately a record of functions
-- rather than a type class: it keeps the library build free of
-- HTTP dependencies while letting downstream code swap in a real
-- @http-client@ (or Nostr, or player-hosted-oracle) backend
-- without touching scenario code.
--
-- See "Engine.Sync.Relic.Shamir" for the secret-sharing primitives.
--
-- The default 'stubTransport' returns 'NotConfigured' for every
-- call, which is the right shape for the current build: players
-- with no oracle configured still get a working engine, just with
-- every relic call a no-op.
module Engine.Sync.Relic
  ( -- * Transport
    RelicTransport (..)
  , RelicTransportError (..)
  , RelicOutcome
  , stubTransport
    -- * Local storage
  , FragmentStore
  , defaultFragmentStoreDir
  , defaultBundleStoreDir
  , loadFragments
  , loadFragmentsFor
  , saveFragment
  , deleteFragment
  , loadBundle
  , saveBundle
  , loadAllBundles
    -- * Re-exports
  , module Engine.Sync.Relic.Types
  ) where

import           Control.Monad           (when)
import qualified Data.Aeson              as Aeson
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Map.Strict         as Map
import           Data.Map.Strict         (Map)
import           Data.Maybe              (catMaybes)
import qualified Data.Text               as T
import           System.Directory        (createDirectoryIfMissing,
                                          doesDirectoryExist, doesFileExist,
                                          getHomeDirectory, listDirectory,
                                          removeFile)
import           System.FilePath         ((</>), (<.>), takeExtension)

import           Engine.Sync.Relic.Types

-- ---------------------------------------------------------------------------
-- Transport
-- ---------------------------------------------------------------------------

-- | A non-protocol transport failure (network, TLS, timeout, etc.)
-- plus a catch-all for "no transport configured" so stubs can say
-- "no" without pretending to be a network error.
data RelicTransportError
  = NotConfigured
  | TransportFailure !T.Text
  | BadResponse      !T.Text
  deriving (Eq, Show)

-- | The result of a relic call is either:
--
--   * a protocol error the oracle returned (rate limits, bad
--     signatures, stale attestations),
--   * a transport error (no server, network failure),
--   * or the success payload.
type RelicOutcome a = IO (Either (Either RelicTransportError RelicError) a)

-- | A relic transport — four functions matching the OpenAPI
-- endpoints the client actually needs.  Swap @'stubTransport'@ for
-- an HTTP-backed or Nostr-backed implementation at launcher time.
-- Scenario code only ever sees this record, not any underlying HTTP
-- library, so the engine build can stay light on deps.
data RelicTransport = RelicTransport
  { transportClaim      :: ClaimRequest    -> RelicOutcome ClaimResponse
  , transportTransfer   :: TransferRequest -> RelicOutcome TransferResponse
  , transportCombine    :: CombineRequest  -> RelicOutcome CombineResponse
  , transportGetBundle  :: BundleId        -> RelicOutcome Bundle
  }

-- | The null transport.  Every call returns 'NotConfigured'.
-- Lets the engine build without a real oracle and lets scenarios
-- call into 'RelicTransport' unconditionally — a missing oracle is
-- just "no relics for you right now", not a crash.
stubTransport :: RelicTransport
stubTransport = RelicTransport
  { transportClaim     = \_ -> pure (Left (Left NotConfigured))
  , transportTransfer  = \_ -> pure (Left (Left NotConfigured))
  , transportCombine   = \_ -> pure (Left (Left NotConfigured))
  , transportGetBundle = \_ -> pure (Left (Left NotConfigured))
  }

-- ---------------------------------------------------------------------------
-- Local storage
-- ---------------------------------------------------------------------------

-- | Type alias for an in-memory view of the fragments a player
-- holds, grouped by set.  The on-disk format is one file per
-- fragment, keyed by share id — this type is the fold of the
-- directory.
type FragmentStore = Map SetId [Fragment]

-- | Default on-disk location for fragments:
-- @$HOME/.local/share/throughline/relic/fragments/@.
defaultFragmentStoreDir :: IO FilePath
defaultFragmentStoreDir = do
  home <- getHomeDirectory
  pure (home </> ".local" </> "share" </> "throughline" </> "relic" </> "fragments")

-- | Default on-disk location for unlocked bundles.
defaultBundleStoreDir :: IO FilePath
defaultBundleStoreDir = do
  home <- getHomeDirectory
  pure (home </> ".local" </> "share" </> "throughline" </> "relic" </> "bundles")

-- | Read every fragment in the store, grouped by set.  Malformed
-- files are skipped silently — the store is an append-only cache,
-- and the authoritative state lives on the oracle.
loadFragments :: FilePath -> IO FragmentStore
loadFragments dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure Map.empty
    else do
      entries <- listDirectory dir
      let jsonFiles = [ dir </> e | e <- entries, takeExtension e == ".json" ]
      fragMaybes <- mapM readOne jsonFiles
      let frags = catMaybes fragMaybes
      pure (foldr insertFrag Map.empty frags)
  where
    readOne p = do
      bs <- BL.readFile p
      pure (Aeson.decode bs :: Maybe Fragment)

    insertFrag f = Map.insertWith (++) (fragmentSetId f) [f]

-- | Fragments the player holds for a single set.
loadFragmentsFor :: FilePath -> SetId -> IO [Fragment]
loadFragmentsFor dir sid = do
  store <- loadFragments dir
  pure (Map.findWithDefault [] sid store)

-- | Persist a fragment to the store.  One file per share id —
-- overwrites any existing file (which is the correct behaviour
-- when a transfer in yields a new attestation for a share we
-- already held).
saveFragment :: FilePath -> Fragment -> IO ()
saveFragment dir f = do
  createDirectoryIfMissing True dir
  let path = fragmentPath dir (fragmentShare f)
  BL.writeFile path (Aeson.encode f)

-- | Remove a fragment from the store.  Used when transferring out;
-- the local copy becomes useless once the oracle revokes the
-- attestation, so we purge it.  No-op if the file doesn't exist.
deleteFragment :: FilePath -> ShareId -> IO ()
deleteFragment dir sid = do
  let path = dir </> T.unpack (unShareId sid) <.> "json"
  exists <- doesFileExist path
  when exists (removeFile path)

-- | Path for a share's fragment file within the store directory.
fragmentPath :: FilePath -> Share -> FilePath
fragmentPath dir s = dir </> T.unpack (unShareId (shareId s)) <.> "json"

-- ---------------------------------------------------------------------------
-- Bundle store
-- ---------------------------------------------------------------------------

-- | Read a cached bundle by id.  Returns 'Nothing' for unknown ids
-- or malformed files.
loadBundle :: FilePath -> BundleId -> IO (Maybe Bundle)
loadBundle dir bid = do
  let path = dir </> T.unpack (unBundleId bid) <.> "json"
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      bs <- BL.readFile path
      pure (Aeson.decode bs)

-- | Persist a bundle so it's available offline.  Overwrites on
-- re-fetch — bundles are immutable under a given @(bundleId,
-- keyId)@, but the oracle may rotate keys, and a later response
-- with a newer signature should replace the older one.
saveBundle :: FilePath -> Bundle -> IO ()
saveBundle dir b = do
  createDirectoryIfMissing True dir
  let path = dir </> T.unpack (unBundleId (bundleId b)) <.> "json"
  BL.writeFile path (Aeson.encode b)

-- | Read every cached bundle.  Used by the journal's Relics tab to
-- enumerate what the player has unlocked without a round-trip to
-- the oracle.
loadAllBundles :: FilePath -> IO [Bundle]
loadAllBundles dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure []
    else do
      entries <- listDirectory dir
      let jsonFiles = [ dir </> e | e <- entries, takeExtension e == ".json" ]
      maybes <- mapM readOne jsonFiles
      pure (catMaybes maybes)
  where
    readOne p = do
      bs <- BL.readFile p
      pure (Aeson.decode bs :: Maybe Bundle)
