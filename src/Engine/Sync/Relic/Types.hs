{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Wire types for the relic oracle protocol.  These mirror the
-- OpenAPI spec at @api/relic-oracle.openapi.yaml@ field-for-field;
-- changes to the YAML must be mirrored here and vice-versa.
--
-- Nothing in this module does I/O — pure types, JSON, and trivial
-- helpers only.  'Engine.Sync.Relic' wraps these with the transport
-- layer and the local persistence story.
module Engine.Sync.Relic.Types
  ( -- * Core identifiers
    SetId (..)
  , CacheId (..)
  , ShareId (..)
  , BundleId (..)
  , OracleKeyId (..)
    -- * Shares, attestations, fragments
  , Share (..)
  , Attestation (..)
  , Fragment (..)
    -- * Claim
  , ClaimRequest (..)
  , ClaimProof (..)
  , ClaimResponse (..)
    -- * Transfer
  , TransferIntent (..)
  , TransferAccept (..)
  , TransferRequest (..)
  , TransferResponse (..)
    -- * Combine
  , CombineRequest (..)
  , CombineResponse (..)
    -- * Bundles
  , BundleKind (..)
  , Bundle (..)
    -- * Metadata
  , SetMetadata (..)
  , TrustList (..)
  , TrustKey (..)
  , HealthResponse (..)
    -- * Errors
  , RelicError (..)
  , ErrorCode (..)
  ) where

import           Data.Aeson          (FromJSON (..), ToJSON (..), (.=), (.:),
                                      (.:?))
import qualified Data.Aeson          as A
import           Data.Text           (Text)
import           Data.Time.Clock     (UTCTime)
import           GHC.Generics        (Generic)

-- ---------------------------------------------------------------------------
-- Identifier newtypes
-- ---------------------------------------------------------------------------

-- | A relic set's stable identifier (e.g. @\"whisper\"@).
newtype SetId = SetId { unSetId :: Text }
  deriving (Eq, Ord, Show, Generic)
instance ToJSON   SetId where toJSON (SetId s) = toJSON s
instance FromJSON SetId where parseJSON v = SetId <$> parseJSON v

-- | A cache's stable identifier — @hex(hash(worldSeed||huntSeed||location))@.
newtype CacheId = CacheId { unCacheId :: Text }
  deriving (Eq, Ord, Show, Generic)
instance ToJSON   CacheId where toJSON (CacheId s) = toJSON s
instance FromJSON CacheId where parseJSON v = CacheId <$> parseJSON v

-- | A share's stable identifier — unique within a set.
newtype ShareId = ShareId { unShareId :: Text }
  deriving (Eq, Ord, Show, Generic)
instance ToJSON   ShareId where toJSON (ShareId s) = toJSON s
instance FromJSON ShareId where parseJSON v = ShareId <$> parseJSON v

-- | A bundle's stable identifier.
newtype BundleId = BundleId { unBundleId :: Text }
  deriving (Eq, Ord, Show, Generic)
instance ToJSON   BundleId where toJSON (BundleId s) = toJSON s
instance FromJSON BundleId where parseJSON v = BundleId <$> parseJSON v

-- | An oracle-key identifier — short human-readable string like
-- @\"oracle-2026-q2\"@.  Used so clients can look up the right
-- verification key in the trust list when a signature arrives.
newtype OracleKeyId = OracleKeyId { unOracleKeyId :: Text }
  deriving (Eq, Ord, Show, Generic)
instance ToJSON   OracleKeyId where toJSON (OracleKeyId s) = toJSON s
instance FromJSON OracleKeyId where parseJSON v = OracleKeyId <$> parseJSON v

-- ---------------------------------------------------------------------------
-- Shares, attestations, fragments
-- ---------------------------------------------------------------------------

-- | The raw share: an x-coordinate and the share bytes.  The share
-- data is public (deterministic from the genesis seed); only the
-- attestation binds it to an owner.
data Share = Share
  { shareId    :: !ShareId
  , shareX     :: !Int          -- ^ GF(256) x-coordinate, 1..255.
  , shareBytes :: !Text         -- ^ Hex-encoded share bytes.
  } deriving (Eq, Show, Generic)

instance ToJSON Share where
  toJSON s = A.object
    [ "shareId" .= shareId s
    , "x"       .= shareX s
    , "bytes"   .= shareBytes s
    ]
instance FromJSON Share where
  parseJSON = A.withObject "Share" $ \o -> Share
    <$> o .: "shareId"
    <*> o .: "x"
    <*> o .: "bytes"

-- | The signed statement that a pubkey currently owns a share.
-- Every transfer invalidates the old attestation (by bumping
-- 'attestationSerial') and issues a new one.
data Attestation = Attestation
  { attestationShareId     :: !ShareId
  , attestationOwnerPubkey :: !Text      -- ^ Hex-encoded Ed25519 pubkey.
  , attestationSerial      :: !Int
  , attestationIssuedAt    :: !UTCTime
  , attestationSignature   :: !Text      -- ^ Hex-encoded Ed25519 signature.
  , attestationKeyId       :: !OracleKeyId
  } deriving (Eq, Show, Generic)

instance ToJSON Attestation where
  toJSON a = A.object
    [ "shareId"     .= attestationShareId a
    , "ownerPubkey" .= attestationOwnerPubkey a
    , "serial"      .= attestationSerial a
    , "issuedAt"    .= attestationIssuedAt a
    , "signature"   .= attestationSignature a
    , "keyId"       .= attestationKeyId a
    ]
instance FromJSON Attestation where
  parseJSON = A.withObject "Attestation" $ \o -> Attestation
    <$> o .: "shareId"
    <*> o .: "ownerPubkey"
    <*> o .: "serial"
    <*> o .: "issuedAt"
    <*> o .: "signature"
    <*> o .: "keyId"

-- | What the client actually stores: a share together with its
-- current attestation.  The pair is what makes a fragment
-- "combinable".
data Fragment = Fragment
  { fragmentSetId       :: !SetId
  , fragmentShare       :: !Share
  , fragmentAttestation :: !Attestation
  } deriving (Eq, Show, Generic)

instance ToJSON Fragment where
  toJSON f = A.object
    [ "setId"       .= fragmentSetId f
    , "share"       .= fragmentShare f
    , "attestation" .= fragmentAttestation f
    ]
instance FromJSON Fragment where
  parseJSON = A.withObject "Fragment" $ \o -> Fragment
    <$> o .: "setId"
    <*> o .: "share"
    <*> o .: "attestation"

-- ---------------------------------------------------------------------------
-- Claim
-- ---------------------------------------------------------------------------

-- | Minimal "I was here" attestation the oracle uses to spot-check
-- claim validity.  Stand-in for a real VRF proof in later versions.
data ClaimProof = ClaimProof
  { claimProofHuntSeed :: !Int
  , claimProofLocation :: !Text
  , claimProofTick     :: !Int
  } deriving (Eq, Show, Generic)

instance ToJSON ClaimProof where
  toJSON p = A.object
    [ "huntSeed" .= claimProofHuntSeed p
    , "location" .= claimProofLocation p
    , "tick"     .= claimProofTick p
    ]
instance FromJSON ClaimProof where
  parseJSON = A.withObject "ClaimProof" $ \o -> ClaimProof
    <$> o .: "huntSeed"
    <*> o .: "location"
    <*> o .: "tick"

data ClaimRequest = ClaimRequest
  { claimCacheId      :: !CacheId
  , claimPlayerPubkey :: !Text
  , claimProof        :: !ClaimProof
  , claimSignature    :: !Text
  } deriving (Eq, Show, Generic)

instance ToJSON ClaimRequest where
  toJSON r = A.object
    [ "cacheId"      .= claimCacheId r
    , "playerPubkey" .= claimPlayerPubkey r
    , "proof"        .= claimProof r
    , "signature"    .= claimSignature r
    ]
instance FromJSON ClaimRequest where
  parseJSON = A.withObject "ClaimRequest" $ \o -> ClaimRequest
    <$> o .: "cacheId"
    <*> o .: "playerPubkey"
    <*> o .: "proof"
    <*> o .: "signature"

newtype ClaimResponse = ClaimResponse { claimResponseFragment :: Fragment }
  deriving (Eq, Show, Generic)
instance ToJSON ClaimResponse where
  toJSON (ClaimResponse f) = A.object ["fragment" .= f]
instance FromJSON ClaimResponse where
  parseJSON = A.withObject "ClaimResponse" (\o -> ClaimResponse <$> o .: "fragment")

-- ---------------------------------------------------------------------------
-- Transfer
-- ---------------------------------------------------------------------------

data TransferIntent = TransferIntent
  { transferIntentShareId    :: !ShareId
  , transferIntentFromPubkey :: !Text
  , transferIntentToPubkey   :: !Text
  , transferIntentNonce      :: !Text
  , transferIntentIssuedAt   :: !UTCTime
  , transferIntentSignature  :: !Text
  } deriving (Eq, Show, Generic)

instance ToJSON TransferIntent where
  toJSON i = A.object
    [ "shareId"    .= transferIntentShareId i
    , "fromPubkey" .= transferIntentFromPubkey i
    , "toPubkey"   .= transferIntentToPubkey i
    , "nonce"      .= transferIntentNonce i
    , "issuedAt"   .= transferIntentIssuedAt i
    , "signature"  .= transferIntentSignature i
    ]
instance FromJSON TransferIntent where
  parseJSON = A.withObject "TransferIntent" $ \o -> TransferIntent
    <$> o .: "shareId"
    <*> o .: "fromPubkey"
    <*> o .: "toPubkey"
    <*> o .: "nonce"
    <*> o .: "issuedAt"
    <*> o .: "signature"

data TransferAccept = TransferAccept
  { transferAcceptIntentHash :: !Text
  , transferAcceptToPubkey   :: !Text
  , transferAcceptSignature  :: !Text
  } deriving (Eq, Show, Generic)

instance ToJSON TransferAccept where
  toJSON a = A.object
    [ "intentHash" .= transferAcceptIntentHash a
    , "toPubkey"   .= transferAcceptToPubkey a
    , "signature"  .= transferAcceptSignature a
    ]
instance FromJSON TransferAccept where
  parseJSON = A.withObject "TransferAccept" $ \o -> TransferAccept
    <$> o .: "intentHash"
    <*> o .: "toPubkey"
    <*> o .: "signature"

data TransferRequest = TransferRequest
  { transferIntent :: !TransferIntent
  , transferAccept :: !TransferAccept
  } deriving (Eq, Show, Generic)

instance ToJSON TransferRequest where
  toJSON r = A.object
    [ "intent" .= transferIntent r
    , "accept" .= transferAccept r
    ]
instance FromJSON TransferRequest where
  parseJSON = A.withObject "TransferRequest" $ \o -> TransferRequest
    <$> o .: "intent"
    <*> o .: "accept"

newtype TransferResponse = TransferResponse { transferResponseAttestation :: Attestation }
  deriving (Eq, Show, Generic)
instance ToJSON TransferResponse where
  toJSON (TransferResponse a) = A.object ["newAttestation" .= a]
instance FromJSON TransferResponse where
  parseJSON = A.withObject "TransferResponse"
    (\o -> TransferResponse <$> o .: "newAttestation")

-- ---------------------------------------------------------------------------
-- Combine
-- ---------------------------------------------------------------------------

data CombineRequest = CombineRequest
  { combineSetId           :: !SetId
  , combineCombinerPubkeys :: ![Text]
  , combineAttestations    :: ![Attestation]
  , combineSignature       :: !Text
  } deriving (Eq, Show, Generic)

instance ToJSON CombineRequest where
  toJSON r = A.object
    [ "setId"           .= combineSetId r
    , "combinerPubkeys" .= combineCombinerPubkeys r
    , "attestations"    .= combineAttestations r
    , "signature"       .= combineSignature r
    ]
instance FromJSON CombineRequest where
  parseJSON = A.withObject "CombineRequest" $ \o -> CombineRequest
    <$> o .: "setId"
    <*> o .: "combinerPubkeys"
    <*> o .: "attestations"
    <*> o .: "signature"

newtype CombineResponse = CombineResponse { combineResponseBundle :: Bundle }
  deriving (Eq, Show, Generic)
instance ToJSON CombineResponse where
  toJSON (CombineResponse b) = A.object ["bundle" .= b]
instance FromJSON CombineResponse where
  parseJSON = A.withObject "CombineResponse"
    (\o -> CombineResponse <$> o .: "bundle")

-- ---------------------------------------------------------------------------
-- Bundles
-- ---------------------------------------------------------------------------

-- | The four bundle shapes the oracle can return.  See the proposal
-- for what each one does to scenario rendering.  Open enum: clients
-- should treat unknown kinds as 'BundleUnknown' rather than fail.
data BundleKind
  = BundleLore
  | BundleNamedCharacter
  | BundleMapReveal
  | BundleCapability
  | BundleUnknown !Text
  deriving (Eq, Show, Generic)

instance ToJSON BundleKind where
  toJSON BundleLore            = "lore"
  toJSON BundleNamedCharacter  = "named-character"
  toJSON BundleMapReveal       = "map-reveal"
  toJSON BundleCapability      = "capability"
  toJSON (BundleUnknown t)     = toJSON t
instance FromJSON BundleKind where
  parseJSON = A.withText "BundleKind" $ \t -> pure (case t of
    "lore"            -> BundleLore
    "named-character" -> BundleNamedCharacter
    "map-reveal"      -> BundleMapReveal
    "capability"      -> BundleCapability
    other             -> BundleUnknown other)

data Bundle = Bundle
  { bundleId              :: !BundleId
  , bundleSetId           :: !SetId
  , bundleKind            :: !BundleKind
  , bundleTitle           :: !Text
  , bundleBody            :: !A.Value
    -- ^ Shape depends on 'bundleKind'; keep as an opaque 'Value'
    -- so adding new body fields doesn't break older parsers.
  , bundleCombinerPubkeys :: !(Maybe [Text])
    -- ^ When present, restricts bundle applicability to these
    -- pubkeys.  Absent ⇒ globally applicable.
  , bundleCreatedAt       :: !UTCTime
  , bundleSignature       :: !Text
  , bundleKeyId           :: !OracleKeyId
  } deriving (Eq, Show, Generic)

instance ToJSON Bundle where
  toJSON b = A.object
    [ "bundleId"        .= bundleId b
    , "setId"           .= bundleSetId b
    , "kind"            .= bundleKind b
    , "title"           .= bundleTitle b
    , "body"            .= bundleBody b
    , "combinerPubkeys" .= bundleCombinerPubkeys b
    , "createdAt"       .= bundleCreatedAt b
    , "signature"       .= bundleSignature b
    , "keyId"           .= bundleKeyId b
    ]
instance FromJSON Bundle where
  parseJSON = A.withObject "Bundle" $ \o -> Bundle
    <$> o .:  "bundleId"
    <*> o .:  "setId"
    <*> o .:  "kind"
    <*> o .:  "title"
    <*> o .:  "body"
    <*> o .:? "combinerPubkeys"
    <*> o .:  "createdAt"
    <*> o .:  "signature"
    <*> o .:  "keyId"

-- ---------------------------------------------------------------------------
-- Metadata
-- ---------------------------------------------------------------------------

data SetMetadata = SetMetadata
  { setMetadataSetId       :: !SetId
  , setMetadataThreshold   :: !Int
  , setMetadataTotalShares :: !Int
  , setMetadataUnlockKind  :: !BundleKind
  , setMetadataTitle       :: !Text
  , setMetadataSummary     :: !Text
  , setMetadataDiscovered  :: !(Maybe Int)
  } deriving (Eq, Show, Generic)

instance ToJSON SetMetadata where
  toJSON s = A.object
    [ "setId"       .= setMetadataSetId s
    , "threshold"   .= setMetadataThreshold s
    , "totalShares" .= setMetadataTotalShares s
    , "unlockKind"  .= setMetadataUnlockKind s
    , "title"       .= setMetadataTitle s
    , "summary"     .= setMetadataSummary s
    , "discovered"  .= setMetadataDiscovered s
    ]
instance FromJSON SetMetadata where
  parseJSON = A.withObject "SetMetadata" $ \o -> SetMetadata
    <$> o .:  "setId"
    <*> o .:  "threshold"
    <*> o .:  "totalShares"
    <*> o .:  "unlockKind"
    <*> o .:  "title"
    <*> o .:  "summary"
    <*> o .:? "discovered"

data TrustKey = TrustKey
  { trustKeyId        :: !OracleKeyId
  , trustKeyPubkey    :: !Text
  , trustKeyActive    :: !Bool
  , trustKeyRotatedAt :: !(Maybe UTCTime)
  } deriving (Eq, Show, Generic)

instance ToJSON TrustKey where
  toJSON k = A.object
    [ "keyId"     .= trustKeyId k
    , "pubkey"    .= trustKeyPubkey k
    , "active"    .= trustKeyActive k
    , "rotatedAt" .= trustKeyRotatedAt k
    ]
instance FromJSON TrustKey where
  parseJSON = A.withObject "TrustKey" $ \o -> TrustKey
    <$> o .:  "keyId"
    <*> o .:  "pubkey"
    <*> o .:  "active"
    <*> o .:? "rotatedAt"

data TrustList = TrustList
  { trustListKeys      :: ![TrustKey]
  , trustListUpdatedAt :: !UTCTime
  } deriving (Eq, Show, Generic)

instance ToJSON TrustList where
  toJSON l = A.object
    [ "keys"      .= trustListKeys l
    , "updatedAt" .= trustListUpdatedAt l
    ]
instance FromJSON TrustList where
  parseJSON = A.withObject "TrustList" $ \o -> TrustList
    <$> o .: "keys"
    <*> o .: "updatedAt"

data HealthResponse = HealthResponse
  { healthStatus          :: !Text
  , healthVersion         :: !Text
  , healthGenesisSeedHash :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

instance ToJSON HealthResponse where
  toJSON h = A.object
    [ "status"          .= healthStatus h
    , "version"         .= healthVersion h
    , "genesisSeedHash" .= healthGenesisSeedHash h
    ]
instance FromJSON HealthResponse where
  parseJSON = A.withObject "HealthResponse" $ \o -> HealthResponse
    <$> o .:  "status"
    <*> o .:  "version"
    <*> o .:? "genesisSeedHash"

-- ---------------------------------------------------------------------------
-- Error model
-- ---------------------------------------------------------------------------

-- | Matches the enum in the OpenAPI @Error.code@ field.  Open
-- (keeps an 'Unknown' branch) so a new server-side code doesn't
-- crash an older client.
data ErrorCode
  = ErrBadRequest
  | ErrUnauthorized
  | ErrNotFound
  | ErrConflict
  | ErrRateLimited
  | ErrHashMismatch
  | ErrInternal
  | ErrUnknown !Text
  deriving (Eq, Show, Generic)

instance ToJSON ErrorCode where
  toJSON c = toJSON $ case c of
    ErrBadRequest    -> "bad-request" :: Text
    ErrUnauthorized  -> "unauthorized"
    ErrNotFound      -> "not-found"
    ErrConflict      -> "conflict"
    ErrRateLimited   -> "rate-limited"
    ErrHashMismatch  -> "hash-mismatch"
    ErrInternal      -> "internal-error"
    ErrUnknown t     -> t
instance FromJSON ErrorCode where
  parseJSON = A.withText "ErrorCode" $ \t -> pure (case t of
    "bad-request"    -> ErrBadRequest
    "unauthorized"   -> ErrUnauthorized
    "not-found"      -> ErrNotFound
    "conflict"       -> ErrConflict
    "rate-limited"   -> ErrRateLimited
    "hash-mismatch"  -> ErrHashMismatch
    "internal-error" -> ErrInternal
    other            -> ErrUnknown other)

-- | A protocol-level error returned by the oracle.  The client-side
-- transport wraps transport failures (network, TLS, etc.) in
-- 'Engine.Sync.Relic.RelicTransportError' — this is only for
-- responses the oracle itself produced.
data RelicError = RelicError
  { relicErrorCode    :: !ErrorCode
  , relicErrorMessage :: !Text
  , relicErrorDetails :: !(Maybe A.Value)
  } deriving (Eq, Show, Generic)

instance ToJSON RelicError where
  toJSON e = A.object
    [ "code"    .= relicErrorCode e
    , "message" .= relicErrorMessage e
    , "details" .= relicErrorDetails e
    ]
instance FromJSON RelicError where
  parseJSON = A.withObject "RelicError" $ \o -> RelicError
    <$> o .:  "code"
    <*> o .:  "message"
    <*> o .:? "details"
