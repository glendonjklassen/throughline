-- | Ed25519 player identity: keypair generation, signing, and verification.
module Engine.Sync.Identity
  ( Identity(..)
  , defaultIdentityPath
  , loadOrCreate
  , playerIdOf
  , playerCharId
  , publicKeyFromPlayerId
  , signEntry
  , verifyEntry
  ) where

import qualified Crypto.PubKey.Ed25519      as Ed25519
import           Crypto.Error               (CryptoFailable(..))
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteArray             as BA
import           Data.ByteArray.Encoding    (convertFromBase, Base(..))
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BL
import           Numeric                    (showHex)
import           System.Directory           (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import           System.FilePath            ((</>), takeDirectory, dropExtension)

import           GameTypes

data Identity = Identity
  { identitySecretKey :: Ed25519.SecretKey
  , identityPublicKey :: Ed25519.PublicKey
  , identityLabel     :: String
    -- ^ Human-readable display name. Stored in the .label file alongside the
    -- key. Used as the player character's charName; never used for signing or
    -- CRDT attribution — PlayerId (the full hex key) handles those.
  }

defaultIdentityPath :: IO FilePath
defaultIdentityPath = do
  home <- getHomeDirectory
  pure (home </> ".local" </> "share" </> "throughline" </> "identity.key")

loadOrCreate :: FilePath -> IO Identity
loadOrCreate path = do
  exists <- doesFileExist path
  base   <- if exists then loadIdentity path else createIdentity path
  label  <- loadLabel (labelPath path)
  pure base { identityLabel = label }

-- | Derive a PlayerId from an Identity.
-- The PlayerId string is the hex-encoded 32-byte Ed25519 public key.
playerIdOf :: Identity -> PlayerId
playerIdOf ident = PlayerId (hexEncode (BA.convert (identityPublicKey ident)))

-- | Derive the player's self CharId from their Identity.
-- Uses the first 12 hex characters of the public key — unique enough for
-- local use, readable in debug output.
playerCharId :: Identity -> CharId
playerCharId ident = Named (take 12 (hexEncode (BA.convert (identityPublicKey ident))))

-- | Recover a PublicKey from a PlayerId, if it encodes a valid 32-byte key.
-- Returns Nothing for short test/legacy PlayerId values.
publicKeyFromPlayerId :: PlayerId -> Maybe Ed25519.PublicKey
publicKeyFromPlayerId (PlayerId s) =
  case convertFromBase Base16 (asciiToBS s) of
    Left  _  -> Nothing
    Right bs -> case Ed25519.publicKey (bs :: BS.ByteString) of
      CryptoPassed pk -> Just pk
      CryptoFailed _  -> Nothing

-- | Sign a log entry, returning a new entry with the signature attached.
-- Signs over entryId <> entryActionId <> JSON-encoded diff.
signEntry :: Identity -> LogEntry -> LogEntry
signEntry ident entry = entry { entrySignature = Just sig }
  where
    sig = BA.convert (Ed25519.sign (identitySecretKey ident) (identityPublicKey ident) (signingMessage entry))

-- | Verify a log entry's signature against its recorded PlayerId.
-- Returns True for entries with no signature (legacy / test entries) and for
-- PlayerId values that are not key-derived (e.g. "test", "player-a").
verifyEntry :: LogEntry -> Bool
verifyEntry entry = case entrySignature entry of
  Nothing  -> True
  Just sig ->
    case publicKeyFromPlayerId (entryPlayerId entry) of
      Nothing -> True
      Just pk ->
        case Ed25519.signature (sig :: BS.ByteString) of
          CryptoFailed _ -> False
          CryptoPassed s -> Ed25519.verify pk (signingMessage entry) s

-- ---------------------------------------------------------------------------
-- Internal
-- ---------------------------------------------------------------------------

-- | Bytes that are signed/verified: entryId <> entryActionId <> JSON(diff).
signingMessage :: LogEntry -> BS.ByteString
signingMessage entry =
  asciiToBS (entryId entry <> actionIdText (entryActionId entry))
  <> BL.toStrict (Aeson.encode (entryDiff entry))

createIdentity :: FilePath -> IO Identity
createIdentity path = do
  createDirectoryIfMissing True (takeDirectory path)
  sk <- Ed25519.generateSecretKey
  let pk = Ed25519.toPublic sk
  BS.writeFile path (BA.convert sk)
  pure (Identity sk pk "")

loadIdentity :: FilePath -> IO Identity
loadIdentity path = do
  bs <- BS.readFile path
  case Ed25519.secretKey (bs :: BS.ByteString) of
    CryptoFailed err -> ioError (userError ("Invalid identity key: " <> show err))
    CryptoPassed sk  -> pure (Identity sk (Ed25519.toPublic sk) "")

-- | Path to the human-readable label file stored alongside the key file.
labelPath :: FilePath -> FilePath
labelPath keyPath = dropExtension keyPath ++ ".label"

-- | Load the player's display label. Falls back to "Player" if the file
-- does not exist. The user can edit this file to change their display name.
loadLabel :: FilePath -> IO String
loadLabel path = do
  exists <- doesFileExist path
  if exists
    then filter (/= '\n') <$> readFile path
    else pure "Player"

hexEncode :: BS.ByteString -> String
hexEncode = concatMap byteToHex . BS.unpack
  where byteToHex b = let h = showHex b "" in if length h == 1 then '0':h else h

-- | Encode an ASCII string as a ByteString (safe for hex chars and log IDs).
asciiToBS :: String -> BS.ByteString
asciiToBS = BS.pack . map (fromIntegral . fromEnum)
