-- | Tag-driven achievements.
--
-- An 'Achievement' is a named condition on a world 'Tag' that, when
-- first satisfied, gets reported to the backend (eventually Steam,
-- via 'ISteamUserStats::SetAchievement').  The definitions are
-- declarative and scenario-local — each scenario that wants to grant
-- achievements publishes a list of 'Achievement' values; the
-- launcher hands those to the runner as part of the 'AchievementKit'.
--
-- Earning state is persisted to disk so a player who earns something
-- on a playthrough doesn't lose it after they reset the save.  The
-- persistence format is a simple JSON record keyed by achievement id
-- so future schema changes can add fields without invalidating the
-- stored keys.
{-# LANGUAGE OverloadedStrings #-}
module SDL.Achievements
  ( Achievement(..)
  , AchievementKit(..)
  , EarnedMap
  , emptyEarnedMap
  , loadEarned
  , saveEarned
  , checkEarnAgainstDiff
  , achievementsFilePath
  ) where

import           Control.Exception      (SomeException, try)
import qualified Data.Aeson             as Aeson
import qualified Data.ByteString.Lazy   as BL
import qualified Data.Map.Strict        as Map
import           System.Directory       (XdgDirectory(..), createDirectoryIfMissing,
                                         doesFileExist, getXdgDirectory)
import           System.FilePath        ((</>), takeDirectory)

import           GameTypes              (Tag, WorldDiff(..))

-- | One earnable achievement.  Kept flat and declarative so
-- scenarios can assemble their catalogs without reaching into the
-- engine, and so the list is easy to eyeball for Steam submission.
data Achievement = Achievement
  { achId          :: !String   -- ^ stable identifier (matches Steam 'API Name')
  , achDisplayName :: !String
  , achDescription :: !String
  , achRequiredTag :: !Tag      -- ^ the world tag whose *addition* grants the award
  } deriving (Show, Eq)

-- | Everything the runner needs to process achievements.  The kit
-- bundles the catalog (what can be earned) with a live-earned map
-- (what has been earned) and the on-disk path where progress
-- persists.  Assembled once at launcher start.
data AchievementKit = AchievementKit
  { akCatalog  :: [Achievement]
  , akFilePath :: FilePath
  , akEarned   :: EarnedMap
  } deriving (Show)

-- | Map from achievement id to the ISO-ish timestamp string when it
-- was earned.  Stored as 'String' rather than 'UTCTime' to keep the
-- on-disk shape dumb and forward-compatible.
type EarnedMap = Map.Map String String

emptyEarnedMap :: EarnedMap
emptyEarnedMap = Map.empty

-- | Canonical path for earned-achievements persistence.  Lives under
-- the same XDG config root as 'Settings' so a single sync-folder
-- backup captures both.
achievementsFilePath :: IO FilePath
achievementsFilePath = do
  dir <- getXdgDirectory XdgConfig "throughline"
  pure (dir </> "achievements.json")

-- | Load the earned map, falling back to empty on any failure.
-- Corrupt files are tolerated rather than fatal: an achievement-
-- persistence error shouldn't block the game from starting.
loadEarned :: FilePath -> IO EarnedMap
loadEarned path = do
  exists <- doesFileExist path
  if not exists
    then pure emptyEarnedMap
    else do
      r <- try (BL.readFile path) :: IO (Either SomeException BL.ByteString)
      pure $ case r of
        Left  _   -> emptyEarnedMap
        Right raw -> case Aeson.eitherDecode raw of
          Left  _ -> emptyEarnedMap
          Right m -> m

-- | Persist the earned map.  Any IO error propagates so the caller
-- can decide whether to surface it.
saveEarned :: FilePath -> EarnedMap -> IO ()
saveEarned path m = do
  createDirectoryIfMissing True (takeDirectory path)
  BL.writeFile path (Aeson.encode m)

-- | Given a catalog and a WorldDiff from a log entry, return the list
-- of achievement ids this diff *grants*.  Callers feed an @earned@
-- map to avoid regranting the same id — achievements are once-only.
--
-- The check is purely a set-membership test on 'diffWorldTagsAdded'
-- plus the "is not already earned" filter.  No engine machinery
-- needed: scenarios encode their milestones as world tags anyway.
checkEarnAgainstDiff
  :: [Achievement]
  -> EarnedMap
  -> WorldDiff
  -> [Achievement]
checkEarnAgainstDiff catalog earned diff =
  let added = diffWorldTagsAdded diff
  in [ a
     | a <- catalog
     , achRequiredTag a `elem` added
     , not (achId a `Map.member` earned)
     ]
