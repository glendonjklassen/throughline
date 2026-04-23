-- | Top-level crash handler for the SDL executable.
--
-- Wraps the program entry point so an unhandled exception — thrown from
-- anywhere in the engine, runner, or a scenario axiom — gets written to
-- disk as a crash report and shown to the player on a friendly screen
-- instead of a terminal stack trace.
--
-- Nothing here reaches the network: the whole point is that a player on
-- Windows who double-clicks the executable gets an actionable message,
-- and the developer gets a file they can ask the player to send.
module SDL.CrashHandler
  ( withCrashHandler
  , writeCrashReport
  , crashReportDir
  ) where

import           Control.Exception      (SomeException, displayException, try)
import           Data.Time.Clock        (getCurrentTime)
import           Data.Time.Format       (defaultTimeLocale, formatTime)
import           System.Directory       (createDirectoryIfMissing)
import           System.Exit            (exitFailure)
import           System.FilePath        ((</>))
import           System.IO              (hPutStrLn, stderr)

import           Engine.Runtime         (sessionsRootDir)

-- | Directory under the sessions root where crash reports are written.
crashReportDir :: FilePath
crashReportDir = sessionsRootDir </> "crashes"

-- | Run an IO action and, if it throws, persist a crash report and show
-- the user a terminal-style message instead of a raw stack dump.  The
-- process exits with a non-zero status after reporting so shells and
-- Steam can still see that the run failed.
--
-- Takes a fallback display action so the caller can render something
-- graphical when SDL is still alive; if the renderer itself is what
-- crashed, the fallback is skipped and only the file + stderr path runs.
withCrashHandler :: (FilePath -> String -> IO ()) -> IO a -> IO a
withCrashHandler showToUser action = do
  result <- try action
  case result of
    Right a  -> pure a
    Left  e  -> do
      reportPath <- writeCrashReport (e :: SomeException)
      -- Best-effort: if the GUI path also throws (SDL might be dead
      -- already) fall through to stderr so the player at least sees it.
      _ <- try (showToUser reportPath (displayException e)) :: IO (Either SomeException ())
      hPutStrLn stderr ""
      hPutStrLn stderr "throughline crashed."
      hPutStrLn stderr ("Crash report: " <> reportPath)
      hPutStrLn stderr ""
      hPutStrLn stderr (displayException e)
      exitFailure

-- | Serialize the exception to a timestamped file under 'crashReportDir'.
-- Returns the path so the UI can tell the user exactly where to look.
writeCrashReport :: SomeException -> IO FilePath
writeCrashReport e = do
  createDirectoryIfMissing True crashReportDir
  now <- getCurrentTime
  let stamp = formatTime defaultTimeLocale "%Y%m%d-%H%M%S" now
      path  = crashReportDir </> ("crash-" <> stamp <> ".txt")
      body  = unlines
        [ "throughline crash report"
        , "time: " <> show now
        , ""
        , "exception:"
        , displayException e
        ]
  writeFile path body
  pure path
