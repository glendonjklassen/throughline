-- | Terminal game runner: input handling, display refresh, and breathing prompt animation.
module Terminal.Runner (terminalUI) where

import           System.IO          (hSetBuffering, hSetEcho, stdin,
                                     BufferMode(..), hFlush, stdout)

import           Terminal.Layout     (ScenarioDisplay(..))
import           Engine.Runtime     (RuntimeUI(..))
import           Terminal.ANSI
import           Terminal.Render    (gameLoop, rawInputMode, cookedInputMode)
import           MonadStack         (runApp)

-- | Construct a terminal-based RuntimeUI from a ScenarioDisplay.
terminalUI :: ScenarioDisplay -> RuntimeUI
terminalUI display = RuntimeUI
  { uiSetup     = rawInputMode >> clearScreen
  , uiTeardown  = cookedInputMode >> clearScreen
  , uiGameLoop  = \env world ->
      runApp env world (gameLoop (sdLayout display) (sdStatusLine display))
  , uiOnEnd     = \finalW -> do
      mapM_ putStrLn (sdEndScreen display finalW)
      putStrLn ""
      putStrLn (grey "Press any key to exit.")
      _ <- getChar
      clearScreen
  , uiOnError   = \msg -> putStrLn (red "Fatal: " <> msg)
  , uiOnWarn    = putStrLn . yellow
  , uiPromptMerge = \name count -> do
      putStrLn (grey ("Foreign log from " <> name <> ": "
        <> show count <> " new action(s). Merge? (y/n) "))
      hFlush stdout
      hSetBuffering stdin LineBuffering
      hSetEcho stdin True
      answer <- getLine
      hSetBuffering stdin NoBuffering
      hSetEcho stdin False
      pure (answer `elem` ["y", "Y", "yes"])
  }
