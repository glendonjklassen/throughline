-- | Entry-point scaffolding shared between the multi-scenario dev
-- launcher and single-scenario Steam bundles.
--
-- An executable tells 'runLauncher' which scenarios it wants to offer
-- and the launcher decides whether to render a picker (multiple) or a
-- title screen with Continue / New hunt (one).  It also hosts the
-- crash-handler wrap, so every shipped executable gets the same
-- on-crash behavior for free.
--
-- What lives here is all the Main-ish plumbing.  Scenario bodies stay
-- scenario-local; bundle-specific metadata (title, tagline, display)
-- travels in 'ScenarioEntry'.
module SDL.Launcher
  ( ScenarioEntry(..)
  , runLauncher
  ) where

import           Control.Exception      (SomeException, try)

import           Data.Maybe             (fromMaybe)
import qualified Data.Version           as Version

import           Engine.Runtime         (runScenarioWith)
import           Engine.Sync.Identity   (defaultIdentityPath, loadOrCreate, playerIdOf)
import           Engine.Sync.Progress   (Progress, defaultProgressPath, recordHunt)
import           GameTypes              (CharacterId(..), PlayerId, Scenario, scenarioName)
import           Paths_throughline      (version)
import           SDL.ClickMap           (ClickMap, gridRowRect, hitTest)
import           SDL.CrashHandler       (withCrashHandler)
import           SDL.FontContext        (renderText)
import           SDL.InputHandler       (InputEvent(..), awaitInputSDL)
import           SDL.Layout             (ScenarioDisplay)
import           SDL.Onboarding         (defaultHowToPlay, howToPlayLoop)
import           SDL.Palette            (PaletteMode(..), textColor, dimTextColor, chromeColor, warningColor)
import           SDL.Renderer           (SDLContext(..), clearSDL, freeSDL, initSDLWith, presentSDL)
import           SDL.Runner             (sdlUI)
import           SDL.SaveSlots          (SaveStatus(..), resetScenarioSave,
                                         scenarioSaveStatus)
import           SDL.Settings           (Settings(..), loadSettings,
                                         viewportRecommendedFontScale,
                                         viewportSize)
import           SDL.SharedFolder       (scanSharedLogs)
import           SDL.SettingsMenu       (settingsMenu)

-- | What the launcher needs to know about one scenario.  'label' and
-- 'tagline' are the player-facing strings; 'display' is wired into
-- the runner for scenario-specific rendering; 'make' is the seed-
-- and-playerId-parameterized scenario constructor; 'howToPlay' lets
-- a scenario ship a custom help screen, falling back to generic help
-- when absent.
data ScenarioEntry = ScenarioEntry
  { entryLabel     :: String
  , entryTagline   :: String
  , entryDisplay   :: ScenarioDisplay
  , entryMake      :: Int -> CharacterId -> Scenario
  , entryHowToPlay :: Maybe [String]
  }

-- | Font asset path — same for every bundle, shipped alongside the
-- binary.  If we ever need per-bundle fonts this moves into
-- 'ScenarioEntry' or a bundle config.
fontAsset :: FilePath
fontAsset = "assets/JetBrainsMono-Regular.ttf"

-- ---------------------------------------------------------------------------
-- Top-level entry
-- ---------------------------------------------------------------------------

-- | Run the launcher for a bundle.  Empty list is a programmer error;
-- a one-entry list skips the picker and shows a title screen; many
-- entries render the scenario menu.
runLauncher :: [ScenarioEntry] -> IO ()
runLauncher []       = error "runLauncher: no scenarios in bundle"
runLauncher entries  = withCrashHandler renderCrashScreen (launcherMain entries)

launcherMain :: [ScenarioEntry] -> IO ()
launcherMain entries = do
  settings <- loadSettings
  let mode  = if sHighContrast settings then HighContrast else Autumn
      title = case entries of
        [single] -> entryLabel single <> " — throughline"
        _        -> "throughline"
      vp    = sViewport settings
      -- The user scale multiplies on top of the viewport's
      -- recommended default, so the player picks "a bit bigger" or
      -- "a bit smaller" relative to what reads cleanly on their
      -- screen, not raw pixel sizes.
      scale = viewportRecommendedFontScale vp * sFontScale settings
  ctx    <- initSDLWith fontAsset title (viewportSize vp) scale mode
  ident  <- loadOrCreate =<< defaultIdentityPath
  let pid = playerIdOf ident
  choice <- case entries of
    [single] -> singleScenarioMenu ctx pid single
    _        -> multiScenarioMenu  ctx pid entries
  freeSDL ctx
  case choice of
    Nothing    -> pure ()
    Just entry -> do
      -- Bump the player's hunt counter for this identity.  The record
      -- is the scaffolding for Tier-2 lifetime finds — stature reads
      -- off 'progressHuntCount', rotation bumps 'progressEpoch'.
      -- Failures here must not prevent a hunt from starting, so any
      -- I/O error is swallowed.
      progressPath <- defaultProgressPath
      _ <- try (recordHunt pid progressPath) :: IO (Either SomeException Progress)
      -- The shared-folder scanner fires once at scenario start (and
      -- again if a live-merge pass re-reads).  If the player hasn't
      -- configured a shared folder, the action returns an empty
      -- list and the merge behaves identically to the solo path.
      let sharedScan n = case sSharedFolder settings of
            Nothing  -> pure []
            Just dir -> scanSharedLogs dir n pid
          -- scenarioName doesn't depend on seed/you — probe with
          -- dummies to get the string for the runner.
          scenName = scenarioName (entryMake entry 0 dummyChar)
      runScenarioWith (sdlUI scenName (entryDisplay entry)) sharedScan
                      (entryMake entry)

-- ---------------------------------------------------------------------------
-- Single-scenario bundle: title screen with Continue / New hunt
-- ---------------------------------------------------------------------------

-- | Title screen for a one-scenario Steam bundle.  Shows Continue only
-- when a save exists; always offers New (with a confirmation if a
-- save would be discarded).
singleScenarioMenu :: SDLContext -> PlayerId -> ScenarioEntry -> IO (Maybe ScenarioEntry)
singleScenarioMenu ctx pid entry = do
  status <- scenarioEntryStatus pid entry
  cm <- renderSingleMenu ctx entry status
  pickSingle ctx pid entry status cm

-- | Render the title screen and return the click-map that resolves a
-- pointer position to the same dispatch char the keyboard uses.
renderSingleMenu :: SDLContext -> ScenarioEntry -> SaveStatus -> IO ClickMap
renderSingleMenu ctx entry status = do
  clearSDL ctx
  let fc = sdlFont ctx
      -- Each clickable row spans the full screen width so a tap
      -- anywhere on the line selects the option.
      rowHit r = gridRowRect fc 0 r 80
  renderText fc (entryLabel entry)                  textColor (3, 2)
  renderText fc (entryTagline entry)                dimTextColor     (3, 3)
  renderText fc ""                                  dimTextColor     (3, 4)
  let primaryRow =
        if hasSave status
          then do
            renderText fc "1) Continue"                 textColor (4, 6)
            renderText fc (continueHint status)         dimTextColor     (4, 7)
            renderText fc "2) New hunt (discards save)" textColor (4, 9)
          else
            renderText fc "1) Begin"                    textColor (4, 6)
  primaryRow
  renderText fc "h) How to play"                    chromeColor    (4, 11)
  renderText fc "s) Settings"                       chromeColor    (4, 12)
  renderText fc "q) Quit"                           chromeColor    (4, 13)
  renderText fc versionTag                          dimTextColor     (4, 15)
  presentSDL ctx
  let continueMap =
        [ rowHit 6 '1'
        , rowHit 7 '1'   -- hint line belongs to Continue
        , rowHit 9 '2'
        ]
      beginMap = [ rowHit 6 '1' ]
      footer   =
        [ rowHit 11 'h'
        , rowHit 12 's'
        , rowHit 13 'q'
        ]
  pure (if hasSave status then continueMap <> footer else beginMap <> footer)
  where
    continueHint s = "   " <> show (saveEntryCount s) <> " actions logged"

pickSingle :: SDLContext -> PlayerId -> ScenarioEntry -> SaveStatus
           -> ClickMap -> IO (Maybe ScenarioEntry)
pickSingle ctx pid entry status = loop
  where
    loop cm = do
      me <- awaitInputSDL (sdlWindow ctx)
      case me of
        Nothing                  -> pure Nothing
        Just (KeyPress c)        -> dispatch cm c
        Just (ClickAt px py)     -> case hitTest cm px py of
          Just c  -> dispatch cm c
          Nothing -> loop cm

    dispatch cm c = case c of
      'q' -> pure Nothing
      'h' -> showHelp
      'H' -> showHelp
      's' -> showSettings
      'S' -> showSettings
      '1' | hasSave status -> pure (Just entry)
          | otherwise      -> pure (Just entry)
      '2' | hasSave status -> do
              confirmed <- confirmDiscard ctx (entryLabel entry)
              if confirmed
                then do
                  resetScenarioSave pid (scenarioName (entryMake entry 0 dummyChar))
                  pure (Just entry)
                else do
                  cm' <- renderSingleMenu ctx entry status
                  loop cm'
          | otherwise -> loop cm
      _   -> loop cm

    showHelp = do
      howToPlayLoop ctx (entryLabel entry) (helpPagesFor entry)
      cm' <- renderSingleMenu ctx entry status
      loop cm'
    showSettings = do
      _ <- settingsMenu ctx
      cm' <- renderSingleMenu ctx entry status
      loop cm'

-- | A placeholder CharacterId used only to ask a scenario for its 'scenarioName'.
-- Scenario names are static strings that never depend on the player
-- CharacterId passed at construction, so feeding in a throwaway value is
-- safe and saves the launcher from having to know the real one early.
dummyChar :: CharacterId
dummyChar = Truth

-- ---------------------------------------------------------------------------
-- Multi-scenario bundle: dev launcher with numeric picker
-- ---------------------------------------------------------------------------

-- | Render the scenario menu in the SDL window and await a choice.
-- Shows a save indicator next to scenarios with in-progress hunts.
multiScenarioMenu :: SDLContext -> PlayerId -> [ScenarioEntry] -> IO (Maybe ScenarioEntry)
multiScenarioMenu ctx pid entries = do
  statuses <- mapM (scenarioEntryStatus pid) entries
  cm <- renderMultiMenu ctx entries statuses
  pickMulti statuses cm
  where
    pickMulti statuses cm = do
      me <- awaitInputSDL (sdlWindow ctx)
      case me of
        Nothing              -> pure Nothing
        Just (KeyPress c)    -> dispatch statuses cm c
        Just (ClickAt px py) -> case hitTest cm px py of
          Just c  -> dispatch statuses cm c
          Nothing -> pickMulti statuses cm

    dispatch statuses cm c = case c of
      'q' -> pure Nothing
      'h' -> showHelp statuses
      'H' -> showHelp statuses
      's' -> showSettings statuses
      'S' -> showSettings statuses
      ch  ->
        let n = fromEnum ch - fromEnum '0'
        in if n >= 1 && n <= length entries
             then pure (Just (entries !! (n - 1)))
             else pickMulti statuses cm

    showHelp statuses = do
      howToPlayLoop ctx "how to play" defaultHowToPlay
      cm' <- renderMultiMenu ctx entries statuses
      pickMulti statuses cm'
    showSettings statuses = do
      _ <- settingsMenu ctx
      cm' <- renderMultiMenu ctx entries statuses
      pickMulti statuses cm'

renderMultiMenu :: SDLContext -> [ScenarioEntry] -> [SaveStatus] -> IO ClickMap
renderMultiMenu ctx entries statuses = do
  clearSDL ctx
  let fc = sdlFont ctx
      rowHit r = gridRowRect fc 0 r 80
  renderText fc "throughline" textColor (3, 2)
  renderText fc "A narrative engine." dimTextColor (3, 3)
  renderText fc "" dimTextColor (3, 4)
  mapM_ (renderRow fc) (zip3 [1 :: Int ..] entries statuses)
  let helpRow     = fromIntegral (4 + length entries * 2 + 2)
      settingsRow = helpRow + 1
      quitRow     = settingsRow + 1
  renderText fc "h) How to play" chromeColor (4, helpRow)
  renderText fc "s) Settings"    chromeColor (4, settingsRow)
  renderText fc "q) Quit"        chromeColor (4, quitRow)
  renderText fc versionTag       dimTextColor  (4, quitRow + 2)
  presentSDL ctx
  -- Each scenario takes two text rows (label + tagline); click on
  -- either row dispatches the scenario's digit.
  let scenarioRects =
        concat
          [ let key = digitFor n
            in [ rowHit (4 + n * 2)     key
               , rowHit (4 + n * 2 + 1) key
               ]
          | n <- [1 .. length entries]
          ]
      digitFor n = case show n of
        (c : _) -> c
        []      -> '?'   -- unreachable: show never yields [] on Int
      footer =
        [ rowHit (fromIntegral helpRow)     'h'
        , rowHit (fromIntegral settingsRow) 's'
        , rowHit (fromIntegral quitRow)     'q'
        ]
  pure (scenarioRects <> footer)
  where
    renderRow fc (n, e, s) = do
      let row   = fromIntegral (4 + n * 2)
          label = show n <> ". " <> entryLabel e <> saveTag s
      renderText fc label           textColor (4, row)
      renderText fc ("   " <> entryTagline e) dimTextColor (4, row + 1)
    saveTag s
      | hasSave s = "  (in progress)"
      | otherwise = ""

-- ---------------------------------------------------------------------------
-- Shared helpers
-- ---------------------------------------------------------------------------

scenarioEntryStatus :: PlayerId -> ScenarioEntry -> IO SaveStatus
scenarioEntryStatus pid e =
  scenarioSaveStatus pid (scenarioName (entryMake e 0 dummyChar))

-- | Short human-readable version tag pulled from the cabal file at
-- build time.  Shown in the footer of the launcher menus so players
-- can include it when filing bug reports.
versionTag :: String
versionTag = "v" <> Version.showVersion version

-- | Pick the help pages for a scenario entry, falling back to the
-- generic 'defaultHowToPlay' when the entry didn't supply any.
helpPagesFor :: ScenarioEntry -> [String]
helpPagesFor e = fromMaybe defaultHowToPlay (entryHowToPlay e)

-- | Two-option confirmation.  Either key (y / n) or a click on the
-- corresponding row resolves; anything else cancels conservatively.
-- The "discards save" wording on the main screen already does the
-- warning work, so this screen stays spartan.
confirmDiscard :: SDLContext -> String -> IO Bool
confirmDiscard ctx label = do
  clearSDL ctx
  let fc = sdlFont ctx
      rowHit r = gridRowRect fc 0 r 80
  renderText fc "Start a new hunt?"             textColor  (3, 2)
  renderText fc ("This deletes your " <> label) warningColor (3, 3)
  renderText fc "save permanently."             warningColor (3, 4)
  renderText fc ""                              dimTextColor      (3, 5)
  renderText fc "y) Yes, start over"            textColor  (4, 7)
  renderText fc "n) Cancel"                     textColor  (4, 8)
  presentSDL ctx
  let cm = [rowHit 7 'y', rowHit 8 'n']
  loop cm
  where
    loop cm = do
      me <- awaitInputSDL (sdlWindow ctx)
      case me of
        Nothing              -> pure False
        Just (KeyPress c)    -> decide c
        Just (ClickAt px py) -> case hitTest cm px py of
          Just c  -> decide c
          Nothing -> loop cm
    decide c = pure (c == 'y' || c == 'Y')

-- ---------------------------------------------------------------------------
-- Crash screen
-- ---------------------------------------------------------------------------

-- | GUI fallback for the crash handler: try to spin up a fresh SDL
-- context (the old one might be dead) and show the crash-report path
-- and a short excerpt of the exception.  Any failure here falls
-- through — the handler also logs to stderr and disk.
renderCrashScreen :: FilePath -> String -> IO ()
renderCrashScreen reportPath message = do
  -- Spin up a fresh context with default settings — the user's own
  -- settings might have been implicated in the crash, so we don't
  -- re-read them here.
  r <- try (initSDLWith fontAsset "throughline — crash" (1280, 800) 1.0 Autumn)
         :: IO (Either SomeException SDLContext)
  case r of
    Left _    -> pure ()
    Right ctx -> do
      clearSDL ctx
      let fc = sdlFont ctx
      renderText fc "throughline crashed."                    warningColor (3, 2)
      renderText fc ""                                        dimTextColor      (3, 3)
      renderText fc "A crash report was written to:"          textColor  (3, 4)
      renderText fc reportPath                                textColor  (3, 5)
      renderText fc ""                                        dimTextColor      (3, 6)
      renderText fc "You can attach that file when reporting" dimTextColor      (3, 7)
      renderText fc "this issue."                             dimTextColor      (3, 8)
      renderText fc ""                                        dimTextColor      (3, 9)
      renderText fc (excerpt message)                         chromeColor     (3, 10)
      renderText fc ""                                        dimTextColor      (3, 11)
      renderText fc "Press any key or click to close."        chromeColor     (3, 12)
      presentSDL ctx
      _ <- awaitInputSDL (sdlWindow ctx)
      freeSDL ctx
  where
    excerpt s = take 80 (takeWhile (/= '\n') s)
