-- | Headless game execution for scripted testing and automation without terminal I/O.
module Engine.Headless
  ( ActionSource
  , StepHook
  , noopHook
  , coreLoop
  , runHeadlessRandom
  , runHeadlessScript
  , runHeadlessNarrated
  , TurnRecord(..)
  , runHeadlessPlaythrough
  ) where

import           Control.Monad          (unless)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (asks)
import           Control.Monad.State    (get)
import           Data.IORef             (IORef, newIORef, readIORef, writeIORef,
                                         modifyIORef')
import           Data.List              (find)
import           System.Random          (StdGen, mkStdGen, randomR)

import qualified Data.Map.Strict        as Map

import           Engine.Core.Axioms            (diffWorlds)
import           Engine.Core.Conditions        (checkCondition)
import           Engine.Core.Effects           (executeStep)
import           Engine.Core.NarrativeMessage  (NarrativeEntry)
import           Engine.Sync.EventLog          (logAction, memoryLogStore)
import           GameTypes
import           MonadStack

-- ---------------------------------------------------------------------------
-- Action source and step hook types
-- ---------------------------------------------------------------------------

-- | Given the current valid actions, produce the next action to execute,
-- or Nothing to end the loop.
type ActionSource = [AnyAction] -> App (Maybe AnyAction)

-- | Called after each step with the world before, world after, and the diff.
-- Used by the terminal runner for debug output; headless uses noopHook.
type StepHook = GameWorld -> GameWorld -> WorldDiff -> App ()

noopHook :: StepHook
noopHook _ _ _ = pure ()

-- ---------------------------------------------------------------------------
-- Core loop
-- ---------------------------------------------------------------------------

-- | Execute actions from the source until it returns Nothing or the scenario
-- terminal condition is met. Writes to the event log (file or accumulator)
-- via logAction. No terminal I/O.
coreLoop :: StepHook -> ActionSource -> App ()
coreLoop hook source = do
  world    <- get
  acts     <- asks envActions
  terminal <- asks envTerminal
  let actions = filter (checkCondition world . anyActionCondition) acts
  mAction <- source actions
  case mAction of
    Nothing     -> pure ()
    Just (AnyAction action) -> do
      worldBefore <- get
      pid         <- asks envPlayerId
      executeStep action
      worldFinal  <- get
      let diff = diffWorlds pid worldBefore worldFinal
      logAction (actionId action) diff
      hook worldBefore worldFinal diff
      unless (checkCondition worldFinal terminal) (coreLoop hook source)

-- ---------------------------------------------------------------------------
-- Headless runners
-- ---------------------------------------------------------------------------

-- | Run a scenario headlessly for up to @steps@ ticks using a seeded random
-- walk. Returns the final world and the accumulated event log in
-- chronological order.
runHeadlessRandom
  :: (CharacterId -> Scenario)
  -> PlayerId  -- ^ identity for this run (CharacterId is derived as Named (take 12 pid))
  -> Int       -- ^ max steps
  -> Int       -- ^ random seed
  -> IO (Either AppError (GameWorld, [LogEntry]))
runHeadlessRandom mkScenario playerId steps seed = do
  let PlayerId pidStr = playerId
      you      = Named (take 12 pidStr)
      scenario = mkScenario you
  debugRef    <- newIORef Off
  msgRef      <- newIORef []
  accumRef    <- newIORef []
  traceRef    <- newIORef []
  frontierRef <- newIORef Map.empty
  let env = Env
        { envActions      = scenarioActions scenario
        , envAxioms       = scenarioAxioms scenario
        , envMergeAxioms  = scenarioMergeAxioms scenario
        , envRules        = scenarioRules scenario
        , envMergeRules   = scenarioMergeRules scenario
        , envLog          = \_ -> pure ()
        , envDebug        = debugRef
        , envTerminal     = scenarioTerminal scenario
        , envMessageLog   = msgRef
        , envPlayerId     = playerId
        , envPlayerCharId = you
        , envLogStore     = memoryLogStore accumRef
        , envAxiomTrace   = traceRef
        , envFrontier     = frontierRef
        , envLiveMerge    = \w -> pure (w, [])  -- no-op: return world unchanged
        }
  genRef   <- newIORef (mkStdGen seed)
  stepsRef <- newIORef (0 :: Int)
  result   <- runApp env (scenarioInitial scenario)
                (coreLoop noopHook (randomSource genRef steps stepsRef))
  case result of
    Left err       -> pure (Left err)
    Right (_, w)   -> do
      entries <- reverse <$> readIORef accumRef
      pure (Right (w, entries))

-- | Run a scenario headlessly by executing a fixed sequence of action IDs.
-- Unavailable actions (condition not met) are skipped. Returns the final
-- world and the accumulated event log in chronological order.
runHeadlessScript
  :: (CharacterId -> Scenario)
  -> PlayerId   -- ^ identity for this run (CharacterId is derived as Named (take 12 pid))
  -> [ActionId] -- ^ action IDs to execute in order
  -> IO (Either AppError (GameWorld, [LogEntry]))
runHeadlessScript mkScenario playerId actionIds = do
  let PlayerId pidStr = playerId
      you      = Named (take 12 pidStr)
      scenario = mkScenario you
  debugRef    <- newIORef Off
  msgRef      <- newIORef []
  accumRef    <- newIORef []
  traceRef    <- newIORef []
  frontierRef <- newIORef Map.empty
  let env = Env
        { envActions      = scenarioActions scenario
        , envAxioms       = scenarioAxioms scenario
        , envMergeAxioms  = scenarioMergeAxioms scenario
        , envRules        = scenarioRules scenario
        , envMergeRules   = scenarioMergeRules scenario
        , envLog          = \_ -> pure ()
        , envDebug        = debugRef
        , envTerminal     = scenarioTerminal scenario
        , envMessageLog   = msgRef
        , envPlayerId     = playerId
        , envPlayerCharId = you
        , envLogStore     = memoryLogStore accumRef
        , envAxiomTrace   = traceRef
        , envFrontier     = frontierRef
        , envLiveMerge    = \w -> pure (w, [])  -- no-op: return world unchanged
        }
  idsRef <- newIORef actionIds
  result <- runApp env (scenarioInitial scenario)
              (coreLoop noopHook (scriptSource idsRef))
  case result of
    Left err     -> pure (Left err)
    Right (_, w) -> do
      entries <- reverse <$> readIORef accumRef
      pure (Right (w, entries))

-- | Like 'runHeadlessScript' but also returns the captured narrative messages
-- in chronological order. Useful for tests that need to assert on prose output.
runHeadlessNarrated
  :: (CharacterId -> Scenario)
  -> PlayerId   -- ^ identity for this run (CharacterId is derived as Named (take 12 pid))
  -> [ActionId] -- ^ action IDs to execute in order
  -> IO (Either AppError (GameWorld, [LogEntry], [NarrativeEntry]))
runHeadlessNarrated mkScenario playerId actionIds = do
  let PlayerId pidStr = playerId
      you      = Named (take 12 pidStr)
      scenario = mkScenario you
  debugRef    <- newIORef Off
  msgRef      <- newIORef []
  accumRef    <- newIORef []
  traceRef    <- newIORef []
  frontierRef <- newIORef Map.empty
  let env = Env
        { envActions      = scenarioActions scenario
        , envAxioms       = scenarioAxioms scenario
        , envMergeAxioms  = scenarioMergeAxioms scenario
        , envRules        = scenarioRules scenario
        , envMergeRules   = scenarioMergeRules scenario
        , envLog          = \_ -> pure ()
        , envDebug        = debugRef
        , envTerminal     = scenarioTerminal scenario
        , envMessageLog   = msgRef
        , envPlayerId     = playerId
        , envPlayerCharId = you
        , envLogStore     = memoryLogStore accumRef
        , envAxiomTrace   = traceRef
        , envFrontier     = frontierRef
        , envLiveMerge    = \w -> pure (w, [])
        }
  idsRef <- newIORef actionIds
  result <- runApp env (scenarioInitial scenario)
              (coreLoop noopHook (scriptSource idsRef))
  case result of
    Left err     -> pure (Left err)
    Right (_, w) -> do
      entries  <- reverse <$> readIORef accumRef
      messages <- reverse <$> readIORef msgRef
      pure (Right (w, entries, messages))

-- ---------------------------------------------------------------------------
-- Per-turn recording
-- ---------------------------------------------------------------------------

data TurnRecord = TurnRecord
  { turnAvailable :: [String]         -- ^ labels of all available actions this turn
  , turnChosen    :: String           -- ^ label of the action taken
  , turnMessages  :: [NarrativeEntry] -- ^ narrative messages produced this turn
  } deriving (Show)

-- | Run a scenario headlessly by executing a fixed sequence of action IDs,
-- recording a 'TurnRecord' for each turn. Returns the final world and the
-- per-turn records in chronological order.
runHeadlessPlaythrough
  :: (CharacterId -> Scenario)
  -> PlayerId
  -> [ActionId]
  -> IO (Either AppError (GameWorld, [TurnRecord]))
runHeadlessPlaythrough mkScenario playerId actionIds = do
  let PlayerId pidStr = playerId
      you      = Named (take 12 pidStr)
      scenario = mkScenario you
  debugRef    <- newIORef Off
  msgRef      <- newIORef []
  accumRef    <- newIORef []
  traceRef    <- newIORef []
  frontierRef <- newIORef Map.empty
  recordsRef  <- newIORef []
  let env = Env
        { envActions      = scenarioActions scenario
        , envAxioms       = scenarioAxioms scenario
        , envMergeAxioms  = scenarioMergeAxioms scenario
        , envRules        = scenarioRules scenario
        , envMergeRules   = scenarioMergeRules scenario
        , envLog          = \_ -> pure ()
        , envDebug        = debugRef
        , envTerminal     = scenarioTerminal scenario
        , envMessageLog   = msgRef
        , envPlayerId     = playerId
        , envPlayerCharId = you
        , envLogStore     = memoryLogStore accumRef
        , envAxiomTrace   = traceRef
        , envFrontier     = frontierRef
        , envLiveMerge    = \w -> pure (w, [])
        }
  idsRef <- newIORef actionIds
  result <- runApp env (scenarioInitial scenario)
              (playthroughLoop idsRef msgRef recordsRef)
  case result of
    Left err     -> pure (Left err)
    Right (_, w) -> do
      records <- reverse <$> readIORef recordsRef
      pure (Right (w, records))

-- | Internal loop for 'runHeadlessPlaythrough'. Steps through the script one
-- action at a time, recording a 'TurnRecord' after each step.
playthroughLoop :: IORef [ActionId] -> IORef [NarrativeEntry] -> IORef [TurnRecord] -> App ()
playthroughLoop idsRef msgRef recordsRef = do
  world    <- get
  acts     <- asks envActions
  terminal <- asks envTerminal
  let available = filter (checkCondition world . anyActionCondition) acts
  mAction <- scriptSource idsRef available
  case mAction of
    Nothing -> pure ()
    Just (AnyAction action) -> do
      let availLabels = map anyActionLabel available
          chosenLabel = actionLabel action
      msgsBefore <- liftIO $ readIORef msgRef
      let oldLen = length msgsBefore
      worldBefore <- get
      pid         <- asks envPlayerId
      executeStep action
      worldFinal  <- get
      let diff = diffWorlds pid worldBefore worldFinal
      logAction (actionId action) diff
      msgsAfter <- liftIO $ readIORef msgRef
      let newLen   = length msgsAfter
          newMsgs  = reverse (take (newLen - oldLen) msgsAfter)
      liftIO $ modifyIORef' recordsRef (TurnRecord availLabels chosenLabel newMsgs :)
      unless (checkCondition worldFinal terminal)
             (playthroughLoop idsRef msgRef recordsRef)

-- ---------------------------------------------------------------------------
-- Action source implementations
-- ---------------------------------------------------------------------------

randomSource :: IORef StdGen -> Int -> IORef Int -> ActionSource
randomSource genRef maxSteps stepsRef actions = liftIO $ do
  n <- readIORef stepsRef
  if n >= maxSteps || null actions
    then pure Nothing
    else do
      gen <- readIORef genRef
      let (idx, gen') = randomR (0, length actions - 1) gen
      writeIORef genRef gen'
      modifyIORef' stepsRef (+ 1)
      pure (Just (actions !! idx))

scriptSource :: IORef [ActionId] -> ActionSource
scriptSource idsRef actions = liftIO $ do
  ids <- readIORef idsRef
  go ids
  where
    go []     = pure Nothing
    go (x:xs) = do
      writeIORef idsRef xs
      case find ((== x) . anyActionId) actions of
        Just a  -> pure (Just a)
        Nothing -> go xs
