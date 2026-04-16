module TestFixtures
  ( -- Characters
    player
  , npc
    -- Worlds
  , emptyWorld
  , twoCharWorld
    -- App runner helpers
  , mkEnv
  , runEffect
  , runApp'
  , runAppEither
    -- Scenario test helpers
  , mkScenarioEnv
  , step
  , tickUntil
    -- Misc
  , emptyDiff
  , arbUUID
  ) where

import           Data.IORef
import           Data.List          (find)
import qualified Data.Map.Strict as Map

import           Engine.Core.Conditions (checkCondition)
import           Engine.Core.Effects
import           Engine.Core.World    (setRelStat, setCharacterStat)
import           Engine.CRDT.ORSet
import           Engine.Sync.EventLog  (nullLogStore)
import           GameTypes
import           MonadStack

-- Import Generators so that any module which does `import TestFixtures` also
-- gets the Arbitrary instances in scope (they live in Generators as orphans).
-- arbUUID is re-exported here so existing callers remain unaffected.
import           Generators             (arbUUID)

-- ---------------------------------------------------------------------------
-- Characters
-- ---------------------------------------------------------------------------

player :: CharId
player = Named "player"

npc :: CharId
npc = Named "npc"

-- ---------------------------------------------------------------------------
-- Worlds
-- ---------------------------------------------------------------------------

emptyWorld :: GameWorld
emptyWorld = GameWorld
  { worldCharacters    = Map.empty
  , worldGraph         = Map.empty
  , worldLocations     = Map.empty
  , worldActiveEffects = []
  , worldTags          = orEmpty
  , worldClock         = LamportClock 0 (PlayerId "init")
  , worldLocationGraph = emptyLocationGraph
  , worldSeed          = 0
  }

-- | Two characters with trust 5 in each direction and all ground-truth stats at 5.
twoCharWorld :: GameWorld
twoCharWorld = emptyWorld
  { worldCharacters = Map.fromList
      [ (player, Character player "Player" [] orEmpty)
      , (npc,    Character npc    "NPC"    [] orEmpty)
      ]
  , worldGraph
      = setRelStat player npc    Trust         5
      . setRelStat npc    player Trust         5
      . setCharacterStat player (Capacity Intelligence)  5
      . setCharacterStat player (Capacity Strength)      5
      . setCharacterStat player (Capacity Charisma)      5
      . setCharacterStat player (Capacity Understanding) 5
      . setCharacterStat player (Capacity Hunger)        5
      . setCharacterStat player (Capacity SocialStamina) 5
      . setCharacterStat npc   (Capacity Intelligence)  5
      . setCharacterStat npc   (Capacity Strength)      5
      . setCharacterStat npc   (Capacity Charisma)      5
      . setCharacterStat npc   (Capacity Understanding) 5
      . setCharacterStat npc   (Capacity Hunger)        5
      . setCharacterStat npc   (Capacity SocialStamina) 5
      $ Map.empty
  }

-- ---------------------------------------------------------------------------
-- App runner
-- ---------------------------------------------------------------------------

mkEnv :: IO Env
mkEnv = do
  ref          <- newIORef Off
  msgRef       <- newIORef []
  traceRef     <- newIORef []
  frontierRef  <- newIORef Map.empty
  pure Env
    { envActions      = []
    , envAxioms       = []
    , envMergeAxioms  = []
    , envRules        = []
    , envMergeRules   = []
    , envLog          = \_ -> pure ()
    , envDebug        = ref
    , envTerminal     = Any []
    , envMessageLog   = msgRef
    , envPlayerId     = PlayerId "test"
    , envPlayerCharId = player
    , envLogStore     = nullLogStore
    , envAxiomTrace   = traceRef
    , envFrontier     = frontierRef
    , envLiveMerge    = \w -> pure (w, [])
    }

runEffect :: EffectBody -> GameWorld -> IO GameWorld
runEffect body world = do
  env <- mkEnv
  result <- runApp env world (executeBody body)
  case result of
    Left err     -> error ("Unexpected AppError in test: " <> show err)
    Right (_, w) -> pure w

runApp' :: GameWorld -> App a -> IO (a, GameWorld)
runApp' world action = do
  env <- mkEnv
  result <- runApp env world action
  case result of
    Left err -> error ("Unexpected AppError in test: " <> show err)
    Right r  -> pure r

-- | Like runApp' but returns Either, allowing tests to assert expected failures.
runAppEither :: GameWorld -> App a -> IO (Either AppError (a, GameWorld))
runAppEither world action = do
  env <- mkEnv
  runApp env world action

-- ---------------------------------------------------------------------------
-- Scenario test helpers
-- ---------------------------------------------------------------------------

-- | Build an 'Env' wired to a specific scenario and player character.
-- Sets all 'Env' fields including 'envMergeAxioms' and 'envFrontier'.
mkScenarioEnv :: CharId -> Scenario -> IO Env
mkScenarioEnv cid scenario = do
  debugRef    <- newIORef Off
  msgRef      <- newIORef []
  traceRef    <- newIORef []
  frontierRef <- newIORef Map.empty
  pure Env
    { envActions      = scenarioActions scenario
    , envAxioms       = scenarioAxioms scenario
    , envMergeAxioms  = scenarioMergeAxioms scenario
    , envRules        = scenarioRules scenario
    , envMergeRules   = scenarioMergeRules scenario
    , envLog          = \_ -> pure ()
    , envDebug        = debugRef
    , envTerminal     = scenarioTerminal scenario
    , envMessageLog   = msgRef
    , envPlayerId     = PlayerId "test"
    , envPlayerCharId = cid
    , envLogStore     = nullLogStore
    , envAxiomTrace   = traceRef
    , envFrontier     = frontierRef
    , envLiveMerge    = \w -> pure (w, [])
    }

-- | Execute a named action, returning the updated world. Errors if the action
-- is not currently available.
step :: Env -> ActionId -> GameWorld -> IO GameWorld
step env aid world = do
  let available = filter (checkCondition world . anyActionCondition) (envActions env)
  case find (\a -> anyActionId a == aid) available of
    Nothing -> error $ "Action not available: " <> actionIdText aid
                    <> "\nAvailable: " <> show (map anyActionId available)
    Just (AnyAction action) -> do
      result <- runApp env world (executeStep action)
      case result of
        Left err     -> error $ "AppError in test: " <> show err
        Right (_, w) -> pure w

-- | Execute @wait@ repeatedly until the condition holds. Guards against
-- infinite loops with a step limit.
tickUntil :: Condition -> Env -> GameWorld -> IO GameWorld
tickUntil cond env = go (200 :: Int)
  where
    go 0 _ = error "tickUntil: condition never fired within 200 steps"
    go n w
      | checkCondition w cond = pure w
      | otherwise             = step env (ActionId "wait") w >>= go (n - 1)

emptyDiff :: WorldDiff
emptyDiff = WorldDiff [] [] [] [] [] [] []
