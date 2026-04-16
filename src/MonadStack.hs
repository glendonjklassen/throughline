-- | App monad transformer stack (ReaderT Env over StateT GameWorld, ExceptT, IO) and supporting types.
module MonadStack where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.IORef

import           Engine.Core.NarrativeMessage
import           GameTypes

data AppError
  = NotFound String
  | InvalidAction String
  deriving (Show)

data Env = Env
  { envActions        :: [AnyAction]
  , envAxioms         :: [Axiom]
  , envMergeAxioms    :: [MergeAxiom]
  , envRules          :: [AxiomRule]
  , envMergeRules     :: [MergeAxiomRule]
  , envLog            :: String -> IO ()
  , envDebug          :: IORef DebugMode
  , envTerminal       :: Condition
  , envMessageLog     :: IORef [NarrativeEntry]  -- ^ structured narrative entries, newest first
  , envPlayerId       :: PlayerId
  , envPlayerCharId   :: CharId                    -- ^ the player's own CharId
  , envLogStore       :: LogStore
  , envAxiomTrace     :: IORef [AxiomTrace]        -- ^ last tick's axiom traces (for learning mode)
  , envFrontier       :: IORef CausalFrontier      -- ^ causal frontier: latest seen entry per sync partner
  , envLiveMerge      :: GameWorld -> IO (GameWorld, [(String, Int)])
    -- ^ Between-turn merge: scan for new foreign log entries and apply them.
    -- Returns the merged world and a list of (displayName, entryCount) for
    -- each player whose entries were folded in.
  }

type BaseStack       = ExceptT AppError IO
type StatefulStack   = StateT GameWorld BaseStack
type App a           = ReaderT Env StatefulStack a
type AppResult a     = Either AppError (a, GameWorld)

runApp :: Env -> GameWorld -> App a -> IO (AppResult a)
runApp env world app =
  runExceptT
    (runStateT
      (runReaderT app env)
      world)
