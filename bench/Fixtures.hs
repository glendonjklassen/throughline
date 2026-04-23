module Fixtures where

import qualified Data.Map.Strict     as Map
import           Data.IORef          (newIORef)

import           Engine.Author.DSL   (staticLive, timed)
import           Engine.CRDT.ORSet   (orEmpty, orFromList)
import           Engine.Core.World   (setCharacterStat)
import           Engine.Sync.EventLog (nullLogStore)
import           GameTypes
import           MonadStack

-- ---------------------------------------------------------------------------
-- Character IDs
-- ---------------------------------------------------------------------------

player :: CharId
player = Named "player"

npc1 :: CharId
npc1 = Named "npc1"

npc2 :: CharId
npc2 = Named "npc2"

-- ---------------------------------------------------------------------------
-- Scaled world builders
-- ---------------------------------------------------------------------------

-- | World with N characters, each with full stats and bilateral trust edges.
-- Useful for benchmarking diffWorlds, checkCondition, etc. at varying scale.
scaledWorld :: Int -> GameWorld
scaledWorld n = GameWorld
  { worldCharacters      = Map.fromList chars
  , worldGraph           = graph
  , worldLocations       = Map.fromList locs
  , worldActiveEffects   = []
  , worldTags            = orFromList (map (ScenarioTag . MkScenarioTag . ("tag-" ++) . show) [1 .. min n 20])
  , worldClock           = LamportClock 0 (PlayerId "bench")
  , worldLocationGraph   = emptyLocationGraph
  , worldSeed            = 0
  , worldLocationHistory = Map.empty
  , worldLocationVisits  = Map.empty
  , worldJournal         = []
  , worldDayNumber       = 1
  }
  where
    charIds = [ Named ("char-" ++ show i) | i <- [1..n] ]
    chars   = [ (cid, Character cid ("Char " ++ show i) [] orEmpty)
              | (i, cid) <- zip [(1::Int)..] charIds
              ]
    locs    = [ (cid, Location ("loc-" ++ show (i `mod` 3)))
              | (i, cid) <- zip [(0::Int)..] charIds
              ]
    graph   = foldr addStats Map.empty charIds
    addStats cid =
      setCharacterStat cid (Capacity Intelligence)  5
      . setCharacterStat cid (Capacity Strength)      5
      . setCharacterStat cid (Capacity Charisma)      5
      . setCharacterStat cid (Capacity Understanding) 5
      . setCharacterStat cid (Capacity Hunger)        5
      . setCharacterStat cid (Capacity SocialStamina) 5

-- | Like scaledWorld but with some stats mutated, creating diffs when compared.
scaledWorldMutated :: Int -> GameWorld
scaledWorldMutated n = w
  { worldGraph = mutateStats (worldGraph w)
  , worldTags  = orFromList (map (ScenarioTag . MkScenarioTag . ("tag-" ++) . show) [1 .. min n 20]
                          ++ [ScenarioTag (MkScenarioTag "extra-tag")])
  }
  where
    w = scaledWorld n
    -- Bump Intelligence for every other character
    mutateStats g = foldr bump g [ Named ("char-" ++ show i) | i <- [1..n], even i ]
    bump cid = setCharacterStat cid (Capacity Intelligence) 7

-- | World with N active effects (cycled timers).
worldWithActiveEffects :: Int -> GameWorld
worldWithActiveEffects n = (scaledWorld 5)
  { worldActiveEffects = map staticLive effects }
  where
    effects = [ timed (n `div` 2 + 1) (AddWorldTag (ScenarioTag (MkScenarioTag ("fx-" ++ show i))))
              | i <- [1..n]
              ]

-- ---------------------------------------------------------------------------
-- Condition builders
-- ---------------------------------------------------------------------------

-- | Nested condition tree of depth d (binary tree of All/Any).
deepCondition :: Int -> Condition
deepCondition 0 = RelationAbove Truth player (Capacity Intelligence) 3
deepCondition d = All [Any [deepCondition (d-1), leaf1], deepCondition (d-1)]
  where
    leaf1 = HasWorldTag (ScenarioTag (MkScenarioTag "tag-1"))

-- | Flat condition with n conjuncts.
wideCondition :: Int -> Condition
wideCondition n = All [ RelationAbove Truth (Named ("char-" ++ show i)) (Capacity Strength) 3
                       | i <- [1..n]
                       ]

-- ---------------------------------------------------------------------------
-- App runner for benchmarks
-- ---------------------------------------------------------------------------

mkBenchEnv :: [Axiom] -> IO Env
mkBenchEnv axioms = do
  ref         <- newIORef Off
  msgRef      <- newIORef []
  traceRef    <- newIORef []
  frontierRef <- newIORef Map.empty
  pure Env
    { envActions      = const []
    , envAxioms       = axioms
    , envMergeAxioms  = []
    , envRules        = []
    , envMergeRules   = []
    , envLog          = \_ -> pure ()
    , envDebug        = ref
    , envTerminal     = Any []
    , envMessageLog   = msgRef
    , envPlayerId     = PlayerId "bench"
    , envPlayerCharId = player
    , envLogStore     = nullLogStore
    , envAxiomTrace   = traceRef
    , envFrontier     = frontierRef
    , envLiveMerge    = \w -> pure (w, [])
    }

runBenchApp :: Env -> GameWorld -> App a -> IO (a, GameWorld)
runBenchApp env world action = do
  result <- runApp env world action
  case result of
    Left err -> error ("Benchmark error: " <> show err)
    Right r  -> pure r
