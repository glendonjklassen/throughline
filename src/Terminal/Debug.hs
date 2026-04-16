{-# OPTIONS_GHC -fno-hpc #-}
-- | Debug overlay: learning mode display with axiom traces and world state inspection.
module Terminal.Debug where

import qualified Data.Map.Strict     as Map
import           Data.List           (intercalate, partition)

import           Control.Monad          (unless, when)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.IORef

import           Engine.Core.Conditions (getCharStat)
import           Engine.CRDT.ORSet
import           Engine.Core.World
import           Terminal.ANSI
import           GameTypes
import           MonadStack

debugBefore :: GameWorld -> App ()
debugBefore world = do
  mode <- liftIO . readIORef =<< asks envDebug
  when (mode == Before) $ showWorldState world

debugAfter :: GameWorld -> App ()
debugAfter world = do
  mode <- liftIO . readIORef =<< asks envDebug
  when (mode == After) $ showWorldState world

debugWorldDiff :: WorldDiff -> App ()
debugWorldDiff diff = do
  mode <- liftIO . readIORef =<< asks envDebug
  if mode /= Diff then return () else do
    logEffect (grey "--- World Diff ---")
    mapM_ showStatDelta    (diffStats diff)
    mapM_ showRelDelta     (diffRelations diff)
    mapM_ (showTagChange "+") (diffTagsAdded    diff)
    mapM_ (showTagChange "-") (diffTagsRemoved  diff)
    unless (null (diffWorldTagsAdded   diff)) $
      logEffect ("  world tags +: " <> show (diffWorldTagsAdded   diff))
    unless (null (diffWorldTagsRemoved diff)) $
      logEffect ("  world tags -: " <> show (diffWorldTagsRemoved diff))
    logEffect "-------------------"

showWorldState :: GameWorld -> App ()
showWorldState world = do
  logEffect (grey "--- World State ---")
  logEffect (grey ("  Effects: " <> show (length (worldActiveEffects world))))
  mapM_ debugEffect (worldActiveEffects world)
  logEffect "  Characters:"
  mapM_ debugCharacter (Map.elems (worldCharacters world))
  logEffect "  Relationships:"
  debugRelationships (worldGraph world)
  debugWorldTags world
  logEffect "-------------------"

showStatDelta :: StatDelta -> App ()
showStatDelta d =
  logEffect ("  " <> show (statDeltaChar d)
    <> " " <> show (statDeltaStat d)
    <> ": " <> show (statDeltaOld d)
    <> " -> " <> show (statDeltaNew d))

showRelDelta :: RelationDelta -> App ()
showRelDelta d =
  logEffect ("  " <> show (relationDeltaFrom d)
    <> " -> " <> show (relationDeltaTo d)
    <> " " <> show (relationDeltaStat d)
    <> ": " <> show (relationDeltaOld d)
    <> " -> " <> show (relationDeltaNew d))

showTagChange :: String -> (CharId, Tag) -> App ()
showTagChange sign (cid, tag) =
  logEffect ("  " <> show cid <> " tag " <> sign <> ": " <> show tag)

debugRelationships :: RelationshipGraph -> App ()
debugRelationships graph = do
  mapM_ debugTruthStats  (maybe [] Map.toList (Map.lookup Truth graph))
  mapM_ debugEdge (concatMap expandEdges (filter ((/= Truth) . fst) (Map.toList graph)))

expandEdges :: (CharId, Map.Map CharId Relationship) -> [(CharId, CharId, Relationship)]
expandEdges (from, edges) = map (\(to, rel) -> (from, to, rel)) (Map.toList edges)

debugTruthStats :: (CharId, Relationship) -> App ()
debugTruthStats (cid, rel) =
  logEffect ("  " <> show cid <> " actual"
    <> " | int: "  <> show (getRelStat (Capacity Intelligence) rel)
    <> " str: "    <> show (getRelStat (Capacity Strength) rel)
    <> " cha: "    <> show (getRelStat (Capacity Charisma) rel)
    <> " und: "    <> show (getRelStat (Capacity Understanding) rel)
    <> " hun: "    <> show (getRelStat (Capacity Hunger) rel)
    <> " soc: "    <> show (getRelStat (Capacity SocialStamina) rel))

debugEdge :: (CharId, CharId, Relationship) -> App ()
debugEdge (from, to, rel) =
  logEffect ("  " <> show from <> " -> " <> show to <> " | "
    <> "trust: " <> show (getRelStat Trust rel)
    <> " und: "  <> show (getRelStat (Capacity Understanding) rel))

debugEffect :: LiveEffect -> App ()
debugEffect le =
  let e = liveEffect le
  in logEffect ("  " <> showBody (effectBody e) <> " | lifetime: " <> show (effectLifetime e))

showBody :: EffectBody -> String
showBody (OnExpire inner _) = "OnExpire (" <> showBody inner <> ") ..."
showBody other              = show other

debugWorldTags :: GameWorld -> App ()
debugWorldTags world = do
  let (taken, other) = partition isActionTaken (orToList (worldTags world))
  logEffect ("World tags: " <> show other)
  logEffect ("Actions taken: " <> show taken)

isActionTaken :: Tag -> Bool
isActionTaken (EngineTag (ActionTaken _)) = True
isActionTaken _                           = False

debugCharacter :: Character -> App ()
debugCharacter c =
  logEffect (charName c <> " | Tags: " <> show (charTags c))

-- ---------------------------------------------------------------------------
-- Learning mode (pure — returns lines for the left pane, no effects)
-- ---------------------------------------------------------------------------

-- | Build static lines for the left pane showing which axioms fired and
-- what hookable state is available. Returns empty when not in Learning mode.
learningModeLines :: [AxiomTrace] -> CharId -> GameWorld -> [String]
learningModeLines traces you world =
  let fired = filter (not . null . traceEffects) traces
      axiomLines = concatMap formatTrace fired
      hookLines  = hookableStateLines you world
  in if null axiomLines && null hookLines then [] else
     [dim "── axioms ────────────────────────"]
     ++ axiomLines
     ++ [dim "── hooks ─────────────────────────"]
     ++ hookLines

formatTrace :: AxiomTrace -> [String]
formatTrace (AxiomTrace aid _priority effects) =
  let label = case aid of
        SystemAxiom  s  -> dim "sys" <> " " <> s
        ScenarioAxiom s -> dim "scn" <> " " <> s
  in label : map (\e -> "  " <> dim (summarizeEffect (effectBody e))) effects

summarizeEffect :: EffectBody -> String
summarizeEffect (ModifyRelation Truth cid stat n) =
  show cid <> " " <> show stat <> " " <> showDelta n
summarizeEffect (ModifyRelation from to stat n) =
  show from <> " -> " <> show to <> " " <> show stat <> " " <> showDelta n
summarizeEffect (AddTag cid tag) =
  show cid <> " +" <> show tag
summarizeEffect (RemoveTag cid tag) =
  show cid <> " -" <> show tag
summarizeEffect (AddWorldTag tag) = "world +" <> show tag
summarizeEffect (RemoveWorldTag tag) = "world -" <> show tag
summarizeEffect (Narrate s) = "narrate: " <> take 50 s
summarizeEffect (SetLocation cid loc) = show cid <> " -> " <> show loc
summarizeEffect other = show other

showDelta :: Int -> String
showDelta n | n > 0     = "+" <> show n
            | otherwise = show n

hookableStateLines :: CharId -> GameWorld -> [String]
hookableStateLines cid world =
  let charTags' = case Map.lookup cid (worldCharacters world) of
        Just c  -> filter isEngineCharTag (orToList (charTags c))
        Nothing -> []
      stats = [ (s, v)
              | s <- [minBound..maxBound] :: [CapacityStat]
              , Just v <- [getCharStat cid (Capacity s) world]
              , v /= 0
              ]
      tagLine  = [dim "tags: " <> intercalate ", " (map show charTags') | not (null charTags')]
      statLine = [dim "stats: " <> intercalate ", " (map showStat stats) | not (null stats)]
  in tagLine ++ statLine
  where
    isEngineCharTag (EngineTag _) = True
    isEngineCharTag _             = False
    showStat (s, v) = show s <> " " <> show v

cycleDebug :: DebugMode -> DebugMode
cycleDebug Off      = Before
cycleDebug Before   = After
cycleDebug After    = Diff
cycleDebug Diff     = Learning
cycleDebug Learning = Off

describeDebug :: DebugMode -> String
describeDebug Off      = "off."
describeDebug Before   = "world before action."
describeDebug After    = "world after action."
describeDebug Diff     = "world diff."
describeDebug Learning = "learning mode."
