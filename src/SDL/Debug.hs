{-# OPTIONS_GHC -fno-hpc #-}
-- | Debug overlay: learning mode display with axiom traces and world state inspection.
module SDL.Debug
  ( learningModeLines
  , cycleDebug
  ) where

import qualified Data.Map.Strict     as Map
import           Data.List           (intercalate)

import           Engine.Core.Conditions (getCharacterStat)
import           Engine.CRDT.ORSet
import           SDL.Text
import           GameTypes

-- ---------------------------------------------------------------------------
-- Learning mode (pure — returns lines for the left pane, no effects)
-- ---------------------------------------------------------------------------

-- | Build static lines for the left pane showing which axioms fired and
-- what hookable state is available. Returns empty when not in Learning mode.
learningModeLines :: [AxiomTrace] -> CharacterId -> GameWorld -> [String]
learningModeLines traces you world =
  let fired = filter (not . null . traceEffects) traces
      axiomLines = concatMap formatTrace fired
      hookLines  = hookableStateLines you world
  in if null axiomLines && null hookLines then [] else
     [ansiDim "── axioms ────────────────────────"]
     ++ axiomLines
     ++ [ansiDim "── hooks ─────────────────────────"]
     ++ hookLines

formatTrace :: AxiomTrace -> [String]
formatTrace (AxiomTrace aid _priority effects) =
  let label = case aid of
        SystemAxiom  s  -> ansiDim "sys" <> " " <> s
        ScenarioAxiom s -> ansiDim "scn" <> " " <> s
  in label : map (\e -> "  " <> ansiDim (summarizeEffect (effectBody e))) effects

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

hookableStateLines :: CharacterId -> GameWorld -> [String]
hookableStateLines cid world =
  let charTags' = case Map.lookup cid (worldCharacters world) of
        Just c  -> filter isEngineCharTag (orToList (charTags c))
        Nothing -> []
      stats = [ (s, v)
              | s <- [minBound..maxBound] :: [CapacityStat]
              , Just v <- [getCharacterStat cid (Capacity s) world]
              , v /= 0
              ]
      tagLine  = [ansiDim "tags: " <> intercalate ", " (map show charTags') | not (null charTags')]
      statLine = [ansiDim "stats: " <> intercalate ", " (map showStat stats) | not (null stats)]
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
