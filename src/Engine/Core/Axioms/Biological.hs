module Engine.Core.Axioms.Biological
  ( fatigueSystemAxiom
  , tirednessSystemAxiom
  , hungerSystemAxiom
  , hungerStateSystemAxiom
  , charHasFatigue
  , charHasAnyFatigue
  , charHasHungerState
  ) where

import           Data.Maybe          (fromMaybe)
import qualified Data.Map.Strict     as Map

import           Engine.Author.DSL
import           Engine.Core.Conditions (getCharStat)
import           Engine.Core.Axioms.Shared (charIsSleeping)
import           Engine.CRDT.ORSet
import           Engine.Core.World
import           GameTypes

-- ---------------------------------------------------------------------------
-- Character tag queries (biological)
-- ---------------------------------------------------------------------------

charHasFatigue :: CharId -> FatigueLevel -> GameWorld -> Bool
charHasFatigue cid level world =
  case Map.lookup cid (worldCharacters world) of
    Just c  -> orMember (fatigueTag level) (charTags c)
    Nothing -> False

charHasAnyFatigue :: CharId -> GameWorld -> Bool
charHasAnyFatigue cid world =
  case Map.lookup cid (worldCharacters world) of
    Just c  -> any isFatigueTag (orToList (charTags c))
    Nothing -> False

charHasHungerState :: CharId -> HungerLevel -> GameWorld -> Bool
charHasHungerState cid level world =
  case Map.lookup cid (worldCharacters world) of
    Just c  -> orMember (hungerStateTag level) (charTags c)
    Nothing -> False

-- ---------------------------------------------------------------------------
-- Biological system axioms -- fatigue and hunger
-- ---------------------------------------------------------------------------

-- | Drains Strength each hour for every character while awake.
-- Restores Strength by 1/hr while sleeping.
-- Uses a fixed circadian curve: morning is easy, afternoon slumps,
-- night punishes you for being awake.
fatigueSystemAxiom :: Axiom
fatigueSystemAxiom = Axiom
  { axiomId       = SystemAxiom "fatigue"
  , axiomPriority = 3
  , axiomEvaluate = \world _actions diff ->
      let hourTicked = any isTimeTag (diffWorldTagsAdded diff)
          hour       = fromMaybe 12 (getHour world)
          chars      = Map.keys (worldCharacters world)
      in if not hourTicked then [] else
           concatMap (fatigueFor world hour) chars
  }
  where
    fatigueFor world hour cid =
      case getCharStat cid (Capacity Strength) world of
        Nothing -> []
        Just _  ->
          if charIsSleeping cid world
            then [modifyCharacterStatEffect cid (Capacity Strength) 1]
            else let drain = circadianDrain hour
                 in [modifyCharacterStatEffect cid (Capacity Strength) (negate drain) | drain /= 0]

    -- Circadian fatigue curve (drain per hour by time of day):
    --   00-05  heavy drain (2) -- deep night, body demands sleep
    --   06-09  no drain    (0) -- morning respite, cortisol peak
    --   10-12  steady      (1) -- late morning, mild wear
    --   13-15  heavy drain (2) -- post-lunch circadian dip
    --   16-20  steady      (1) -- evening, sustainable wakefulness
    --   21-23  heavy drain (2) -- late night, sleep pressure mounts
    circadianDrain h
      | h >= 0  && h < 6   = 2
      | h >= 6  && h < 10  = 0
      | h >= 10 && h < 13  = 1
      | h >= 13 && h < 16  = 2
      | h >= 16 && h < 21  = 1
      | otherwise           = 2

-- | Sets the Fatigue EngineTag on each character based on Strength thresholds.
-- Strength <= 3 -> Tired. Strength <= 1 -> Exhausted.
-- Clears fatigue when Strength rises above thresholds.
tirednessSystemAxiom :: Axiom
tirednessSystemAxiom = Axiom
  { axiomId       = SystemAxiom "tiredness"
  , axiomPriority = 4
  , axiomEvaluate = \world _actions diff ->
      let dropped = [ statDeltaChar d
                    | d <- diffStats diff
                    , statDeltaStat d == Capacity Strength
                    ]
      in concatMap (tirednessFor world) dropped
  }
  where
    exhaustionThreshold = 1
    tirednessThreshold  = 3

    tirednessFor world cid =
      case getCharStat cid (Capacity Strength) world of
        Nothing -> []
        Just s
          | s <= exhaustionThreshold -> setFatigue cid Exhausted world
          | s <= tirednessThreshold  -> setFatigue cid Tired world
          | otherwise                -> clearFatigue cid world

    setFatigue cid level world =
      [immediate (AddTag cid (fatigueTag level)) | not (charHasFatigue cid level world)]

    clearFatigue cid world =
      if charHasAnyFatigue cid world
        then [immediate (RemoveTag cid (fatigueTag Tired))
             , immediate (RemoveTag cid (fatigueTag Exhausted))]
        else []

-- | Drains Hunger by 1 per hour for every character while awake.
-- Auto-restores to 6 when Hunger hits 1.
hungerSystemAxiom :: Axiom
hungerSystemAxiom = Axiom
  { axiomId       = SystemAxiom "hunger"
  , axiomPriority = 3
  , axiomEvaluate = \world _actions diff ->
      let hourTicked = any isTimeTag (diffWorldTagsAdded diff)
          chars      = Map.keys (worldCharacters world)
      in if not hourTicked then [] else
           concatMap (hungerFor world) chars
  }
  where
    hungerAutoRestoreThreshold = 1
    hungerRestoreTarget        = 6

    hungerFor world cid =
      case getCharStat cid (Capacity Hunger) world of
        Nothing -> []
        Just hunger ->
          if charIsSleeping cid world then [] else
          if hunger <= hungerAutoRestoreThreshold
            then [modifyCharacterStatEffect cid (Capacity Hunger) (hungerRestoreTarget - hunger)]
            else [modifyCharacterStatEffect cid (Capacity Hunger) (-1)]

-- | Sets the HungerState EngineTag on each character based on Hunger thresholds.
-- Hunger <= 2 -> Hungry. Hunger <= 4 -> Peckish. Above 4 -> Satiated.
hungerStateSystemAxiom :: Axiom
hungerStateSystemAxiom = Axiom
  { axiomId       = SystemAxiom "hungerState"
  , axiomPriority = 4
  , axiomEvaluate = \world _actions diff ->
      let changed = [ statDeltaChar d
                    | d <- diffStats diff
                    , statDeltaStat d == Capacity Hunger
                    ]
      in concatMap (hungerStateFor world) changed
  }
  where
    hungryThreshold  = 2
    peckishThreshold = 4

    hungerStateFor world cid =
      case getCharStat cid (Capacity Hunger) world of
        Nothing -> []
        Just h
          | h <= hungryThreshold  -> setHunger cid Hungry world
          | h <= peckishThreshold -> setHunger cid Peckish world
          | otherwise             -> setHunger cid Satiated world

    setHunger cid level world =
      [immediate (AddTag cid (hungerStateTag level)) | not (charHasHungerState cid level world)]
