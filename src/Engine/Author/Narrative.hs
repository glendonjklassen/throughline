-- | Default prose generation from effects, tiered by character Understanding stat.
module Engine.Author.Narrative where

import qualified Data.Map.Strict as Map

import           GameTypes

narrateEffect :: CharId -> GameWorld -> EffectBody -> Maybe String
narrateEffect you w (ModifyRelation from to Trust n)
  | n > 0     = tiered you w
      (name from w <> " feels something ease between them and " <> name to w <> ".")
      ("Something eases between " <> name from w <> " and " <> name to w <> ".")
      "You sense a subtle warmth in the air."
  | n < 0     = tiered you w
      ("Something shifts in how " <> name from w <> " regards " <> name to w <> ".")
      ("Something changes between " <> name from w <> " and " <> name to w <> ".")
      "You sense a subtle tension in the air."
  | otherwise  = Nothing
narrateEffect _ _ ModifyRelation {} = Nothing
narrateEffect _ _ (Think _ _)      = Nothing  -- renders itself in executeBody
narrateEffect _ _ (Narrate _)      = Nothing  -- renders itself in executeBody
narrateEffect _ _ NarratePool {}   = Nothing  -- renders itself in executeBody
narrateEffect _ _ (Dialogue _)     = Nothing  -- renders itself in executeBody
narrateEffect _ _ Say {}           = Nothing  -- renders itself in executeBody
narrateEffect _ _ (AddTag _ _)     = Nothing
narrateEffect _ _ (AddWorldTag _)  = Nothing
narrateEffect _ _ (RemoveTag _ _)  = Nothing
narrateEffect _ _ (RemoveWorldTag _) = Nothing
narrateEffect _ _ (SetLocation _ _)  = Nothing
narrateEffect _ _ SetLocationRandom {} = Nothing
narrateEffect _ _ (SetLocationAdjacent _ _) = Nothing
narrateEffect _ _ SetLocationAdjacentPrefer {} = Nothing
narrateEffect _ _ (OnExpire _ _)   = Nothing
narrateEffect _ _ (CycleMany _ _)  = Nothing
narrateEffect _ _ (Cycle {})       = Nothing
narrateEffect _ _ (JournalEntry _) = Nothing  -- writes silently to worldJournal
narrateEffect _ _ AdvanceDay       = Nothing
narrateEffect _ _ DoNothing        = Nothing

-- | Select narration detail based on the player's Understanding.
-- High (>=7): precise, character-named prose.
-- Mid (>=3): directional but impersonal.
-- Low (>=1): barely perceptible.
-- Zero: silence.
tiered :: CharId -> GameWorld -> String -> String -> String -> Maybe String
tiered you w hi mid lo =
  case playerUnderstanding you w of
    p | p >= 7   -> Just hi
      | p >= 3   -> Just mid
      | p >= 1   -> Just lo
      | otherwise -> Nothing

playerUnderstanding :: CharId -> GameWorld -> Int
playerUnderstanding you w =
  maybe 0 (getRelStat (Capacity Understanding))
    (Map.lookup Truth (worldGraph w) >>= Map.lookup you)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

name :: CharId -> GameWorld -> String
name cid w = maybe (show cid) charName (Map.lookup cid (worldCharacters w))
