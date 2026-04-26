-- | Structured narrative message types emitted by the engine (Say, Think, Narrate, Effect, Dialogue).
module Engine.Core.NarrativeMessage
  ( NarrativeMessage (..)
  , NarrativeEntry (..)
  ) where

import GameTypes (CharacterId)

-- | Structured message emitted by the engine when an effect fires.
-- The engine produces these; the display layer decides how to render them.
data NarrativeMessage
  = MsgSay     CharacterId String [CharacterId] [String] String
    -- ^ speaker charId, speaker name, listener charIds, listener names, text
  | MsgThink   CharacterId String          -- ^ thinker charId, text
  | MsgNarrate String                 -- ^ narration prose ("> ...")
  | MsgEffect  String                 -- ^ effect narration from renderNarrative
  | MsgDialogue [(CharacterId, String, [CharacterId], [String], String)]
    -- ^ dialogue lines: (speaker charId, speaker name, listener charIds, listener names, text)
  deriving (Show, Eq)

-- | A narrative message with world-state context from when it was emitted.
data NarrativeEntry = NarrativeEntry
  { neMessage   :: NarrativeMessage
  , neTension   :: Int
  , neTimeLabel :: String
  } deriving (Show, Eq)
