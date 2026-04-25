{-# LANGUAGE DataKinds #-}
module Engine.Author.Dialogue where

import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           GameTypes
import           GameTypes.Types (Action(..))

-- ---------------------------------------------------------------------------
-- Local effect builders (inlined to avoid circular dependency with DSL)
-- ---------------------------------------------------------------------------

immediate' :: EffectBody -> Effect
immediate' body = Effect { effectBody = body, effectLifetime = Just 1, effectCondition = unconditional, effectNarrative = Nothing }

timed' :: Int -> EffectBody -> Effect
timed' n body = Effect { effectBody = body, effectLifetime = Just n, effectCondition = unconditional, effectNarrative = Nothing }

-- ---------------------------------------------------------------------------
-- Dialogue
-- ---------------------------------------------------------------------------

-- | A complete action that plays a dialogue sequence exactly once.
-- The action disappears permanently after playing (gated on actionTaken).
-- For sequences that should re-trigger or have a custom follow-up,
-- compose dialogueChain directly.
dialogueAction :: ActionId -> String -> NonEmpty (CharacterId, [CharacterId], String) -> Action 'Once
dialogueAction aid label ls = Action
  { actionId        = aid
  , actionLabel     = label
  , actionTarget    = Nothing
  , actionCondition = Not (HasWorldTag (actionTaken aid))
  , actionEffects   = dialogueChain aid ls
  }

-- | Chain a sequence of (speaker, listener, line) triples into a timed
-- effect sequence. Pushes ActionTaken did to world tags while the dialogue
-- is in play and removes it when the last line expires.
dialogueChain :: ActionId -> NonEmpty (CharacterId, [CharacterId], String) -> [Effect]
dialogueChain did ls =
  [ immediate' (AddWorldTag (actionTaken did))
  , immediate' (AddWorldTag dialogueInProgress)
  , dialogueChainThen ls (immediate' (RemoveWorldTag dialogueInProgress))
  ]

dialogueChainThen :: NonEmpty (CharacterId, [CharacterId], String) -> Effect -> Effect
dialogueChainThen ((c, l, w) :| [])   after = timed' 1 (OnExpire (Say c l w) after)
dialogueChainThen ((c, l, w) :| rest) after = timed' 1 (OnExpire (Say c l w) (dialogueChainThen (NE.fromList rest) after))

-- | Bracket a dialogue chain with dialogueInProgress so the
-- continueAction appears while lines are playing.
-- For dialogue that chains into specific follow-up effects after the
-- last line, use dialogueChainThen directly.
dialogue :: NonEmpty (CharacterId, [CharacterId], String) -> [Effect]
dialogue ls =
  [ immediate' (AddWorldTag dialogueInProgress)
  , dialogueChainThen ls (immediate' (RemoveWorldTag dialogueInProgress))
  ]

-- | Immediate dialogue: all lines fire on a single tick, no interleaving.
immediateDialogue :: ActionId -> NonEmpty (CharacterId, [CharacterId], String) -> [Effect]
immediateDialogue did ls =
  [ immediate' (AddWorldTag (actionTaken did))
  , immediate' (AddWorldTag dialogueInProgress)
  , immediate' (Dialogue ls)
  , immediate' (RemoveWorldTag dialogueInProgress)
  ]

-- | Two-person conversation: auto-infers that each speaker addresses the
-- other. Takes simple (speaker, text) pairs and fills in listeners.
-- Use this for the common case; use dialogue directly for group scenes
-- or undirected speech.
conversation :: CharacterId -> CharacterId -> NonEmpty (CharacterId, String) -> [Effect]
conversation a b ls = dialogue (fmap addListener ls)
  where addListener (speaker, text)
          | speaker == a = (speaker, [b], text)
          | otherwise    = (speaker, [a], text)

-- | Like conversation but chains into a follow-up effect after the last line.
conversationThen :: CharacterId -> CharacterId -> NonEmpty (CharacterId, String) -> Effect -> Effect
conversationThen a b ls = dialogueChainThen (fmap addListener ls)
  where addListener (speaker, text)
          | speaker == a = (speaker, [b], text)
          | otherwise    = (speaker, [a], text)

-- | A "Continue..." action that appears only while dialogue is in progress.
-- Scenarios that want it just include continueAction in their action list.
continueAction :: Action 'Repeatable
continueAction = Action
  { actionId        = ActionId "continue-dialogue"
  , actionLabel     = "Continue..."
  , actionTarget    = Nothing
  , actionCondition = HasWorldTag dialogueInProgress
  , actionEffects   = [immediate' DoNothing]
  }

-- ---------------------------------------------------------------------------
-- Targeted dialogue action
-- ---------------------------------------------------------------------------

-- | Like dialogueAction but directed at a specific entity (the conversation partner).
targetedDialogueAction :: ActionId -> String -> Entity -> NonEmpty (CharacterId, [CharacterId], String) -> Action 'Once
targetedDialogueAction aid label target ls = Action
  { actionId        = aid
  , actionLabel     = label
  , actionTarget    = Just target
  , actionCondition = Not (HasWorldTag (actionTaken aid))
  , actionEffects   = dialogueChain aid ls
  }
