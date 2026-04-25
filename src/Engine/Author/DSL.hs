{-# LANGUAGE DataKinds #-}
module Engine.Author.DSL
  ( module Engine.Author.DSL
  , module Engine.Author.Dialogue
  , module Engine.Author.MergeHelpers
  ) where

import           Data.List.NonEmpty (NonEmpty(..))
import qualified Engine.CRDT.ORSet      as ORSet
import           Engine.CRDT.ORSet      (ORSet, initToken)
import qualified Engine.CRDT.TombstoneGC as TombstoneGC
import           Engine.Core.Conditions (checkCondition)
import           GameTypes
import           GameTypes.Types (Action(..))

import           Engine.Author.Dialogue
import           Engine.Author.MergeHelpers

-- ---------------------------------------------------------------------------
-- EffectBody builders — terse constructors for common effect bodies
-- ---------------------------------------------------------------------------

-- | Directed speech: one character says something to another.
sayTo :: CharId -> CharId -> String -> EffectBody
sayTo speaker listener = Say speaker [listener]

-- | Speech addressed to multiple listeners (e.g. addressing an audience).
sayToMany :: CharId -> [CharId] -> String -> EffectBody
sayToMany = Say

-- | Undirected speech: said to the room, no specific listener.
sayToRoom :: CharId -> String -> EffectBody
sayToRoom speaker = Say speaker []

-- | Internal thought. No audience by definition.
think :: CharId -> String -> EffectBody
think = Think

-- | Move a character to a location.
moveTo :: CharId -> Location -> EffectBody
moveTo = SetLocation

-- | Append a line to the player's journal.  Renders no prose — the
-- entry is visible when the player opens the journal.  Pair with a
-- regular 'Narrate' if you also want the event surfaced in the moment.
journal :: String -> EffectBody
journal = JournalEntry

-- ---------------------------------------------------------------------------
-- Effect builders
-- ---------------------------------------------------------------------------

-- | An effect that lasts forever, unconditionally.
eternal :: EffectBody -> Effect
eternal body = Effect { effectBody = body, effectLifetime = Nothing, effectCondition = unconditional, effectNarrative = Nothing }

-- | An effect that lasts forever, guarded by a condition.
eternalWhen :: Condition -> EffectBody -> Effect
eternalWhen c body = Effect { effectBody = body, effectLifetime = Nothing, effectCondition = c, effectNarrative = Nothing }

-- | An effect that fires once, unconditionally.
immediate :: EffectBody -> Effect
immediate body = Effect { effectBody = body, effectLifetime = Just 1, effectCondition = unconditional, effectNarrative = Nothing }

-- | An effect that fires once, guarded by a condition.
immediateWhen :: Condition -> EffectBody -> Effect
immediateWhen c body = Effect { effectBody = body, effectLifetime = Just 1, effectCondition = c, effectNarrative = Nothing }

-- | An effect active for n ticks, unconditionally.
timed :: Int -> EffectBody -> Effect
timed n body = Effect { effectBody = body, effectLifetime = Just n, effectCondition = unconditional, effectNarrative = Nothing }

-- | An effect active for n ticks, guarded by a condition.
timedWhen :: Int -> Condition -> EffectBody -> Effect
timedWhen n c body = Effect { effectBody = body, effectLifetime = Just n, effectCondition = c, effectNarrative = Nothing }

-- | Override the engine's default narrative for an effect.
withNarrative :: String -> Effect -> Effect
withNarrative prose e = e { effectNarrative = Just prose }

-- | An effect that fires once with an author narrative override.
immediateNarrated :: String -> EffectBody -> Effect
immediateNarrated prose body = withNarrative prose (immediate body)

-- | An effect active for n ticks with an author narrative override.
timedNarrated :: Int -> String -> EffectBody -> Effect
timedNarrated n prose body = withNarrative prose (timed n body)

-- | An effect that lasts forever with an author narrative override.
eternalNarrated :: String -> EffectBody -> Effect
eternalNarrated prose body = withNarrative prose (eternal body)

-- | Fire an effect once after n ticks, but only if the given condition holds
-- at each intermediate tick. If the condition fails at any point, the chain
-- is dropped and the final effect never fires.
--
-- Use this to model "if this keeps happening" or "if you keep doing this":
-- something that only resolves if a state persists uninterrupted. E.g. a
-- character staying in a location, a mode remaining active, or sustained
-- attention. Putting the player in a different state mid-chain cancels it.
--
-- The condition gates each intermediate step. The final Effect fires
-- according to its own effectCondition (caller's responsibility). If you
-- want the chain to be fully interruptible up to the last tick, pass the
-- same condition as the final effect's guard (e.g. immediateWhen cond body).
ifItPersists :: Int -> Condition -> Effect -> Effect
ifItPersists n cond final = timed 1 (OnExpire DoNothing (go (max 1 n)))
  where
    go 1 = final
    go k = timedWhen 1 cond (OnExpire DoNothing (go (k - 1)))

-- | Like ifItPersists, but fully interruptible at every tick including the
-- last — the same condition gates both the intermediate steps and the final
-- effect. Use this when "stopping" should always prevent the outcome.
-- Use ifItPersists directly when you need a different final condition
-- (e.g. unconditional: "consequence fires regardless once the chain completes").
whileItPersists :: Int -> Condition -> EffectBody -> Effect
whileItPersists n cond body = ifItPersists n cond (immediateWhen cond body)

-- | Alternate between two effects, each lasting t ticks.
effectCycle :: EffectBody -> EffectBody -> Int -> Effect
effectCycle e1 e2 t = timed t (Cycle t e1 e2)

-- | Cycle through a list of effects in order, each lasting t ticks.
effectCycleMany :: Int -> NonEmpty EffectBody -> Effect
effectCycleMany t bodies = timed t (CycleMany t bodies)

-- ---------------------------------------------------------------------------
-- Sequencing helpers
-- ---------------------------------------------------------------------------

-- | Fire an effect body once, then expire. Use for one-tick delays in chains.
-- Replaces the verbose: timed 1 (OnExpire body (immediate DoNothing))
delayed :: EffectBody -> Effect
delayed body = timed 1 (OnExpire body (immediate DoNothing))

-- ---------------------------------------------------------------------------
-- Tag helpers
-- ---------------------------------------------------------------------------

-- | Add multiple world tags at once.
addTags :: [Tag] -> [Effect]
addTags = map (immediate . AddWorldTag)

-- | Remove multiple world tags at once.
removeTags :: [Tag] -> [Effect]
removeTags = map (immediate . RemoveWorldTag)

-- | Check whether a world tag is present. Shorthand for checkCondition + HasWorldTag.
hasTag :: GameWorld -> Tag -> Bool
hasTag w t = checkCondition w (HasWorldTag t)

-- | Empty tag set.  Use for initial 'Character' tags or empty
-- 'worldTags' in scenario init.
emptyTags :: ORSet Tag
emptyTags = ORSet.orEmpty

-- | Build an initial tag set from a list.  Use for 'worldTags' at
-- scenario init.
tagsFromList :: [Tag] -> ORSet Tag
tagsFromList = ORSet.orFromList

-- | All world tags currently present, as a plain list.  Use this
-- instead of reaching into the ORSet directly when iterating over
-- tags in axioms or scenario logic.
worldTagList :: GameWorld -> [Tag]
worldTagList = ORSet.orToList . worldTags

-- | Tombstone GC schedule that drops entries older than @n@ days.
-- Pass to @scenarioTombstoneGC = Just (olderThanDays 365)@.
olderThanDays :: Integer -> TombstoneGCRule
olderThanDays = TombstoneGC.olderThanDays

-- ---------------------------------------------------------------------------
-- Relationship helpers (DSL aliases for the generalized relation effects)
-- ---------------------------------------------------------------------------

-- | Modify the Trust relation from one character to another by a delta.
modifyTrust :: CharId -> CharId -> Int -> Effect
modifyTrust from to delta = immediate (ModifyRelation from to Trust delta)

-- | Symmetric trust modification — both characters gain the same delta.
mutualTrust :: CharId -> CharId -> Int -> [Effect]
mutualTrust a b n = [modifyTrust a b n, modifyTrust b a n]

-- | Asymmetric bidirectional trust — each direction gets its own delta.
bidirectionalTrust :: CharId -> CharId -> Int -> Int -> [Effect]
bidirectionalTrust a b ab ba = [modifyTrust a b ab, modifyTrust b a ba]

-- | Condition: a character's ground truth stat exceeds a threshold.
trueStatAbove :: CharId -> StatType -> Int -> Condition
trueStatAbove = RelationAbove Truth

-- | Alias for trueStatAbove.
statAbove :: CharId -> StatType -> Int -> Condition
statAbove = trueStatAbove

-- | Effect: modify a character's ground truth stat by a delta.
modifyCharacterStatEffect :: CharId -> StatType -> Int -> Effect
modifyCharacterStatEffect cid stat n = immediate (ModifyRelation Truth cid stat n)

-- | Condition: Trust from one character to another exceeds a threshold.
trustAbove :: CharId -> CharId -> Int -> Condition
trustAbove from to = RelationAbove from to Trust

-- | Condition: a character is at the given location.
atLocation :: CharId -> Location -> Condition
atLocation = AtLocation

-- | Apply a location gate to a list of actions.
-- The location condition is ANDed with each action's existing condition.
atScene :: CharId -> Location -> [AnyAction] -> [AnyAction]
atScene cid loc = map gate
  where
    gate (AnyAction a) = AnyAction (a { actionCondition = All [AtLocation cid loc, actionCondition a] })

-- ---------------------------------------------------------------------------
-- Action builders
-- ---------------------------------------------------------------------------

-- | An action that can only be taken once (tracked via a world tag).
onceAction :: ActionId -> String -> Condition -> [Effect] -> Action 'Once
onceAction aid label cond effs = Action
  { actionId        = aid
  , actionLabel     = label
  , actionTarget    = Nothing
  , actionCondition = All [cond, Not (HasWorldTag (actionTaken aid))]
  , actionEffects   = effs ++ [immediate (AddWorldTag (actionTaken aid))]
  }

-- | A pair of mutually exclusive actions sharing a world tag.
-- The first action is available when the tag is absent and adds it;
-- the second is available when the tag is present and removes it.
-- Both actions are additionally gated by the shared condition.
togglePair
  :: ActionId           -- ^ base id (suffixed with ":on" / ":off")
  -> Tag                -- ^ world tag that represents the on/off state
  -> Condition          -- ^ shared condition (e.g. Not (HasWorldTag backAtTruck))
  -> String -> [Effect] -- ^ activate label + extra effects
  -> String -> [Effect] -- ^ deactivate label + extra effects
  -> (Action 'Repeatable, Action 'Repeatable)
togglePair stateId tag cond activateLabel onEffects deactivateLabel offEffects = (activate, deactivate)
  where
    activate   = repeatableAction (ActionId (actionIdText stateId <> ":on"))  activateLabel   (All [cond, Not (HasWorldTag tag)]) (immediate (AddWorldTag tag)    : onEffects)
    deactivate = repeatableAction (ActionId (actionIdText stateId <> ":off")) deactivateLabel (All [cond, HasWorldTag tag])       (immediate (RemoveWorldTag tag) : offEffects)

-- | A standard repeatable action.
repeatableAction :: ActionId -> String -> Condition -> [Effect] -> Action 'Repeatable
repeatableAction aid label cond effs = Action
  { actionId        = aid
  , actionLabel     = label
  , actionTarget    = Nothing
  , actionCondition = cond
  , actionEffects   = effs
  }

-- ---------------------------------------------------------------------------
-- Targeted action builders
-- ---------------------------------------------------------------------------

-- | Like onceAction but directed at a specific entity.
targetedOnceAction :: ActionId -> String -> Entity -> Condition -> [Effect] -> Action 'Once
targetedOnceAction aid label target cond effs = Action
  { actionId        = aid
  , actionLabel     = label
  , actionTarget    = Just target
  , actionCondition = All [cond, Not (HasWorldTag (actionTaken aid))]
  , actionEffects   = effs ++ [immediate (AddWorldTag (actionTaken aid))]
  }

-- | Like repeatableAction but directed at a specific entity.
targetedRepeatableAction :: ActionId -> String -> Entity -> Condition -> [Effect] -> Action 'Repeatable
targetedRepeatableAction aid label target cond effs = Action
  { actionId        = aid
  , actionLabel     = label
  , actionTarget    = Just target
  , actionCondition = cond
  , actionEffects   = effs
  }

-- ---------------------------------------------------------------------------
-- Static initialization
-- ---------------------------------------------------------------------------

-- | Wrap an Effect as a LiveEffect for static scenario initialization.
-- Uses birthTick=0 and a deterministic UUID derived from Show.
-- Only use at scenario setup (initial world); at runtime use toLiveEffect.
staticLive :: Effect -> LiveEffect
staticLive e = LiveEffect
  { liveId        = initToken e
  , liveEffect     = e
  , liveBirthClock = LamportClock 0 (PlayerId "init")
  }

-- ---------------------------------------------------------------------------
-- Diff helpers
-- ---------------------------------------------------------------------------

-- | Convenience effect to set the world tension level (0–10).
-- Deduplicates via the engine's Tension tag family.
setTension :: Int -> Effect
setTension n = immediate (AddWorldTag (tensionTag n))

-- | Return effects only when a world tag was added this tick; otherwise [].
-- Replaces the verbose: if tag `notElem` diffWorldTagsAdded diff then [] else ...
whenTagAdded :: Tag -> WorldDiff -> [Effect] -> [Effect]
whenTagAdded t diff effs
  | t `elem` diffWorldTagsAdded diff = effs
  | otherwise                        = []

-- | Locations a character newly arrived at this tick.  No-op moves
-- (from == to) are dropped: a "resettle" should not surface arrival
-- beats.  Use inside an axiom's 'axiomEvaluate' body to drive
-- arrival-keyed effects.
playerArrivals :: CharId -> WorldDiff -> [Location]
playerArrivals cid diff =
  [ locationDeltaTo ld
  | ld <- diffLocations diff
  , locationDeltaChar ld == cid
  , locationDeltaFrom ld /= locationDeltaTo ld
  ]

-- | Erase the frequency phantom for use in uniform action lists.
anyAction :: Action f -> AnyAction
anyAction = AnyAction
