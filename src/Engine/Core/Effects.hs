-- | Effect execution: applies effect bodies to game world state within the App monad.
module Engine.Core.Effects where

import           Control.Applicative    ((<|>))
import           Control.Monad.Reader   (asks)
import           Control.Monad.State
import           Data.IORef             (writeIORef)
import           Data.List              (nubBy, sortBy)
import           Data.List.NonEmpty      (NonEmpty(..))
import qualified Data.Set               as Set
import qualified Data.List.NonEmpty     as NE
import qualified Data.Map.Strict        as Map
import qualified Data.UUID.V4           as UUID.V4

import           Engine.Author.Narrative
import           Engine.Author.Random   (scenarioSeed)
import           Engine.Core.Axioms
import           Engine.Core.Conditions
import           Engine.Core.World
import           Engine.CRDT.ORSet
import           Engine.CRDT.PNCounter
import           Engine.Core.NarrativeMessage
import           GameTypes
import           MonadStack

executeAction :: GameWorld -> Action f -> [LiveEffect] -> App [LiveEffect]
executeAction w a activeFx = do
  newFx <- traverse (toLiveEffect (worldClock w)) (actionEffects a)
  let allowedNew    = filter (checkCondition w . effectCondition . liveEffect) newFx
  let allowedActive = filter (checkCondition w . effectCondition . liveEffect) activeFx
  resultsNew    <- mapM executeEffect allowedNew
  resultsActive <- mapM executeEffect allowedActive
  pure (concat resultsNew ++ concat resultsActive)

executeEffect :: LiveEffect -> App [LiveEffect]
executeEffect le = do
  w <- get
  let e    = liveEffect le
  let body = currentBody (effectBody e)
  executeBody body
  renderNarrative w body (effectNarrative e)
  tickLive le

-- | Run an effect's body once without persistence logic. Used for axiom effects,
-- which are immediate by convention and do not enter worldActiveEffects.
executeEffectOnce :: Effect -> App ()
executeEffectOnce e = do
  w <- get
  let body = currentBody (effectBody e)
  executeBody body
  renderNarrative w body (effectNarrative e)

-- | Extract the body that fires this tick from a potentially compound EffectBody.
currentBody :: EffectBody -> EffectBody
currentBody (OnExpire inner _)  = inner
currentBody (CycleMany _ (e :| _)) = e
currentBody (Cycle _ e1 _)      = e1
currentBody other               = other

renderNarrative :: GameWorld -> EffectBody -> Maybe String -> App ()
renderNarrative w body override = do
  you <- asks envPlayerCharId
  case override <|> narrateEffect you w body of
    Just prose -> narrate (MsgEffect prose)
    Nothing    -> pure ()

executeBody :: EffectBody -> App ()
executeBody (Say cid listeners b) = do
  w <- get
  let speakerName   = maybe (show cid) charName (Map.lookup cid (worldCharacters w))
      listenerNames = map (\lid -> maybe (show lid) charName (Map.lookup lid (worldCharacters w))) listeners
  narrate (MsgSay cid speakerName listeners listenerNames b)
executeBody (Think cid b)               = narrate (MsgThink cid b)
executeBody (Narrate b)                 = narrate (MsgNarrate b)
executeBody (NarratePool salt variants) = do
  w <- get
  case variants of
    []  -> pure ()
    _   -> do
      let seed = scenarioSeed (lcTick (worldClock w)) salt
          idx  = abs seed `mod` length variants
      narrate (MsgNarrate (variants !! idx))
executeBody (AddTag cid tag) = do
  let clean = maybe id orDeleteWhere (deduplicator tag)
  case deduplicator tag of
    Nothing -> do
      uuid <- liftIO UUID.V4.nextRandom
      modifyCharacter cid (\c -> c { charTags = orInsert uuid tag (charTags c) })
    Just _  ->
      modifyCharacter cid (\c -> c { charTags = orUpsert (initToken tag) tag (clean (charTags c)) })
executeBody (RemoveTag cid tag) =
  modifyCharacter cid (\c -> c { charTags = orDelete tag (charTags c) })
executeBody (AddWorldTag t) = do
  let clean = maybe id orDeleteWhere (deduplicator t)
  case deduplicator t of
    Nothing -> do
      uuid <- liftIO UUID.V4.nextRandom
      modify (\w -> w { worldTags = orInsert uuid t (worldTags w) })
    Just _  ->
      modify (\w -> w { worldTags = orUpsert (initToken t) t (clean (worldTags w)) })
executeBody (RemoveWorldTag t) =
  modify (\w -> w { worldTags = orDelete t (worldTags w) })
executeBody (ModifyRelation from to stat delta) = do
  pid <- asks envPlayerId
  modify (\w -> w { worldGraph = modifyRelStat pid from to stat delta (worldGraph w) })
executeBody (SetLocation cid loc)       = modify (moveCharacter cid loc)
executeBody (Dialogue dls) = do
  w <- get
  let resolve (cid, listeners, b) =
        let speakerName   = maybe (show cid) charName (Map.lookup cid (worldCharacters w))
            listenerNames = map (\lid -> maybe (show lid) charName (Map.lookup lid (worldCharacters w))) listeners
        in (cid, speakerName, listeners, listenerNames, b)
  narrate (MsgDialogue (map resolve (NE.toList dls)))
executeBody (SetLocationRandom cid salt locs) = do
  w <- get
  case locs of
    [] -> pure ()
    _  -> do
      let idx = scenarioSeed (lcTick (worldClock w)) salt `mod` length locs
      modify (moveCharacter cid (locs !! idx))
executeBody (SetLocationAdjacent cid salt) = do
  w <- get
  case Map.lookup cid (worldLocations w) of
    Nothing  -> pure ()
    Just loc -> do
      let neighbors = lgAdjacentTo loc (worldLocationGraph w)
      case neighbors of
        [] -> pure ()
        _  -> do
          let idx = scenarioSeed (lcTick (worldClock w)) salt `mod` length neighbors
          modify (moveCharacter cid (neighbors !! idx))
executeBody (SetLocationAdjacentPrefer cid salt region) = do
  w <- get
  case Map.lookup cid (worldLocations w) of
    Nothing  -> pure ()
    Just loc -> do
      let neighbors = lgAdjacentTo loc (worldLocationGraph w)
          preferred = filter (\l -> lgInRegion l (worldLocationGraph w) == Just region) neighbors
          candidates = if null preferred then neighbors else preferred
      case candidates of
        [] -> pure ()
        _  -> do
          let idx = scenarioSeed (lcTick (worldClock w)) salt `mod` length candidates
          modify (moveCharacter cid (candidates !! idx))
executeBody DoNothing                   = pure ()
executeBody (OnExpire _ _)              = error "executeBody: compound body reached; currentBody must be called first"
executeBody (CycleMany _ _)             = error "executeBody: compound body reached; currentBody must be called first"
executeBody (Cycle {})                  = error "executeBody: compound body reached; currentBody must be called first"

-- | Move a character to a new location, pushing the departing location
-- onto the bounded per-character history and incrementing the visit
-- count for the destination.  No-op moves (already at 'loc') leave
-- everything untouched so axioms that redundantly re-assert a location
-- don't pollute the trail or inflate visit counts.
moveCharacter :: CharId -> Location -> GameWorld -> GameWorld
moveCharacter cid loc w =
  let prev        = Map.lookup cid (worldLocations w)
      sameSpot    = prev == Just loc
      newLocs     = Map.insert cid loc (worldLocations w)
      newHistory  = case (prev, sameSpot) of
        (Just old, False) ->
          Map.insertWith pushBounded cid [old] (worldLocationHistory w)
        _                  -> worldLocationHistory w
      newVisits   = if sameSpot
        then worldLocationVisits w
        else Map.insertWith (Map.unionWith (+)) cid
               (Map.singleton loc 1) (worldLocationVisits w)
  in w { worldLocations       = newLocs
       , worldLocationHistory = newHistory
       , worldLocationVisits  = newVisits
       }
  where
    -- Cap the trail at 8 entries.  'insertWith' passes (new, old), so the
    -- single-element new list prepends onto whatever was there.
    pushBounded new old = take 8 (new ++ old)

-- | Return the deduplication predicate for singleton EngineTag families,
-- or Nothing for tags that allow multiple concurrent values.
deduplicator :: Tag -> Maybe (Tag -> Bool)
deduplicator (EngineTag (Weather _))      = Just isWeather
deduplicator (EngineTag (Clock _))        = Just isClockTag
deduplicator (EngineTag (Tension _))      = Just isTensionTag
deduplicator (EngineTag (Fatigue _))      = Just isFatigueTag
deduplicator (EngineTag (HungerState _))  = Just isHungerStateTag
deduplicator (EngineTag (SocialEnergy _)) = Just isSocialEnergyTag
deduplicator _                            = Nothing

-- | Execute one full action step: advance worldClock, run effects, run axioms and rules.
-- Used by both the game loop and log replay to ensure identical semantics.
executeStep :: Action f -> App ()
executeStep action = do
  worldBefore <- get
  pid         <- asks envPlayerId
  modify (\w -> w { worldClock = LamportClock (lcTick (worldClock w) + 1) pid })
  actions     <- asks envActions
  let available   = filter (checkCondition worldBefore . anyActionCondition) actions
  remainingFx <- executeAction worldBefore action (worldActiveEffects worldBefore)
  modify (\w -> w { worldActiveEffects = remainingFx })
  worldAfter <- get
  axioms      <- asks envAxioms
  rules       <- asks envRules
  traceRef    <- asks envAxiomTrace
  let theDiff      = diffWorlds pid worldBefore worldAfter
      axiomTraces  = runAxiomsTraced axioms worldAfter available theDiff
      ruleTraces   = runRulesTraced rules worldAfter available theDiff
      allTraces    = sortBy (\a b -> compare (tracePriority a) (tracePriority b)) (axiomTraces ++ ruleTraces)
  liftIO $ writeIORef traceRef allTraces
  mapM_ executeEffectOnce (concatMap traceEffects allTraces)

-- | Apply a WorldDiff to the current world state.
-- Used when receiving another player's diff via portal merge.
-- Stats and relations are attributed to the player recorded in each delta.
applyWorldDiff :: WorldDiff -> App ()
applyWorldDiff diff = do
  mapM_ applyStatDelta   (diffStats diff)
  mapM_ applyRelDelta    (diffRelations diff)
  mapM_ applyTagAdded    (diffTagsAdded diff)
  mapM_ applyTagRemoved  (diffTagsRemoved diff)
  mapM_ applyWTagAdded   (diffWorldTagsAdded diff)
  mapM_ applyWTagRemoved (diffWorldTagsRemoved diff)
  mapM_ applyLocDelta    (diffLocations diff)
  where
    applyStatDelta :: StatDelta -> App ()
    applyStatDelta sd =
      modify (\w -> w { worldGraph =
        modifyRelStat (statDeltaPlayer sd) Truth (statDeltaChar sd)
          (statDeltaStat sd) (statDeltaNew sd - statDeltaOld sd) (worldGraph w) })
    applyRelDelta :: RelationDelta -> App ()
    applyRelDelta rd =
      modify (\w -> w { worldGraph =
        modifyRelStat (relationDeltaPlayer rd) (relationDeltaFrom rd)
          (relationDeltaTo rd) (relationDeltaStat rd)
          (relationDeltaNew rd - relationDeltaOld rd) (worldGraph w) })
    applyTagAdded    (cid, tag) = executeBody (AddTag cid tag)
    applyTagRemoved  (cid, tag) = executeBody (RemoveTag cid tag)
    applyWTagAdded   tag        = executeBody (AddWorldTag tag)
    applyWTagRemoved tag        = executeBody (RemoveWorldTag tag)
    applyLocDelta    ld         = executeBody (SetLocation (locationDeltaChar ld) (locationDeltaTo ld))

-- | OR-Set merge for active effects: union by liveId, deduplicating shared entries.
-- An effect present in both sides (same UUID) is kept once.
-- NOTE: nubBy is O(n²). If active effect counts grow large (e.g. shared-universe
-- scenarios with many concurrent players), replace with a Map-based dedup.
mergeActiveEffects :: [LiveEffect] -> [LiveEffect] -> [LiveEffect]
mergeActiveEffects as bs = nubBy (\x y -> liveId x == liveId y) (as ++ bs)

-- | Merge two GameWorld snapshots using CRDT merge on each field.
-- Tags: OR-Set merge (add-wins, concurrent add+remove resolves to add).
-- Stats: PN-Counter merge (per-player buckets, high-water mark per player).
-- Active effects: OR-Set merge by liveId.
-- Clock: take the later Lamport value.
-- Locations: left-biased union. In practice, each player's CharId is
-- cryptographically unique (derived from Ed25519 keypair), so location
-- conflicts only arise for shared NPCs like the deer — where left-bias
-- is acceptable since log replay is the authoritative merge path for
-- contested state. Parallel games by the same identity are not a concern.
--
-- This is the pure snapshot merge path — no log replay required.
-- Both sides must have been tracking changes under distinct PlayerId keys
-- for stat merges to be correct (concurrent changes from the same PlayerId
-- are indistinguishable).
mergeWorlds :: GameWorld -> GameWorld -> GameWorld
mergeWorlds a b = GameWorld
  { worldCharacters      = Map.unionWith mergeChar (worldCharacters a) (worldCharacters b)
  , worldGraph           = Map.unionWith (Map.unionWith mergeRel) (worldGraph a) (worldGraph b)
  , worldLocations       = Map.union (worldLocations a) (worldLocations b)
  , worldActiveEffects   = mergeActiveEffects (worldActiveEffects a) (worldActiveEffects b)
  , worldTags            = orMerge (worldTags a) (worldTags b)
  , worldClock           = max (worldClock a) (worldClock b)
  , worldLocationGraph   = mergeLocationGraphs (worldLocationGraph a) (worldLocationGraph b)
  , worldSeed            = worldSeed a
  , worldLocationHistory = Map.union (worldLocationHistory a) (worldLocationHistory b)
    -- Left-biased: local history wins, foreign history fills gaps.  Ordering
    -- between logs is not meaningful here; downstream UI only reads prefixes.
  , worldLocationVisits  = Map.unionWith (Map.unionWith (+)) (worldLocationVisits a) (worldLocationVisits b)
    -- Visit counts sum; merges double-count if both sides saw the same arrival,
    -- but that's a known consequence of snapshot merges (see the function's
    -- haddock).  The log-replay path is authoritative for contested state.
  }
  where
    mergeChar ca cb      = ca { charTags = orMerge (charTags ca) (charTags cb) }
    mergeRel (Relationship ma) (Relationship mb) = Relationship (Map.unionWith pnMerge ma mb)

-- | Merge two location graphs: set union on edges, left-biased map union on regions.
mergeLocationGraphs :: LocationGraph -> LocationGraph -> LocationGraph
mergeLocationGraphs a b = LocationGraph
  { lgEdges   = Set.union (lgEdges a) (lgEdges b)
  , lgRegions = Map.union (lgRegions a) (lgRegions b)
  , lgCoords  = Map.union (lgCoords a) (lgCoords b)
  }

-- ---------------------------------------------------------------------------
-- LiveEffect helpers
-- ---------------------------------------------------------------------------

-- | Wrap a plain Effect as a LiveEffect becoming active at the given clock.
toLiveEffect :: LamportClock -> Effect -> App LiveEffect
toLiveEffect birthClock e = do
  uuid <- liftIO UUID.V4.nextRandom
  pure LiveEffect { liveId = uuid, liveEffect = e, liveBirthClock = birthClock }

-- | Determine what persists after an effect fires this tick.
tickLive :: LiveEffect -> App [LiveEffect]
tickLive le = do
  currentClock <- gets worldClock
  case effectLifetime (liveEffect le) of
    Nothing -> pure [le]
    Just n  ->
      let remaining = n - (lcTick currentClock - lcTick (liveBirthClock le))
      in if remaining > 0
           then pure [le]
           else expireLive le

-- | Spawn the next step as a new LiveEffect born at currentClock.
expireLive :: LiveEffect -> App [LiveEffect]
expireLive le = do
  uuid         <- liftIO UUID.V4.nextRandom
  currentClock <- gets worldClock
  let spawn next = pure [LiveEffect { liveId = uuid, liveEffect = next, liveBirthClock = currentClock }]
  let e = liveEffect le
  case effectBody e of
    OnExpire _ child          -> spawn child
    CycleMany t (x :| xs)     -> spawn e { effectBody = CycleMany t (NE.fromList (xs ++ [x])) }
    Cycle     t _ e2          -> spawn e { effectBody = Cycle t e2 (currentBody (effectBody e)) }
    _                         -> pure []
