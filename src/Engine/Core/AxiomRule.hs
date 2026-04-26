-- | Declarative axiom rule evaluation: data-driven axioms that don't require functions.
module Engine.Core.AxiomRule
  ( evaluateRule
  , evaluateMergeRule
  , substituteSelf
  ) where

import qualified Data.Map.Strict as Map
import           Data.List       (nub)

import           Engine.Core.Conditions (checkCondition)
import           GameTypes

-- ---------------------------------------------------------------------------
-- Rule evaluation
-- ---------------------------------------------------------------------------

-- | Evaluate a single declarative axiom rule against the current world state and diff.
evaluateRule :: GameWorld -> [AnyAction] -> WorldDiff -> AxiomRule -> [Effect]
evaluateRule world _actions diff rule
  | not (triggerFires (ruleTrigger rule) diff) = []
  | otherwise =
      let targets = resolveTarget (ruleTarget rule) world diff
      in concatMap (evalForTarget world rule) targets

-- | Check whether a trigger condition is met by the current diff.
triggerFires :: Trigger -> WorldDiff -> Bool
triggerFires EveryTick _ = True
triggerFires WhenLocationChanged diff = not (null (diffLocations diff))
triggerFires (WhenTagAdded tag) diff =
  tag `elem` map snd (diffTagsAdded diff) || tag `elem` diffWorldTagsAdded diff
triggerFires (WhenWorldTagAdded tag) diff =
  tag `elem` diffWorldTagsAdded diff
triggerFires (WhenStatChanged stat) diff =
  any (\sd -> statDeltaStat sd == stat) (diffStats diff)
triggerFires (WhenRelationChanged stat) diff =
  any (\rd -> relationDeltaStat rd == stat) (diffRelations diff)

-- | Resolve a target specification to a list of CharIds to iterate over.
resolveTarget :: Target -> GameWorld -> WorldDiff -> [CharacterId]
resolveTarget EachCharacter world _ =
  [ cid | cid <- Map.keys (worldCharacters world), cid /= Truth ]
resolveTarget (SpecificChar cid) _ _ = [cid]
resolveTarget ChangedChars _ diff =
  nub $ map statDeltaChar (diffStats diff)
      ++ concatMap (\rd -> [relationDeltaFrom rd, relationDeltaTo rd]) (diffRelations diff)
      ++ map fst (diffTagsAdded diff)
      ++ map fst (diffTagsRemoved diff)
      ++ map locationDeltaChar (diffLocations diff)
resolveTarget (CoLocatedWith cid) world _ =
  case Map.lookup cid (worldLocations world) of
    Nothing  -> []
    Just loc -> [ c | (c, l) <- Map.toList (worldLocations world), l == loc, c /= cid, c /= Truth ]
resolveTarget (CharsAtLocation loc) world _ =
  [ c | (c, l) <- Map.toList (worldLocations world), l == loc, c /= Truth ]

-- | Evaluate a rule for a single target character: substitute self, check guard, return effects.
evalForTarget :: GameWorld -> AxiomRule -> CharacterId -> [Effect]
evalForTarget world rule cid =
  let guard' = substituteSelfCondition cid (ruleGuard rule)
  in if checkCondition world guard'
     then map (substituteSelf cid) (ruleEffects rule)
     else []

-- ---------------------------------------------------------------------------
-- Self substitution
-- ---------------------------------------------------------------------------

-- | Replace the @self@ sentinel CharacterId with a concrete CharacterId in an Effect.
substituteSelf :: CharacterId -> Effect -> Effect
substituteSelf cid e = e { effectBody = substituteSelfBody cid (effectBody e)
                         , effectCondition = substituteSelfCondition cid (effectCondition e) }

substituteSelfBody :: CharacterId -> EffectBody -> EffectBody
substituteSelfBody cid (AddTag c t)             = AddTag (sub cid c) t
substituteSelfBody cid (RemoveTag c t)          = RemoveTag (sub cid c) t
substituteSelfBody _   (AddWorldTag t)          = AddWorldTag t
substituteSelfBody _   (RemoveWorldTag t)       = RemoveWorldTag t
substituteSelfBody cid (ModifyRelation f t s d) = ModifyRelation (sub cid f) (sub cid t) s d
substituteSelfBody cid (Say s ls txt)           = Say (sub cid s) (map (sub cid) ls) txt
substituteSelfBody cid (Think c txt)            = Think (sub cid c) txt
substituteSelfBody _   (Narrate txt)            = Narrate txt
substituteSelfBody _   (NarratePool s vs)       = NarratePool s vs
substituteSelfBody cid (SetLocation c l)        = SetLocation (sub cid c) l
substituteSelfBody cid (OnExpire inner child)   = OnExpire (substituteSelfBody cid inner) (substituteSelf cid child)
substituteSelfBody cid (CycleMany n bs)         = CycleMany n (fmap (substituteSelfBody cid) bs)
substituteSelfBody cid (Cycle n b1 b2)          = Cycle n (substituteSelfBody cid b1) (substituteSelfBody cid b2)
substituteSelfBody cid (Dialogue dls)           = Dialogue (fmap (\(s,ls,txt) -> (sub cid s, map (sub cid) ls, txt)) dls)
substituteSelfBody cid (SetLocationRandom c s ls)        = SetLocationRandom (sub cid c) s ls
substituteSelfBody cid (SetLocationAdjacent c s)         = SetLocationAdjacent (sub cid c) s
substituteSelfBody cid (SetLocationAdjacentPrefer c s r) = SetLocationAdjacentPrefer (sub cid c) s r
substituteSelfBody _   (JournalEntry txt)       = JournalEntry txt
substituteSelfBody _   AdvanceDay               = AdvanceDay
substituteSelfBody _   DoNothing                = DoNothing

substituteSelfCondition :: CharacterId -> Condition -> Condition
substituteSelfCondition cid (HasTag c t)            = HasTag (sub cid c) t
substituteSelfCondition _   (HasWorldTag t)         = HasWorldTag t
substituteSelfCondition cid (RelationAbove f t s n) = RelationAbove (sub cid f) (sub cid t) s n
substituteSelfCondition cid (AtLocation c l)        = AtLocation (sub cid c) l
substituteSelfCondition cid (CoLocated a b)         = CoLocated (sub cid a) (sub cid b)
substituteSelfCondition cid (InRegion c r)          = InRegion (sub cid c) r
substituteSelfCondition cid (InSameRegion a b)      = InSameRegion (sub cid a) (sub cid b)
substituteSelfCondition _   (Chance s p)            = Chance s p
substituteSelfCondition cid (HasCoLocated c ex)     = HasCoLocated (sub cid c) (map (sub cid) ex)
substituteSelfCondition cid (Not c)                 = Not (substituteSelfCondition cid c)
substituteSelfCondition cid (All cs)                = All (map (substituteSelfCondition cid) cs)
substituteSelfCondition cid (Any cs)                = Any (map (substituteSelfCondition cid) cs)

sub :: CharacterId -> CharacterId -> CharacterId
sub cid c = if c == self then cid else c

-- ---------------------------------------------------------------------------
-- Merge rule evaluation
-- ---------------------------------------------------------------------------

-- | Evaluate a declarative merge rule against a merge diff.
evaluateMergeRule :: GameWorld -> MergeDiff -> MergeAxiomRule -> [Effect]
evaluateMergeRule world diff rule
  | not (mergeTriggerFires (mergeRuleTrigger rule) diff) = []
  | not (provenanceMatches (mergeRuleProvenance rule) diff) = []
  | not (checkCondition world (mergeRuleGuard rule)) = []
  | otherwise = mergeRuleEffects rule

mergeTriggerFires :: MergeTrigger -> MergeDiff -> Bool
mergeTriggerFires OnAnyMerge _ = True
mergeTriggerFires WhenMergeRelationChanged diff = not (null (mergeRelations diff))
mergeTriggerFires WhenMergeLocationChanged diff = not (null (mergeLocations diff))
mergeTriggerFires WhenMergeTagChanged diff = not (null (mergeTags diff))
mergeTriggerFires WhenMergeWorldTagChanged diff = not (null (mergeWorldTags diff))

provenanceMatches :: Maybe Provenance -> MergeDiff -> Bool
provenanceMatches Nothing _ = True
provenanceMatches (Just prov) diff = prov `elem` allProvs
  where
    allProvs = map mdProvenance (mergeStats diff)
            ++ map mdProvenance (mergeRelations diff)
            ++ map mdProvenance (mergeTags diff)
            ++ map mdProvenance (mergeWorldTags diff)
            ++ map mdProvenance (mergeLocations diff)
