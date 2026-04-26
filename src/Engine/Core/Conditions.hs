-- | Pure condition evaluation: checks predicates against world state.
module Engine.Core.Conditions where

import qualified Data.Map.Strict as Map
import           System.Random   (mkStdGen, randomR)

import           Engine.Author.Random (scenarioSeed)
import           Engine.CRDT.ORSet
import           GameTypes

checkCondition :: GameWorld -> Condition -> Bool
checkCondition g (Any cs)               = foldr (\x acc -> checkCondition g x || acc) False cs
checkCondition g (All cs)               = foldr (\x acc -> checkCondition g x && acc) True cs
checkCondition g (Not c)                = not (checkCondition g c)
checkCondition g (HasTag cid tag)       =
  case Map.lookup cid (worldCharacters g) of
    Just c  -> orMember tag (charTags c)
    Nothing -> False
checkCondition g (HasWorldTag tag)      = orMember tag (worldTags g)
checkCondition g (RelationAbove from to stat t) =
  case Map.lookup from (worldGraph g) >>= Map.lookup to of
    Just rel -> getRelStat stat rel > t
    Nothing  -> False
checkCondition g (AtLocation cid loc) =
  Map.lookup cid (worldLocations g) == Just loc
checkCondition g (CoLocated a b) =
  case (Map.lookup a (worldLocations g), Map.lookup b (worldLocations g)) of
    (Just la, Just lb) -> la == lb
    _                  -> False
checkCondition g (InRegion cid region) =
  case Map.lookup cid (worldLocations g) of
    Just loc -> Map.lookup loc (lgRegions (worldLocationGraph g)) == Just region
    Nothing  -> False
checkCondition g (InSameRegion a b) =
  case (Map.lookup a (worldLocations g), Map.lookup b (worldLocations g)) of
    (Just la, Just lb) ->
      let regions = lgRegions (worldLocationGraph g)
      in case (Map.lookup la regions, Map.lookup lb regions) of
           (Just ra, Just rb) -> ra == rb
           _                  -> False
    _ -> False
checkCondition g (Chance salt p) =
  let seed = scenarioSeed (lcTick (worldClock g)) salt
      (roll, _) = randomR (0.0 :: Double, 1.0) (mkStdGen seed)
  in roll < p
checkCondition g (HasCoLocated cid excludes) =
  case Map.lookup cid (worldLocations g) of
    Nothing  -> False
    Just loc -> any (\(c, l) -> l == loc && c /= cid && c /= Truth && c `notElem` excludes)
                    (Map.toList (worldLocations g))

-- | Read a character's ground truth stat from the truth edge.
-- NOTE: returns Just 0 for any stat on a character that has at least one
-- truth edge entry, even if the specific stat was never set. Use
-- 'hasCharacterStat' to distinguish "explicitly set to 0" from "not set".
getCharacterStat :: CharacterId -> StatType -> GameWorld -> Maybe Int
getCharacterStat cid stat world = do
  truthEdges <- Map.lookup Truth (worldGraph world)
  rel        <- Map.lookup cid truthEdges
  pure (getRelStat stat rel)

-- | Check whether a stat was explicitly set on a character's truth edge.
-- Returns False when the stat key is absent from the relationship map,
-- even if 'getCharacterStat' would return Just 0.
hasCharacterStat :: CharacterId -> StatType -> GameWorld -> Bool
hasCharacterStat cid stat world =
  case Map.lookup Truth (worldGraph world) >>= Map.lookup cid of
    Just (Relationship m) -> Map.member stat m
    Nothing               -> False
