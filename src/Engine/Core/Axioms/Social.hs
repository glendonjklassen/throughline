module Engine.Core.Axioms.Social
  ( socialEnergyAxiom
  , socialEnergyStateAxiom
  , perceptionDriftAxiom
  , coLocated
  , trustedCompanionThreshold
  , charHasSocialEnergy
  ) where

import           Data.Maybe          (fromMaybe)
import qualified Data.Map.Strict     as Map

import           Engine.Author.DSL
import           Engine.Core.Conditions (getCharacterStat, hasCharacterStat)
import           Engine.Core.Axioms.Shared (charIsSleeping)
import           Engine.CRDT.ORSet
import           GameTypes

-- ---------------------------------------------------------------------------
-- Shared constants -- used across social axioms
-- ---------------------------------------------------------------------------

trustedCompanionThreshold :: Int
trustedCompanionThreshold = 5

-- ---------------------------------------------------------------------------
-- Character tag queries (social)
-- ---------------------------------------------------------------------------

charHasSocialEnergy :: CharacterId -> SocialEnergyLevel -> GameWorld -> Bool
charHasSocialEnergy cid level world =
  case Map.lookup cid (worldCharacters world) of
    Just c  -> orMember (socialEnergyTag level) (charTags c)
    Nothing -> False

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Find all characters at the same location as the given character.
coLocated :: CharacterId -> GameWorld -> [CharacterId]
coLocated cid world =
  case Map.lookup cid (worldLocations world) of
    Nothing  -> []
    Just loc -> [ other
                | (other, otherLoc) <- Map.toList (worldLocations world)
                , other /= cid
                , otherLoc == loc
                ]

-- ---------------------------------------------------------------------------
-- Social axioms -- social energy and perception
-- ---------------------------------------------------------------------------

-- | Drains or restores SocialStamina each hour based on Extraversion and company.
-- Extraverts gain energy from company and lose it alone.
-- Introverts gain energy alone and lose it in untrusted company.
socialEnergyAxiom :: Axiom
socialEnergyAxiom = Axiom
  { axiomId       = SystemAxiom "socialEnergy"
  , axiomPriority = 3
  , axiomEvaluate = \world _actions diff ->
      let hourTicked = any isTimeTag (diffWorldTagsAdded diff)
          chars      = Map.keys (worldCharacters world)
      in if not hourTicked then [] else
           concatMap (socialEnergyFor world) chars
  }
  where
    socialEnergyFor world cid
      | not (hasCharacterStat cid (Capacity SocialStamina) world) = []
      | charIsSleeping cid world = []
      | otherwise =
          -- Without a personality system, social energy drains in untrusted
          -- company and restores when alone — a neutral default.
          let others = coLocated cid world
              alone  = null others
          in if alone
               then [modifyStat cid (Capacity SocialStamina) 1]
               else [modifyStat cid (Capacity SocialStamina) (-1)
                    | not (any (trustedCompanion cid world) others)]

    trustedCompanion cid world other =
      case Map.lookup cid (worldGraph world) >>= Map.lookup other of
        Just rel -> getRelStat Trust rel > trustedCompanionThreshold
        Nothing  -> False

-- | Sets the SocialEnergy EngineTag on each character based on SocialStamina thresholds.
-- SocialStamina <= 2 -> Drained. SocialStamina <= 4 -> Neutral. Above 4 -> Energized.
socialEnergyStateAxiom :: Axiom
socialEnergyStateAxiom = Axiom
  { axiomId       = SystemAxiom "socialEnergyState"
  , axiomPriority = 4
  , axiomEvaluate = \world _actions diff ->
      let changed = [ statDeltaChar d
                    | d <- diffStats diff
                    , statDeltaStat d == Capacity SocialStamina
                    ]
      in concatMap (socialEnergyStateFor world) changed
  }
  where
    drainedThreshold = 2
    neutralThreshold = 4

    socialEnergyStateFor world cid =
      case getCharacterStat cid (Capacity SocialStamina) world of
        Nothing -> []
        Just s
          | s <= drainedThreshold -> setSocialEnergy cid Drained world
          | s <= neutralThreshold -> setSocialEnergy cid Neutral world
          | otherwise             -> setSocialEnergy cid Energized world

    setSocialEnergy cid level world =
      [immediate (AddTag cid (socialEnergyTag level)) | not (charHasSocialEnergy cid level world)]

-- | When a location change causes two characters to become co-located, emits
-- an ifItPersists chain: if they remain co-located for perceptionDriftTicks,
-- each Perceived stat the perceiver already holds drifts 1 toward truth.
-- The chain self-cancels if either character leaves.
-- Only drifts stats where the perceiver already has a nonzero Perceived value.
perceptionDriftAxiom :: Axiom
perceptionDriftAxiom = Axiom
  { axiomId       = SystemAxiom "perceptionDrift"
  , axiomPriority = 7
  , axiomEvaluate = \world _actions diff ->
      let movedChars = map locationDeltaChar (diffLocations diff)
          allChars   = Map.keys (worldCharacters world)
      in concatMap (driftEffectsFor world allChars) movedChars
  }
  where
    perceptionDriftTicks = 12  -- ~12 hours co-located before drift fires

    driftEffectsFor world _allChars mover =
      let others = coLocated mover world
      in -- mover perceives others; others perceive mover
         concatMap (mkDriftChain world mover) others
         ++ concatMap (\o -> mkDriftChain world o mover) others

    mkDriftChain world perceiver target =
      case Map.lookup perceiver (worldLocations world) of
        Nothing -> []
        Just l  ->
          let coLocCond = All [AtLocation perceiver l, AtLocation target l]
              drifts    = concatMap (driftEffect perceiver target world) [minBound..maxBound]
          in [ifItPersists perceptionDriftTicks coLocCond e | e <- drifts]

    driftEffect perceiver target world stat =
      let truth     = fromMaybe 0 (getCharacterStat target (Capacity stat) world)
          perceived = perceivedStat perceiver target stat world
      in if perceived == 0 || perceived == truth then [] else
           let delta = signum (truth - perceived)
           in [immediate (ModifyRelation perceiver target (Perceived stat) delta)]

    perceivedStat perceiver target stat world =
      case Map.lookup perceiver (worldGraph world) >>= Map.lookup target of
        Just rel -> getRelStat (Perceived stat) rel
        Nothing  -> 0
