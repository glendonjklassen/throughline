-- | One-shot ambient rumors: a deterministic line from a pool that
-- surfaces at most once per run on a player arrival.  Useful for
-- "things the protagonist has heard" — folklore, gossip, half-remembered
-- stories — that should land in the world without promising the
-- player any particular event.
module Engine.Author.Rumor
  ( Rumor (..)
  , pickRumor
  , rumorAxiom
  ) where

import           Engine.Author.DSL
import           Engine.Core.Conditions  (checkCondition)
import           GameTypes

-- | One thing the character has "heard".  'rumorText' is the
-- in-the-moment line; 'rumorJournal' is the diary shorthand.  Both
-- voiced by the rendering callback supplied to 'rumorAxiom'.
data Rumor = Rumor
  { rumorText    :: !String
  , rumorJournal :: !String
  } deriving (Show, Eq)

-- | Deterministically pick a rumor by seed.  Modulus over the pool
-- size — collisions across seeds are fine, distinct seeds usually
-- pick distinct rumors.
pickRumor :: Int -> [Rumor] -> Rumor
pickRumor _    []   = error "pickRumor: empty pool"
pickRumor seed pool = pool !! (abs seed `mod` length pool)

-- | An axiom that surfaces a rumor at most once per run on a player
-- arrival.  The 'Tag' is the "delivered" guard — once added, the
-- axiom is silent for the rest of the run.  The voice callback is
-- applied to 'rumorText' for the in-the-moment beat (e.g. @Think you@
-- for an internal voice, @Narrate@ for an external one).
rumorAxiom
  :: AxiomId
  -> CharId                      -- ^ player character whose arrivals trigger
  -> Tag                         -- ^ delivered-guard tag
  -> [Rumor]                     -- ^ scenario's pool
  -> Int                         -- ^ deterministic seed
  -> Double                      -- ^ per-arrival probability
  -> (String -> EffectBody)      -- ^ voice (e.g. @Think you@, @Narrate@)
  -> Axiom
rumorAxiom aid you delivered pool seed chance voice = Axiom
  { axiomId       = aid
  , axiomPriority = 4
  , axiomEvaluate = \world _avail diff ->
      concatMap (drop1 world) (playerArrivals you diff)
  }
  where
    frag = pickRumor seed pool
    guard = Not (HasWorldTag delivered)

    drop1 world loc
      | checkCondition world (HasWorldTag delivered) = []
      | checkCondition world (Chance (locSalt loc) chance) =
          [ immediateWhen guard (voice (rumorText frag))
          , immediateWhen guard (JournalEntry (rumorJournal frag))
          , immediateWhen guard (AddWorldTag delivered)
          ]
      | otherwise = []

    locSalt (Location s) = foldl (\acc c -> acc * 131 + fromEnum c) 11 s
