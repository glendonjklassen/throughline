-- | Intra/cross transition narration tables.  When a scenario
-- classifies its locations into terrain (or zone, or biome, or
-- whatever) categories, movement between two locations is either
-- intra-class (same category, possibly different sub-position) or
-- cross-class (boundary crossing).  Each axis has its own pool of
-- prose variants.  This module provides the table type and the
-- lookup helpers; scenarios pick their classifier and seed the pools.
module Engine.Author.Transition
  ( TransitionPool (..)
  , intraVariants
  , crossVariants
  , transitionNarration
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Engine.Author.Scene (poolNarration)
import           GameTypes

-- | Narration pool for movement between classified locations.
-- @cls@ is the scenario's classification (e.g. terrain class).
-- @hint@ is a sub-position discriminator used only for intra-class
-- moves (e.g. interior / edge / bridge).
data TransitionPool cls hint = TransitionPool
  { tpIntra    :: Map (cls, hint) [String]
  , tpCross    :: Map (cls, cls)  [String]
  , tpFallback :: [String]
  }

-- | Variants for a same-class arrival.  Returns the table's intra
-- pool for @(cls, hint)@ if non-empty, otherwise 'tpFallback'.
intraVariants :: (Ord cls, Ord hint) => TransitionPool cls hint -> cls -> hint -> [String]
intraVariants t cls hint =
  case Map.lookup (cls, hint) (tpIntra t) of
    Just xs@(_:_) -> xs
    _             -> tpFallback t

-- | Variants for a cross-class arrival.  Returns the table's cross
-- pool for @(from, to)@ if non-empty, otherwise 'tpFallback'.
crossVariants :: Ord cls => TransitionPool cls hint -> cls -> cls -> [String]
crossVariants t from to =
  case Map.lookup (from, to) (tpCross t) of
    Just xs@(_:_) -> xs
    _             -> tpFallback t

-- | Build a 'Narration' for an edge from a transition table plus
-- per-location classifiers.  Intra moves consult 'tpIntra' keyed by
-- the destination's class/hint; cross moves consult 'tpCross' keyed
-- by the (from, to) class pair.  Salted by the location pair so
-- adjacent edges produce independent PRNG sequences.
transitionNarration
  :: (Ord cls, Ord hint)
  => TransitionPool cls hint
  -> (Location -> cls)
  -> (Location -> hint)
  -> Location -> Location
  -> Narration
transitionNarration table classify hintOf =
  poolNarration $ \from to ->
    let clsFrom = classify from
        clsTo   = classify to
    in if clsFrom == clsTo
         then intraVariants table clsTo (hintOf to)
         else crossVariants table clsFrom clsTo
