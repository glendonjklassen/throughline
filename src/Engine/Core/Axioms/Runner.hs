module Engine.Core.Axioms.Runner
  ( runAxiomsTraced
  , runRulesTraced
  , runAxioms
  ) where

import           Data.List           (sortBy)
import           Data.Ord            (comparing)

import           Engine.Core.Axioms.System (systemAxioms)
import           Engine.Core.AxiomRule     (evaluateRule)
import           GameTypes

-- ---------------------------------------------------------------------------
-- Axiom runner
-- ---------------------------------------------------------------------------

-- | Evaluate all axioms, preserving which axiom produced which effects.
-- Used by learning mode to attribute effects to their source axiom.
runAxiomsTraced :: [Axiom] -> GameWorld -> [AnyAction] -> WorldDiff -> [AxiomTrace]
runAxiomsTraced axioms world actions diff =
  [ AxiomTrace (axiomId a) (axiomPriority a) (axiomEvaluate a world actions diff)
  | a <- sortBy (comparing axiomPriority) (systemAxioms <> axioms)
  ]

-- | Evaluate declarative axiom rules, preserving which rule produced which effects.
runRulesTraced :: [AxiomRule] -> GameWorld -> [AnyAction] -> WorldDiff -> [AxiomTrace]
runRulesTraced rules world actions diff =
  [ AxiomTrace (ruleId r) (rulePriority r) effs
  | r <- sortBy (comparing rulePriority) rules
  , let effs = evaluateRule world actions diff r
  , not (null effs)
  ]

-- | Evaluate all axioms against the same post-action snapshot.
-- Axioms run in priority order but all see the same world and diff --
-- no axiom's output can influence another's evaluation within the same tick.
runAxioms :: [Axiom] -> GameWorld -> [AnyAction] -> WorldDiff -> [Effect]
runAxioms axioms world actions diff =
  concatMap traceEffects (runAxiomsTraced axioms world actions diff)
