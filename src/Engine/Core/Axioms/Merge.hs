module Engine.Core.Axioms.Merge
  ( divergentRelationAxiom
  , strangerArrivalAxiom
  , foreignStateAxiom
  , systemMergeAxioms
  ) where

import           Engine.Author.DSL
import           GameTypes

-- ---------------------------------------------------------------------------
-- System merge axioms
-- ---------------------------------------------------------------------------

systemMergeAxioms :: [MergeAxiom]
systemMergeAxioms =
  [ divergentRelationAxiom
  , strangerArrivalAxiom
  , foreignStateAxiom
  ]

-- | When a relationship changed from a timeline that didn't know about ours.
divergentRelationAxiom :: MergeAxiom
divergentRelationAxiom = MergeAxiom
  { mergeAxiomId       = SystemAxiom "divergentRelation"
  , mergeAxiomPriority = 5
  , mergeAxiomEvaluate = \_world md ->
      [ immediate (Narrate "Something feels doubled \x2014 like two conversations wrote over each other.")
      | any ((== Unaware) . mdProvenance) (mergeRelations md) ]
  }

-- | When a character arrived at our location from a timeline that didn't know we were here.
strangerArrivalAxiom :: MergeAxiom
strangerArrivalAxiom = MergeAxiom
  { mergeAxiomId       = SystemAxiom "strangerArrival"
  , mergeAxiomPriority = 3
  , mergeAxiomEvaluate = \_world md ->
      [ immediate (Narrate "Someone is here who wasn\x2019t before. You don\x2019t remember them arriving.")
      | any ((== Unaware) . mdProvenance) (mergeLocations md) ]
  }

-- | When a world tag appeared from a timeline that didn't know about ours.
foreignStateAxiom :: MergeAxiom
foreignStateAxiom = MergeAxiom
  { mergeAxiomId       = SystemAxiom "foreignState"
  , mergeAxiomPriority = 4
  , mergeAxiomEvaluate = \_world md ->
      [ immediate (Narrate "The world shifted. Something changed that you didn\x2019t cause.")
      | any ((== Unaware) . mdProvenance) (mergeWorldTags md) ]
  }
