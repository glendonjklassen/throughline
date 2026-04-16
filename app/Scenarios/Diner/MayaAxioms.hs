module Scenarios.Diner.MayaAxioms (allAxiomsMaya, allRulesMaya) where

import           Engine.Author.CommonAxioms (weatherNarrationAxiom,
                                             weatherInfluenceAxiom, moodDriftAxiom)
import           GameTypes
import           Scenarios.Diner.Axioms     (dawnRule, weatherDesc, weatherEffect)
import           Scenarios.Diner.Constants

allAxiomsMaya :: CharId -> [Axiom]
allAxiomsMaya mayaId =
  [ weatherNarrationAxiom weatherDesc
  , weatherInfluenceAxiom mayaId weatherEffect
  , moodDriftAxiom        mayaId [(Capacity Charisma, 7)]
  ]

allRulesMaya :: [AxiomRule]
allRulesMaya = [dawnRule mayaDawn]
