module Scenarios.TopBuy.Axioms
  ( allAxioms
  , smallAskRule
  , kyleAuditRule
  , earlyReportRule
  , weatherDesc
  , weatherEffect
  ) where

import           Data.Maybe                 (fromMaybe)
import           Engine.Author.CommonAxioms        (weatherNarrationAxiom,
                                             weatherInfluenceAxiom, moodDriftAxiom)
import           Engine.Core.Conditions          (checkCondition, getCharacterStat)
import           Engine.Author.DSL
import           GameTypes
import           Scenarios.TopBuy.Constants
import           Scenarios.TopBuy.Locations

-- | When Bradley first forms an opinion of you (Understanding set from 0),
-- narrate it. Perceptive players get the full read; others notice something is different.
perceptionAxiom :: CharacterId -> Axiom
perceptionAxiom you = Axiom
  { axiomId       = ScenarioAxiom "perception"
  , axiomPriority = 5
  , axiomEvaluate = \world _actions diff ->
      let bradleyAssessed = any (\d -> relationDeltaFrom d == bradley
                                    && relationDeltaTo   d == you
                                    && relationDeltaStat d == Perceived Understanding
                                    && relationDeltaOld  d == 0)
                                (diffRelations diff)
      in if not bradleyAssessed then [] else
           if checkCondition world (statAbove you (Capacity Understanding) 4)
             then [ immediateNarrated
                      "Something is off about Bradley — distracted, a little jumpy. You can't name it exactly, but something's changed."
                      (AddWorldTag playerSuspecting)
                  ]
             else [ immediate (Narrate "Bradley has been less chatty lately. You don't think much of it.") ]
  }

-- | When Bradley witnesses you handle a customer situation or read him directly,
-- he revises his assessment of you upward.
bradleyWatchingAxiom :: CharacterId -> Axiom
bradleyWatchingAxiom you = Axiom
  { axiomId       = ScenarioAxiom "bradleyWatching"
  , axiomPriority = 5
  , axiomEvaluate = \world _actions diff ->
      let watched = [ActionId "askIfHesOkay", ActionId "reassureAngryCustomer", ActionId "explainProduct"]
          noticed = any (\aid -> actionTaken aid `elem` diffWorldTagsAdded diff) watched
          tookAsk = actionTaken (ActionId "askIfHesOkay") `elem` diffWorldTagsAdded diff
      in if not noticed || not (checkCondition world (RelationAbove bradley you (Perceived Understanding) 0)) then [] else
           [ immediate (ModifyRelation bradley you (Perceived Understanding) 1)
           , if tookAsk
               then immediate (Narrate "Bradley looks at you for a beat longer than necessary.")
               else immediate (Narrate "Bradley glances over from across the floor. He looks less settled than he did.")
           ]
  }

-- | When the player covers the floor, Bradley steals. Start Kyle's clock.
accompliceAxiom :: CharacterId -> Axiom
accompliceAxiom you = Axiom
  { axiomId       = ScenarioAxiom "accomplice"
  , axiomPriority = 5
  , axiomEvaluate = \world _actions diff ->
      effectsIfTagAdded coveredForBradley diff $
        let hint =
              [ immediateNarrated
                  "He doesn't seem relieved to be done with the count. He seems relieved to be done with something else."
                  (Think you "That wasn't an inventory run.")
              | checkCondition world (statAbove you (Capacity Understanding) 6) ]
        in [ immediateNarrated
               "Bradley comes back twenty minutes later. He thanks you casually, makes a joke about the stockroom being a disaster."
               (AddWorldTag bradleySucceeded)
           , timed 2 (OnExpire (AddWorldTag kyleInvestigating) (immediate DoNothing))
           ] ++ hint
  }

-- | When waitAction triggers bradleyAsking, Bradley delivers the request.
-- Sets bradleySmallAsk so response actions unlock on the next turn.
smallAskRule :: AxiomRule
smallAskRule = AxiomRule
  { ruleId       = ScenarioAxiom "smallAsk"
  , rulePriority = 5
  , ruleTrigger  = WhenWorldTagAdded bradleyAsking
  , ruleGuard    = unconditional
  , ruleTarget   = SpecificChar Truth
  , ruleEffects  = [ immediate (sayToRoom bradley "Hey, my hands are full. Can you log that return at register three? Should be in the system.")
                   , immediate (AddWorldTag bradleySmallAsk)
                   ]
  }

-- | Kyle eventually does his own audit. Fires when the inventory discrepancy
-- is found, regardless of whether the player reports it. Slower than the
-- early-report path (6 ticks vs 3), so proactive reporting still matters.
kyleAuditRule :: AxiomRule
kyleAuditRule = AxiomRule
  { ruleId       = ScenarioAxiom "kyleAudit"
  , rulePriority = 5
  , ruleTrigger  = WhenWorldTagAdded inventoryDiscrepancy
  , ruleGuard    = unconditional
  , ruleTarget   = SpecificChar Truth
  , ruleEffects  = [ timed 6 (OnExpire (AddWorldTag kyleInvestigating) (immediate DoNothing)) ]
  }

-- | If the player reported the discrepancy, Kyle was already watching.
earlyReportRule :: AxiomRule
earlyReportRule = AxiomRule
  { ruleId       = ScenarioAxiom "earlyReport"
  , rulePriority = 5
  , ruleTrigger  = WhenWorldTagAdded reportedToKyle
  , ruleGuard    = unconditional
  , ruleTarget   = SpecificChar Truth
  , ruleEffects  = [ immediate (Narrate "Kyle listens carefully. He doesn't react much, but you notice him write something down.")
                   , timed 3 (OnExpire (AddWorldTag kyleInvestigating) (immediate DoNothing))
                   ]
  }

-- | Manages the work shift: narrates end of shift at 5 PM and start at 9 AM,
-- restores Strength and clears tiredness tags for a new day.
shiftAxiom :: CharacterId -> Axiom
shiftAxiom you = Axiom
  { axiomId       = ScenarioAxiom "shift"
  , axiomPriority = 2
  , axiomEvaluate = \world _actions diff ->
      let endOfShift   = timeTag 17 `elem` diffWorldTagsAdded diff
          startOfShift = timeTag 9  `elem` diffWorldTagsAdded diff
                      && not (checkCondition world (AtLocation you salesFloor))
          strDelta     = 5 - fromMaybe 0 (getCharacterStat you (Capacity Strength) world)
          hunDelta     = 8 - fromMaybe 0 (getCharacterStat you (Capacity Hunger)   world)
      in if endOfShift then
           [ immediate (Narrate "Your shift ends. You clock out and head for the door.")
           , immediate (SetLocation you home)
           , immediate (AddTag you sleepingTag)
           , immediate (RemoveWorldTag phoneOut)
           , immediate (RemoveWorldTag scrollingPhone)
           ]
         else if startOfShift then
           [ immediate (Narrate "Morning. You badge in and take your spot on the floor.")
           , immediate (SetLocation you salesFloor)
           , immediate (RemoveTag you sleepingTag)
           , modifyStat you (Capacity Strength) strDelta
           , modifyStat you (Capacity Hunger)   hunDelta
           , immediate (RemoveTag you (fatigueTag Tired))
           , immediate (RemoveTag you (fatigueTag Exhausted))
           , immediate (RemoveTag you (hungerStateTag Peckish))
           , immediate (RemoveTag you (hungerStateTag Hungry))
           ]
         else []
  }

-- | Centrally models the physical cost of player actions.
-- All effort drains live here rather than scattered across action definitions.
effortAxiom :: CharacterId -> Axiom
effortAxiom you = Axiom
  { axiomId       = ScenarioAxiom "effort"
  , axiomPriority = 2
  , axiomEvaluate = \_world _actions diff ->
      let took aid = actionTaken aid `elem` diffWorldTagsAdded diff
          drain n  = [modifyStat you (Capacity Strength) (-n)]
          costs    = [ (ActionId "helpCustomer",        1)
                     , (ActionId "checkStockroom",      2)
                     , (ActionId "logReturnForBradley", 1)
                     , (ActionId "coverForBradley",     1)
                     ]
      in concat [ drain n | (aid, n) <- costs, took aid ]
  }

weatherDesc :: WeatherDesc -> String
weatherDesc (WeatherDesc "Clear")         = "You glance at the storefront. Clear skies, hard light."
weatherDesc (WeatherDesc "Partly Cloudy") = "The light through the entrance has gone soft. Clouds moving in."
weatherDesc (WeatherDesc "Overcast")      = "Outside has gone ansiGrey. The parking lot looks flat and muted."
weatherDesc (WeatherDesc "Light Rain")    = "Rain taps the glass doors at the entrance. A few customers shake off their coats."
weatherDesc (WeatherDesc "Windy")         = "Someone comes in and the automatic doors don't close fast enough. Cold air hits the floor."
weatherDesc (WeatherDesc "Stormy")        = "You can hear the wind from here. Rain against the windows. The automatic doors keep triggering."
weatherDesc w                             = "The weather outside has shifted. " <> weatherName w <> "."

-- | How weather affects you during a shift at a big-box store.
-- Bad weather makes the floor feel heavier; clear skies lift your mood.
weatherEffect :: WeatherDesc -> [(StatType, Int)]
weatherEffect (WeatherDesc "Stormy")    = [(Capacity Charisma, -1)]
weatherEffect (WeatherDesc "Light Rain")= [(Capacity Charisma, -1)]
weatherEffect _                         = []

-- | Reads world tags and sets the tension level to match the highest-matching
-- story milestone.  Checks from highest to lowest so the first match wins.
-- Only emits a setTension effect when the level actually changes.
tensionAxiom :: Axiom
tensionAxiom = Axiom
  { axiomId       = ScenarioAxiom "tension"
  , axiomPriority = 10          -- low priority: runs after gameplay axioms
  , axiomEvaluate = \world _actions _diff ->
      let has     = hasTag world
          current = getTension world
          target
            | has reportedToKyle                              = 9
            | has kyleInvestigating                           = 8
            | has coveredForBradley || has loggedReturnForBradley = 7
            | has bradleyBigAsk                               = 6
            | has playerSuspecting                            = 5
            | has inventoryDiscrepancy                        = 4
            | has bradleySmallAsk                             = 3
            | has bradleyAsking                               = 2
            | otherwise                                       = 0
      in [setTension target | target /= current]
  }

allAxioms :: CharacterId -> [Axiom]
allAxioms you =
  [ shiftAxiom         you
  , effortAxiom        you
  , weatherNarrationAxiom weatherDesc
  , weatherInfluenceAxiom you weatherEffect
  , moodDriftAxiom     you [(Capacity Charisma, 5)]
  , perceptionAxiom    you
  , bradleyWatchingAxiom you
  , accompliceAxiom    you
  , tensionAxiom
  ]
