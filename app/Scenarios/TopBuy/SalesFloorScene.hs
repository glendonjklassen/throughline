{-# LANGUAGE DataKinds #-}
module Scenarios.TopBuy.SalesFloorScene where

import           Data.List.NonEmpty              (NonEmpty(..))
import           Engine.Author.DSL
import           GameTypes
import           Scenarios.TopBuy.Constants
import           Scenarios.TopBuy.Locations

salesFloorActions :: CharId -> [AnyAction]
salesFloorActions you =
  [ anyAction (waitAction you)
  , anyAction (greetBradley you)
  , anyAction (checkPhone you)
  , anyAction putAwayPhone
  , anyAction (smallTalk you)
  , anyAction (helpCustomer you)
  , anyAction (observeStore you)
  , anyAction (complimentWork you)
  , anyAction (checkStockroom you)
  , anyAction (reassureAngryCustomer you)
  , anyAction (explainProduct you)
  , anyAction (askIfHesOkay you)
  , anyAction (logReturnForBradley you)
  , anyAction (refuseReturn you)
  , anyAction (coverForBradley you)
  , anyAction (refuseBigAsk you)
  , anyAction (reportDiscrepancy you)
  , anyAction (talkToKyle you)
  ]

-- ---------------------------------------------------------------------------
-- Phone — available any time on shift; drains Understanding after 5 ticks
-- ---------------------------------------------------------------------------

checkPhone :: CharId -> Action 'Repeatable
checkPhone you = repeatableAction (ActionId "checkPhone")
  "Check your phone."
  (Not (HasWorldTag phoneOut))
  $ addTags [phoneOut, scrollingPhone] ++
    [ ifItPersists 5 (HasWorldTag scrollingPhone)
        (immediateNarrated "You glance up. Something about the floor feels out of focus."
          (ModifyRelation Truth you (Capacity Understanding) (-1)))
    ]

putAwayPhone :: Action 'Repeatable
putAwayPhone = repeatableAction (ActionId "putAwayPhone")
  "Put your phone away."
  (HasWorldTag phoneOut)
  (removeTags [phoneOut, scrollingPhone])

-- ---------------------------------------------------------------------------
-- Early game
-- ---------------------------------------------------------------------------

waitAction :: CharId -> Action 'Repeatable
waitAction you = repeatableAction (ActionId "wait")
  "Stay busy on the floor."
  unconditional
  [ immediateWhen (All [ RelationAbove bradley you (Perceived Understanding) 0
                       , Not (RelationAbove bradley you (Perceived Understanding) 3)
                       , Not (HasWorldTag bradleyAsking)
                       , Not (HasWorldTag bradleySmallAsk)])
      (AddWorldTag bradleyAsking)
  ]

-- Test action for dialogueChain + continueAction flow.
greetBradley :: CharId -> Action 'Once
greetBradley you = onceAction (ActionId "greetBradley") "Say hi to Bradley." unconditional
  $ conversation you bradley
      ( (you,     "Hey Bradley, how's it going?") :|
      [ (bradley, "Not bad. Just got here.")
      , (you,     "Same. Should be a quiet one.")
      , (bradley, "Famous last words.")
      ])

-- Talking to Bradley is what starts his scheme — he decides you're his mark.
smallTalk :: CharId -> Action 'Once
smallTalk you = onceAction (ActionId "smallTalk") "Make small talk with Bradley." unconditional $
  mutualTrust you bradley 4 ++
  [ immediate (ModifyRelation bradley you (Perceived Understanding) 2)
  , conversationThen you bradley
      ( (you,     "Quiet day, huh?") :|
      [ (bradley, "Tell me about it. I've rearranged this display three times.")
      ])
      (immediate DoNothing)
  ]

-- Builds Understanding +1. Available only before things get tense.
helpCustomer :: CharId -> Action 'Once
helpCustomer you = onceAction (ActionId "helpCustomer")
  "Help a customer find what they're looking for."
  (Not (RelationAbove bradley you (Perceived Understanding) 0))
  [ immediate (Narrate "An older man is squinting at the laptop section. You spend ten minutes with him. He leaves happy.")
  , immediate (think you "I'm actually decent at this.")
  , modifyCharacterStatEffect you (Capacity Understanding) 1
  ]

-- Builds Understanding +1. Plants an early seed about Bradley's behaviour.
observeStore :: CharId -> Action 'Once
observeStore you = onceAction (ActionId "observeStore")
  "Take stock of the floor — who's where, what's moving."
  (Not (RelationAbove bradley you (Perceived Understanding) 0))
  [ immediate (Narrate "You do a slow scan. Bradley's made three trips to the stockroom since his break. Kyle hasn't been out front all shift.")
  , modifyCharacterStatEffect you (Capacity Understanding) 1
  ]

complimentWork :: CharId -> Action 'Once
complimentWork you = onceAction (ActionId "complimentWork")
  "Tell Bradley he runs a tight ship back here."
  (All [Not (RelationAbove bradley you (Perceived Understanding) 0), trustAbove you bradley 3]) $
  bidirectionalTrust you bradley 1 2 ++
  [ conversationThen you bradley
      ( (you,     "This section has never looked better. That's on you.") :|
      [ (bradley, "Ha. The secret is caring too much about things that don't matter.")
      ])
      (immediate DoNothing)
  ]

-- ---------------------------------------------------------------------------
-- Mid game — Bradley is acting off
-- ---------------------------------------------------------------------------

checkStockroom :: CharId -> Action 'Once
checkStockroom you = onceAction (ActionId "checkStockroom")
  "Offer to help with the stockroom count."
  (All [RelationAbove bradley you (Perceived Understanding) 0, Not (HasWorldTag inventoryDiscrepancy)])
  [ immediate (Narrate "You pull up the inventory sheet. The numbers are slightly off — a few SKUs unaccounted for. Could be a logging error.")
  , immediate (AddWorldTag inventoryDiscrepancy)
  , immediateWhen (statAbove you (Capacity Understanding) 5)
      (think you "Those gaps aren't random. They're all high-margin items.")
  ]

askIfHesOkay :: CharId -> Action 'Once
askIfHesOkay you = onceAction (ActionId "askIfHesOkay")
  "Ask Bradley if everything's alright."
  (All [RelationAbove bradley you (Perceived Understanding) 0, trustAbove you bradley 3])
  (suspectingEffects ++ normalEffects)
  where
    suspectingCond = HasWorldTag playerSuspecting
    suspectingEffects =
      [ immediateWhen suspectingCond
          (Narrate "You approach Bradley.")
      , conversationThenWhen suspectingCond you bradley
          ((you, "Hey — you seem off today. Everything okay?") :| [])
          (timed 1 (OnExpire (Say bradley [you] "I'm fine. Why?")
            (delayed (think you "That came out defensive."))))
      ]
    normalEffects =
      [ conversationThenWhen (Not suspectingCond) you bradley
          ((you, "Hey — you seem off today. Everything okay?") :| [])
          (delayed (sayTo bradley you "Yeah, I'm good. Just tired. Thanks for asking."))
      ]

-- ---------------------------------------------------------------------------
-- Co-location interactions
-- ---------------------------------------------------------------------------

greetCustomer :: CharId -> CharId -> Action 'Once
greetCustomer you other = onceAction (ActionId "greetCustomer")
  "Greet the customer."
  (All [AtLocation you salesFloor, AtLocation other salesFloor])
  [ immediate (sayTo you other "Hey there — need any help today?") ]

-- Shows Bradley you know how to read people under pressure.
reassureAngryCustomer :: CharId -> Action 'Once
reassureAngryCustomer you = onceAction (ActionId "reassureAngryCustomer")
  "Step in with an agitated customer at the service desk."
  (RelationAbove bradley you (Perceived Understanding) 0)
  [ immediate (Narrate "A man is at the counter, voice up, arms out. You get there before it escalates — acknowledge the problem, don't argue it. He leaves unhappy but quiet.")
  , immediate (think you "People just want to feel heard. Even when they're wrong.")
  , modifyCharacterStatEffect you (Capacity Understanding) 1
  ]

-- Shows Bradley you understand people well enough to guide them, not just serve them.
explainProduct :: CharId -> Action 'Once
explainProduct you = onceAction (ActionId "explainProduct")
  "Help a couple who can't decide on a TV."
  (RelationAbove bradley you (Perceived Understanding) 0)
  [ immediate (Narrate "They have a budget and a list of specs from a Reddit thread. You spend fifteen minutes with them — not selling, just translating. They leave with something they can afford and actually understand.")
  , immediate (think you "That felt good, actually.")
  , modifyCharacterStatEffect you (Capacity Understanding) 1
  ]

-- ---------------------------------------------------------------------------
-- The small ask — Bradley builds the paper trail
-- ---------------------------------------------------------------------------

logReturnForBradley :: CharId -> Action 'Once
logReturnForBradley you = onceAction (ActionId "logReturnForBradley")
  "Log the return at register three."
  (HasWorldTag bradleySmallAsk)
  (suspectingEffects ++ normalEffects)
  where
    suspectingCond = HasWorldTag playerSuspecting
    bigAskLine = conversationThen bradley you
      ((bradley, "Hey — while I have you, I need to do a count in the back. Could you watch the floor for twenty?") :| [])
      (immediate (AddWorldTag bradleyBigAsk))
    suspectingEffects =
      [ immediate (AddWorldTag loggedReturnForBradley)
      , conversationThenWhen suspectingCond you bradley ((you, "Sure, one sec.") :| [])
          (timed 1 (OnExpire (Narrate "You punch it in under your employee login. Takes thirty seconds.")
            (timed 1 (OnExpire (think you "I probably shouldn't be doing this. But it's just a return.")
              bigAskLine))))
      ]
    normalEffects =
      [ conversationThenWhen (Not suspectingCond) you bradley ((you, "Sure, one sec.") :| [])
          (timed 1 (OnExpire (Narrate "You punch it in under your employee login. Takes thirty seconds.")
            bigAskLine))
      ]

refuseReturn :: CharId -> Action 'Once
refuseReturn you = onceAction (ActionId "refuseReturn")
  "Tell Bradley to log it himself."
  (All [HasWorldTag bradleySmallAsk, HasWorldTag playerSuspecting])
  [ conversationThen you bradley
      ( (you,     "Sorry, I shouldn't be on someone else's login. Can you do it when you're free?") :|
      [ (bradley, "Yeah. Yeah, okay.")
      ])
      (delayed (Narrate "He doesn't push it. But something shifts in his expression."))
  ]

-- ---------------------------------------------------------------------------
-- The big ask
-- ---------------------------------------------------------------------------

coverForBradley :: CharId -> Action 'Once
coverForBradley you = onceAction (ActionId "coverForBradley")
  "Cover the floor while Bradley does inventory."
  (HasWorldTag bradleyBigAsk)
  (suspectingEffects ++ normalEffects)
  where
    suspectingCond = HasWorldTag playerSuspecting
    suspectingEffects =
      [ immediate (AddWorldTag coveredForBradley)
      , conversationThenWhen suspectingCond you bradley ((you, "Yeah, of course.") :| [])
          (delayed (think you "I should probably say something. But what, exactly?"))
      ]
    normalEffects =
      [ immediateWhen (Not suspectingCond) (AddWorldTag coveredForBradley)
      , conversationThenWhen (Not suspectingCond) you bradley ((you, "Yeah, of course.") :| [])
          (immediate DoNothing)
      ]

refuseBigAsk :: CharId -> Action 'Once
refuseBigAsk you = onceAction (ActionId "refuseBigAsk")
  "Tell Bradley you'd rather not."
  (All [HasWorldTag bradleyBigAsk, HasWorldTag playerSuspecting])
  [ modifyTrust bradley you (-3)
  , conversationThen you bradley
      ( (you,     "I'd rather not, actually.") :|
      [ (bradley, "Okay. Weird. I'll ask someone else.")
      ])
      (timed 1 (OnExpire (Narrate "He doesn't look annoyed. He looks careful.")
        (delayed (think you "He knows I noticed something."))))
  ]

-- ---------------------------------------------------------------------------
-- Proactive moves
-- ---------------------------------------------------------------------------

reportDiscrepancy :: CharId -> Action 'Once
reportDiscrepancy you = onceAction (ActionId "reportDiscrepancy")
  "Report the inventory discrepancy to your manager."
  (All [HasWorldTag inventoryDiscrepancy, Not (HasWorldTag reportedToKyle)])
  [ immediate (AddWorldTag reportedToKyle)
  , conversationThen you kyle ((you, "Kyle, do you have a second? I was doing a count and the numbers look off.") :| [])
      (immediate DoNothing)
  ]

-- ---------------------------------------------------------------------------
-- The confrontation — path is determined by condition-gated effects
-- ---------------------------------------------------------------------------

talkToKyle :: CharId -> Action 'Once
talkToKyle you = onceAction (ActionId "talkToKyle")
  "Go talk to Kyle."
  (HasWorldTag kyleInvestigating)
  (pathAEffects ++ pathCEffects ++ pathBEffects)
  where
    isReported   = HasWorldTag reportedToKyle
    isImplicated = Any [HasWorldTag loggedReturnForBradley, HasWorldTag coveredForBradley]

    pathACond = isReported
    pathCCond = All [Not isReported, isImplicated]
    pathBCond = All [Not isReported, Not isImplicated]

    opener =
      (kyle, "Hey. Can I grab you for a minute? It's about some inventory numbers.") :| []

    pathAEffects =
      [ conversationThenWhen pathACond you kyle opener
          (conversationThenWhen pathACond you kyle
            ( (kyle, "Actually, I'm glad you came to me when you did. We've been watching Bradley for a few days.") :|
              [ (you,  "I had a feeling something was wrong.")
              , (kyle, "We have him on camera. You're not in any trouble.")
              ])
            (timed 1 (OnExpire (Narrate "You leave his office with your job intact. Bradley doesn't come in the next day.")
              (immediate (AddWorldTag playerCleared)))))
      ]

    pathCEffects =
      [ conversationThenWhen pathCCond you kyle opener
          (conversationThenWhen pathCCond you kyle
            ( (kyle, "There's a return logged under your ID that we can't account for.") :|
              [ (you,  "Bradley asked me to. I didn't know what it was for.")
              , (kyle, "I believe you. But it's not a great position to be in.")
              ])
            (timed 1 (OnExpire (Narrate "He puts you on leave pending review. You sit in your car for a long time.")
              (immediate (AddWorldTag playerSuspended)))))
      ]

    pathBEffects =
      [ conversationThenWhen pathBCond you kyle opener
          (conversationThenWhen pathBCond you kyle
            ( (kyle, "We have a discrepancy in the stockroom. Did you notice anything unusual lately?") :|
              [ (you,  "Bradley's been acting strange. I didn't want to say anything without being sure.")
              , (kyle, "That's helpful. We're looking into it.")
              ])
            (timed 1 (OnExpire (Narrate "Kyle nods slowly. This is clearly not news to him.")
              (immediate (AddWorldTag playerCleared)))))
      ]

-- ---------------------------------------------------------------------------
-- Helpers for condition-gated conversation effects
-- ---------------------------------------------------------------------------

-- | Like conversationThen but gated on a condition. The conversation chain
-- and the continuation effect are both only active when the condition holds.
conversationThenWhen :: Condition -> CharId -> CharId -> NonEmpty (CharId, String) -> Effect -> Effect
conversationThenWhen cond speaker listener lines' cont =
  timedWhen 1 cond (OnExpire (Dialogue (fmap (\(s, t) -> (s, [listener | s == speaker], t)) lines'))
    (cont { effectCondition = All [cond, effectCondition cont] }))
