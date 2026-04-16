{-# LANGUAGE DataKinds #-}
module Scenarios.Diner.Scenes.Counter (counterActions) where

import           Data.List.NonEmpty      (NonEmpty(..))
import           Engine.Author.DSL
import           GameTypes
import           Scenarios.Diner.Constants

counterActions :: CharId -> [AnyAction]
counterActions you =
  [ anyAction (waitCounter you)
  , anyAction (talkToMaya you)
  , anyAction (askMayaAboutHerNight you)
  , anyAction (stayWithMaya you)
  , anyAction (noticeFrame you)
  , anyAction (sitNearFrank you)
  , anyAction (askFrankName you)
  , anyAction (askWhyHeComes you)
  , anyAction (listenToFrank you)
  , anyAction (senseTheRoom you)
  ]

waitCounter :: CharId -> Action 'Repeatable
waitCounter _you = repeatableAction (ActionId "waitCounter")
  "Lean on the counter and wait."
  unconditional
  [immediate DoNothing]

talkToMaya :: CharId -> Action 'Once
talkToMaya you = onceAction (ActionId "talkToMaya")
  "Make conversation with Maya."
  (HasWorldTag orderedCoffee)
  $ conversation you maya
      ( (you,  "Quiet night?") :|
      [ (maya, "Always is, this time. The rush was around midnight — couple of guys from the bar down the street.")
      , (you,  "You do this shift a lot?")
      , (maya, "Four nights a week. You get used to it. The quiet's the best part, honestly.")
      ])
  ++ bidirectionalTrust maya you 2 1

askMayaAboutHerNight :: CharId -> Action 'Once
askMayaAboutHerNight you = onceAction (ActionId "askMayaAboutHerNight")
  "Ask Maya how she's really doing."
  (All [trustAbove maya you 1, HasWorldTag orderedCoffee])
  (perceptiveEffects ++ nonPerceptiveEffects)
  where
    perceptiveCond = statAbove you (Capacity Understanding) 5
    perceptiveEffects =
      map (gateConversationEffect perceptiveCond)
        (conversation you maya
            ( (you,  "You seem a little off tonight. Everything okay?") :|
            [ (maya, "...")
            , (maya, "My kid's been sick. Nothing serious, but — you know. You worry.")
            , (you,  "Yeah. That's hard.")
            , (maya, "It is. Thanks for asking, actually.")
            ]))
      ++ map (gateEffect perceptiveCond) (addTags [mayaOpened, lateNightConfession])
      ++ [ immediateWhen perceptiveCond (ModifyRelation maya you Trust 3)
         , immediateWhen perceptiveCond (ModifyRelation you maya Trust 2)
         ]
    nonPerceptiveEffects =
      map (gateConversationEffect (Not perceptiveCond))
        (conversation you maya
            ( (you,  "Long night?") :|
            [ (maya, "Aren't they all.")
            ]))
      ++ [immediateWhen (Not perceptiveCond) (ModifyRelation maya you Trust 1)]

noticeFrame :: CharId -> Action 'Once
noticeFrame you = onceAction (ActionId "noticeFrame")
  "Notice the photo behind the counter."
  (statAbove you (Capacity Understanding) 4)
  [ immediate (Narrate "There's a small photo taped to the side of the register. A kid — maybe four or five — grinning at the camera. Someone drew a star on the corner in marker.")
  , immediate (think you "That's hers.")
  ]

sitNearFrank :: CharId -> Action 'Once
sitNearFrank you = onceAction (ActionId "sitNearFrank")
  "Take the stool next to the older man."
  unconditional
  [ immediate (Narrate "You take the stool one over from him. He doesn't look up, but his posture shifts slightly — aware.")
  , immediate (ModifyRelation frank you (Perceived Understanding) 1)
  ]

askFrankName :: CharId -> Action 'Once
askFrankName you = onceAction (ActionId "askFrankName")
  "Introduce yourself."
  (HasWorldTag (actionTaken (ActionId "sitNearFrank")))
  $ conversation you frank
      ( (you,   "Hey. I'm — well, it doesn't matter. I'm just here.") :|
      [ (frank, "Frank.")
      , (you,   "You come here a lot?")
      , (frank, "Most nights.")
      ])
  ++ mutualTrust frank you 1

askWhyHeComes :: CharId -> Action 'Once
askWhyHeComes you = onceAction (ActionId "askWhyHeComes")
  "Ask Frank what brings him here at this hour."
  (All [ trustAbove frank you 0
       , HasWorldTag (actionTaken (ActionId "askFrankName"))])
  (perceptiveEffects ++ nonPerceptiveEffects)
  where
    perceptiveCond = statAbove you (Capacity Understanding) 5
    perceptiveEffects =
      map (gateConversationEffect perceptiveCond)
        (conversation you frank
            ( (you,   "The apartment get quiet?") :|
            [ (frank, "...")
            , (frank, "Yeah. It does.")
            , (frank, "My wife passed two years ago. The place still smells like her soap sometimes.")
            ]))
      ++ map (gateEffect perceptiveCond) (addTags [frankOpened, lateNightConfession])
      ++ [ immediateWhen perceptiveCond (ModifyRelation frank you Trust 3)
         , immediateWhen perceptiveCond (ModifyRelation you frank Trust 2)
         ]
    nonPerceptiveEffects =
      map (gateConversationEffect (Not perceptiveCond))
        (conversation you frank
            ( (you,   "Can't sleep either, huh?") :|
            [ (frank, "Something like that.")
            ]))
      ++ [immediateWhen (Not perceptiveCond) (ModifyRelation frank you Trust 1)]

stayWithMaya :: CharId -> Action 'Once
stayWithMaya you = onceAction (ActionId "stayWithMaya")
  "Stay at the counter while Maya wipes down."
  (All [HasWorldTag mayaOpened, trustAbove maya you 4]) $
  [ immediate (Narrate "Maya does her closing count, mouthing numbers. You don't interrupt. The coffee machine sighs and clicks off. She tops up your mug without asking.")
  , immediate (think you "This is okay. This is actually okay.")
  ]
  ++ mutualTrust maya you 2
  ++ [ modifyTrust you maya 1 ]
  ++ removeTags [restless]
  ++ addTags [settled, quietPresence]

listenToFrank :: CharId -> Action 'Once
listenToFrank you = onceAction (ActionId "listenToFrank")
  "Just sit with Frank for a while."
  (All [HasWorldTag frankOpened, trustAbove frank you 2]) $
  [ immediate (Narrate "You don't say anything. Neither does he. Maya refills both cups without being asked. The rain eases outside.")
  , immediate (think you "Sometimes company is enough.")
  ]
  ++ mutualTrust frank you 2
  ++ [ modifyTrust you frank 1 ]
  ++ removeTags [restless]
  ++ addTags [settled, quietPresence]

senseTheRoom :: CharId -> Action 'Once
senseTheRoom you = onceAction (ActionId "senseTheRoom")
  "Take in the room — really take it in."
  (HasWorldTag orderedCoffee)
  (bothEffects ++ eitherEffects ++ neitherEffects)
  where
    bothCond    = All [HasWorldTag smallKindness, HasWorldTag worryInTheWalls]
    eitherCond  = All [Any [HasWorldTag smallKindness, HasWorldTag worryInTheWalls], Not bothCond]
    neitherCond = All [Not (HasWorldTag smallKindness), Not (HasWorldTag worryInTheWalls)]
    bothEffects =
      [ immediateWhen bothCond (Narrate "The diner holds more than you expected. There's a gentleness in how the server works the counter, and an ache underneath it — something she's carrying. The room absorbed both.")
      , immediateWhen bothCond (Think you "You came here because you couldn't sit still with your ceiling. But this place isn't empty either. It's full of things people brought with them and left behind.")
      ]
    eitherEffects =
      [ immediateWhen eitherCond (Narrate "Something about the room feels different than when you walked in. The light hasn't changed, the coffee's the same — but there's a texture to the quiet that wasn't there before.")
      , immediateWhen eitherCond (Think you "Like the walls absorbed something. Someone's worry, or someone's care. You can't tell which.")
      ]
    neitherEffects =
      [ immediateWhen neitherCond (Narrate "The diner is what it is. Fluorescent light, coffee that's been sitting too long, a country song from the speakers. Nothing special.")
      , immediateWhen neitherCond (Think you "Just a stopping point between here and morning.")
      ]

-- ---------------------------------------------------------------------------
-- Helpers for condition-gated conversation effects
-- ---------------------------------------------------------------------------

-- | Gate an effect on a condition. For effects with lifetime=Just 1 and
-- unconditional, replaces the condition. For effects with existing
-- conditions or complex lifetimes, wraps in an additional condition.
gateEffect :: Condition -> Effect -> Effect
gateEffect cond e = e { effectCondition = All [cond, effectCondition e] }

-- | Gate a conversation effect. Conversation effects from the DSL may have
-- complex structure (OnExpire chains, DialogueInProgress management).
-- We gate each effect by ANDing the condition.
gateConversationEffect :: Condition -> Effect -> Effect
gateConversationEffect = gateEffect
