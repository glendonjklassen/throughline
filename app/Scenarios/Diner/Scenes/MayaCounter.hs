{-# LANGUAGE DataKinds #-}
module Scenarios.Diner.Scenes.MayaCounter (mayaCounterActions) where

import           Data.List.NonEmpty      (NonEmpty(..))
import           Engine.Author.DSL
import           GameTypes
import           Scenarios.Diner.Constants

mayaCounterActions :: CharacterId -> [AnyAction]
mayaCounterActions mayaId =
  [ anyAction (wipeCounter mayaId)
  , anyAction (prepOrder mayaId)
  , anyAction (noticeNewFace mayaId)
  , anyAction (checkOnFrank mayaId)
  , anyAction (worryAboutKid mayaId)
  , anyAction (textBabysitter mayaId)
  , anyAction (takeStock mayaId)
  ]

wipeCounter :: CharacterId -> Action 'Repeatable
wipeCounter _mayaId = repeatableAction (ActionId "maya:wipeCounter")
  "Wipe down the counter."
  unconditional
  [immediate DoNothing]

prepOrder :: CharacterId -> Action 'Once
prepOrder mayaId = onceAction (ActionId "maya:prepOrder")
  "Pour a coffee for the new customer."
  (HasWorldTag orderedCoffee)
  [ immediate (Narrate "You pull the pot off the burner and pour. The coffee's been sitting since midnight — it's not great. But it's hot.")
  , immediate (think mayaId "At least someone's drinking it.")
  ]

noticeNewFace :: CharacterId -> Action 'Once
noticeNewFace mayaId = onceAction (ActionId "maya:noticeNewFace")
  "Study the person who just came in."
  (AtLocation visitor counter)
  [ immediate (Narrate "They're at the counter now. Young, tired-looking. The kind of tired that isn't about sleep.")
  , immediate (think mayaId "Something's keeping them up. I know that look.")
  , immediate (AddWorldTag noticedVisitor)
  , immediate (ModifyRelation maya visitor (Perceived Understanding) 1)
  ]

checkOnFrank :: CharacterId -> Action 'Once
checkOnFrank mayaId = onceAction (ActionId "maya:checkOnFrank")
  "Top off Frank's cup."
  unconditional
  $ conversation mayaId frank
      ( (mayaId, "How's it going tonight, Frank?") :|
      [ (frank,  "Same as always.")
      , (mayaId, "That good, huh.")
      ])
  ++ addTags [frankChatted, smallKindness]
  ++ [modifyTrust frank maya 1]

worryAboutKid :: CharacterId -> Action 'Once
worryAboutKid mayaId = onceAction (ActionId "maya:worryAboutKid")
  "Check the time and think about Jamie."
  (HasWorldTag (timeTag 3))
  [ immediate (think mayaId "Jamie had that fever again when I left. The babysitter said it was fine. She always says it's fine.")
  , immediate (Narrate "You glance at the clock. 3 AM. Too late to call.")
  , modifyStat mayaId (Capacity Strength) (-1)
  , immediate (AddWorldTag worryInTheWalls)
  ]

textBabysitter :: CharacterId -> Action 'Once
textBabysitter mayaId = onceAction (ActionId "maya:textBabysitter")
  "Send a quick text about Jamie."
  (HasWorldTag (timeTag 4))
  [ immediate (Narrate "You pull out your phone under the counter. Type, delete, retype. Send.")
  , immediate (think mayaId "\"How's Jamie?\" Two words. That's all you can manage at 4 AM.")
  , immediate (AddWorldTag checkedOnKid)
  ]

takeStock :: CharacterId -> Action 'Once
takeStock mayaId = onceAction (ActionId "maya:takeStock")
  "Take stock of the shift."
  (HasWorldTag orderedCoffee)
  (bothEffects ++ eitherEffects ++ neitherEffects)
  where
    bothCond    = All [HasWorldTag lateNightConfession, HasWorldTag quietPresence]
    eitherCond  = All [Any [HasWorldTag lateNightConfession, HasWorldTag quietPresence], Not bothCond]
    neitherCond = All [Not (HasWorldTag lateNightConfession), Not (HasWorldTag quietPresence)]
    bothEffects =
      [ immediateWhen bothCond (Narrate "The night had weight. Someone came in and was quiet — not the checked-out quiet of the usual late crowd, but a quietness that was paying attention. And then they opened up.")
      , immediateWhen bothCond (Think mayaId "You don't get that on a Wednesday at 3 AM. Not usually.")
      ]
    eitherEffects =
      [ immediateWhen eitherCond (Narrate "Something shifted tonight. You can't point to when, exactly — but the diner doesn't feel like it usually does at this hour.")
      , immediateWhen eitherCond (Think mayaId "Someone was actually here tonight. Not just sitting, but here.")
      ]
    neitherEffects =
      [ immediateWhen neitherCond (Narrate "You lean against the register and take a breath. Another night. The coffee needs replacing, the condiments need wiping, the floor needs mopping.")
      , immediateWhen neitherCond (Think mayaId "Four nights a week. You get used to it.")
      ]
