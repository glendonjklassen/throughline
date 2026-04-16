module Scenarios.DeerHunt (deerHunt, deerHuntDisplay) where

import           Engine.Core.Conditions (checkCondition)
import           SDL.Layout
import           SDL.Text
import           GameTypes
import           Scenarios.DeerHunt.Actions   (allActions)
import           Scenarios.DeerHunt.Axioms    (allAxioms, dawnRule,
                                               hunterArrivalMergeAxiom)
import           Scenarios.DeerHunt.Constants
import           Scenarios.DeerHunt.Locations  (truckNorth)

deerHunt :: Int -> CharId -> Scenario
deerHunt seed you = Scenario
  { scenarioName         = "Deer Hunt"
  , scenarioInitial      = initialWorld seed you truckNorth
  , scenarioActions      = allActions you
  , scenarioAxioms       = allAxioms you
  , scenarioMergeAxioms  = [hunterArrivalMergeAxiom you]
  , scenarioRules        = [dawnRule you]
  , scenarioMergeRules   = []
  , scenarioTerminal     = Any [HasWorldTag deerKilled, HasWorldTag hunterShot, HasWorldTag deerGone]
  , scenarioDebugDefault = Off
  , scenarioPlayerCharId = you
  }

deerHuntDisplay :: ScenarioDisplay
deerHuntDisplay = ScenarioDisplay
  { sdEndScreen  = endScreen
  , sdStatusLine = const Nothing
  , sdLayout     = defaultLayout
  }

endScreen :: GameWorld -> [String]
endScreen w
  | checkCondition w (HasWorldTag deerKilled) =
      [ ""
      , bold "  Clean kill."
      , ""
      , grey "  The buck went down fast. You walk up to it in the stubble"
      , grey "  and stand there for a minute before doing anything."
      , grey "  Steam rising off the body in the cold air."
      , ""
      , dim  "  Meat in the freezer. That's a good fall."
      , ""
      ]
  | checkCondition w (HasWorldTag hunterShot) =
      [ ""
      , bold "  You shot a man."
      , ""
      , grey "  You hear him before you see what happened. Then you're running."
      , grey "  The rifle is still in your hands. You don't remember dropping the bolt."
      , ""
      , dim  "  The rest of it — the phone call, the ambulance, the questions —"
      , dim  "  doesn't feel like something that's happening to you."
      , ""
      ]
  | checkCondition w (HasWorldTag deerGone) =
      [ ""
      , bold "  Missed."
      , ""
      , grey "  The crack echoes off the ridge and then it's quiet."
      , grey "  You cycle the bolt but there's nothing to shoot at."
      , grey "  The buck is off the section and gone."
      , ""
      , dim  "  You walk back to the truck in the last of the light."
      , dim  "  The thermos is still warm."
      , ""
      ]
  | otherwise =
      [ ""
      , grey "  The hunt is over."
      , ""
      ]
