# Getting started with throughline

This guide walks you through writing a scenario from scratch. By the end you will have a runnable narrative experience: a starting world, locations the player can walk between, a few actions, and prose that surfaces as the player moves.

It assumes you have the project building (`stack build` succeeds, `stack run throughline-exe` opens the dev launcher) and a Haskell editor on hand.

## Concepts

A **scenario** is a closed narrative experience. It declares:

- A starting world: who exists, where they are, what state they begin with.
- A set of actions the player can choose from each turn (filtered by their conditions).
- A set of axioms — background rules that fire each tick to make the world act on its own (weather changes, NPCs move, beats land).
- A terminal condition — when the scenario ends.

The player navigates through choices. Each action runs a list of effects. Effects mutate state and emit prose. The engine renders the prose, the player picks the next action, and the next tick begins.

## The minimum viable scenario

A scenario is a value of type `Scenario` (defined in `GameTypes`). The smallest one looks like this:

```haskell
module Scenarios.MyFirst (myFirst) where

import qualified Data.Map.Strict as Map
import           Engine.Author.DSL    (emptyTags)
import           Engine.Author.Scene  (compileSceneGraph)
import           GameTypes

myFirst :: Int -> CharacterId -> Scenario
myFirst seed you = Scenario
  { scenarioName         = "my-first"
  , scenarioInitial      = initialWorld seed you
  , scenarioActions      = compileSceneGraph you sceneGraph
  , scenarioAxioms       = []
  , scenarioMergeAxioms  = []
  , scenarioRules        = []
  , scenarioMergeRules   = []
  , scenarioTerminal     = Any []          -- never terminates
  , scenarioDebugDefault = Off
  , scenarioPlayerCharId = you
  , scenarioTombstoneGC  = Nothing
  }
```

`Int` is a seed and `CharacterId` is who the player is — the launcher passes both. You don't need to know what they mean yet; just thread them through.

This won't compile until you provide `initialWorld :: Int -> CharacterId -> GameWorld` and `sceneGraph :: SceneGraph`. The next two sections build them.

## Building the world

A `GameWorld` is the engine's state. The fields you typically set at init:

```haskell
initialWorld :: Int -> CharacterId -> GameWorld
initialWorld seed you = GameWorld
  { worldCharacters       = Map.fromList
      [ (you, Character you "You" [] emptyTags) ]
  , worldGraph            = Map.empty                       -- relationships
  , worldLocations        = Map.fromList [(you, kitchen)]
  , worldActiveEffects    = []
  , worldClock            = LamportClock 0 (PlayerId "init")
  , worldTags             = emptyTags                       -- world-wide tags
  , worldLocationGraph    = emptyLocationGraph
  , worldSeed             = seed
  , worldLocationHistory  = Map.empty
  , worldLocationVisits   = Map.empty
  , worldJournal          = []
  , worldDayNumber        = 1
  }
  where
    kitchen = Location "Kitchen"
```

Most fields stay `[]` / `Map.empty` / `emptyTags` until you need them. The ones that actually shape a first scenario:

- **`worldCharacters`** — every character that exists. The player is one of them; named NPCs are others.
- **`worldLocations`** — where each character is right now.
- **`worldTags`** — global state markers (`scenarioTag MyTag`, `weatherTag (WeatherDesc "Clear")`, etc.). Use `tagsFromList [t1, t2]` to seed multiple.

## Defining the scene graph

Locations are `Location "Name"`. The scene graph lists them along with the edges that connect them:

```haskell
import Engine.Author.Scene

sceneGraph :: SceneGraph
sceneGraph = SceneGraph
  { sgScenes =
      [ Scene kitchen    (const [])
      , Scene hallway    (const [])
      , Scene livingRoom (const [])
      ]
  , sgEdges = concat
      [ biEdge kitchen hallway
          "Step into the hallway."  "You walk into the hallway."
          "Back to the kitchen."    "You return to the kitchen."
      , biEdge hallway livingRoom
          "Enter the living room."  "You step into the living room."
          "Back to the hallway."    "You walk back to the hallway."
      ]
  }
  where
    kitchen    = Location "Kitchen"
    hallway    = Location "Hallway"
    livingRoom = Location "Living Room"
```

`compileSceneGraph you sceneGraph` turns this into the flat `[AnyAction]` list the engine consumes. Each `biEdge` becomes two movement actions, one per direction; each carries its own label (what the player sees) and arrival narration.

`Scene loc (const [])` says "this location has no per-scene actions." If you want an action that's only available at one place, pass a function of `CharacterId -> [AnyAction]` instead:

```haskell
Scene kitchen (\you -> [anyAction (washHands you)])
```

The scene graph automatically gates per-scene actions on `AtLocation` so you don't have to.

## Adding an action

Actions are typed `Action 'Once` or `Action 'Repeatable` — a phantom type captures whether the action can fire again after being taken. The DSL has builders for both:

```haskell
import Engine.Author.DSL
import GameTypes

washHands :: CharacterId -> Action 'Repeatable
washHands _you = repeatableAction (ActionId "kitchen:wash")
  "Wash your hands."                                          -- label shown to player
  unconditional                                                -- when available
  [ immediate (Narrate "You rinse your hands. The water is cold.") ]
```

Effects are produced with these builders:

- `immediate body` — fires once this tick.
- `eternal body` — fires every tick forever.
- `timed n body` — fires for `n` ticks then expires.
- Each has a `*When` variant (`immediateWhen`, `timedWhen`, `eternalWhen`) that takes a `Condition` guard.

The `body` is an `EffectBody` value: `Narrate "..."`, `Say speaker [listener] "..."`, `Think who "..."`, `AddWorldTag t`, `SetLocation cid loc`, `JournalEntry "..."`, `ModifyRelation from to stat delta`, etc.

To make this action visible to the player, lift it to `AnyAction` and put it in the scenario's action list:

```haskell
scenarioActions = anyAction (washHands you) : compileSceneGraph you sceneGraph
```

Or attach it to a specific scene (so it's only available at the kitchen):

```haskell
Scene kitchen (\you -> [anyAction (washHands you)])
```

## Conditions

A `Condition` gates whether an action is available or whether an effect inside an action fires. Common ones:

- `unconditional` — always.
- `HasWorldTag t` — when tag `t` is on the world.
- `Not c`, `All [c1, c2]`, `Any [c1, c2]` — combinators.
- `AtLocation cid loc` — when the character is at the location.
- `statAbove cid stat n` — when the character's ground-truth stat exceeds `n`.
- `Chance salt p` — fires with probability `p`, deterministic per `salt` and tick.

Building an action that's only available at the kitchen and only after you've turned on the tap:

```haskell
washHands you = repeatableAction (ActionId "kitchen:wash")
  "Wash your hands."
  (All [ AtLocation you (Location "Kitchen")
       , HasWorldTag tapOn ])
  [ immediate (Narrate "Cold water. You feel a little more present.") ]
  where
    tapOn = scenarioTag ("tap-on" :: String)
```

You'd set the `tapOn` tag with another action whose effects include `immediate (AddWorldTag tapOn)`.

## Tags

Every persistent piece of scenario state is a tag. Two flavours:

- **World tags** — global. `worldTags w` is the set; check with `hasTag w t` or condition `HasWorldTag t`.
- **Character tags** — per-character (`charTags character`).

Build scenario tags with `scenarioTag` over any `Show, Read, Eq, Ord` value. Common pattern: a sum type that names the states.

```haskell
data MyState = TapOn | DoorLocked | LightsOn
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

tapOn, doorLocked, lightsOn :: Tag
tapOn      = scenarioTag TapOn
doorLocked = scenarioTag DoorLocked
lightsOn   = scenarioTag LightsOn
```

The engine handles serialization, merge across replays, and tombstones automatically.

## Axioms — letting the world act on its own

An **axiom** is a rule the engine evaluates every tick. It looks at the world (and the diff since last tick) and emits any effects it wants to add. Use axioms for ambient behaviour: weather narration, NPC movement, time-of-day beats.

The engine ships several drop-in axioms in `Engine.Author.CommonAxioms`. Example: emit prose when the time-of-day phase changes.

```haskell
import Engine.Author.CommonAxioms (timeOfDayNarrationAxiom)
import Engine.Core.Time           (TimePhase (..))

phaseProse :: TimePhase -> Maybe String
phaseProse Dawn   = Just "Light gathers at the windows. The room turns blue, then warm."
phaseProse Dusk   = Just "The light goes amber. You haven't moved in some time."
phaseProse _      = Nothing

myFirst seed you = Scenario
  { ...
  , scenarioAxioms = [ timeOfDayNarrationAxiom phaseProse ]
  , ...
  }
```

For richer patterns:

- `Engine.Author.Discovery` — track first-finds (trees, animals, sign) into a catalog.
- `Engine.Author.Rumor` — surface a one-shot ambient line on a player arrival.
- `Engine.Author.Transition` — class-keyed prose for movement between zones.

You can write your own axioms too — `Axiom { axiomId, axiomPriority, axiomEvaluate }` — but reach for the existing ones first.

## Reading state inside an axiom or action

Sometimes an action's effects depend on state at fire time. The DSL exposes a few read helpers:

- `hasTag world tag` — predicate.
- `worldTagList world` — list of all current world tags.
- `characterLocation cid world` — `Maybe Location`.
- `characterArrivals cid diff` — locations a character newly entered this tick (use inside `axiomEvaluate`).
- `getWeather`, `getHour`, `getDayOfWeek`, `getSeason` — environment queries (`Engine.Core.World`).
- `getCharacterStat cid stat world` — `Maybe Int`.

## Terminating

`scenarioTerminal :: Condition` — when this holds, the scenario ends. Common pattern: tag the world when something terminal happens, gate on the tag.

```haskell
scenarioTerminal = HasWorldTag (scenarioTag GoneHome)
```

`Any []` (the default in the skeleton above) is unsatisfiable, so the scenario never auto-terminates.

## Wiring into the launcher

To make your scenario runnable, add it to a launcher executable. The dev launcher lives in `app/dev/Main.hs`:

```haskell
import Scenarios.MyFirst (myFirst)
import SDL.Launcher       (ScenarioEntry (..), runLauncher)
import SDL.Layout         (defaultDisplay)

main = runLauncher
  [ ScenarioEntry "My First"
      "A short walk through the kitchen and back."
      defaultDisplay myFirst Nothing                       -- no custom help screen
  , ...
  ]
```

`ScenarioEntry` carries the label, tagline, display config (`defaultDisplay` if you have no custom HUD), the scenario constructor, and an optional help screen `[String]`.

Then `stack run throughline-exe` opens the picker. Select your scenario.

## The 17 public modules at a glance

When you're ready for richer scenarios, here's where things live.

**Types and primitives**
- `GameTypes` — every type a scenario touches. Always imported.

**Defining what happens**
- `Engine.Author.DSL` — effect builders, action builders, tag helpers, condition combinators.
- `Engine.Author.Scene` — `SceneGraph`, `Scene`, `SceneEdge`, `compileSceneGraph`, `biEdge` / `biEdgeWith`.

**Reusable narrative patterns**
- `Engine.Author.CommonAxioms` — drop-in axioms (weather, time-of-day, mood drift).
- `Engine.Author.Transition` — terrain-class transition pools.
- `Engine.Author.Discovery` — first-find catalog tracking.
- `Engine.Author.Rumor` — one-shot ambient rumors.

**Utilities**
- `Engine.Author.Random` — deterministic seeded RNG (`rollD`, `rollChoice`, `rollCheck`).
- `Engine.Author.Calendar` — short-form date formatting (`formatShortDate`).
- `Engine.Author.Help` — help-screen builder (`helpScreen`).

**State queries**
- `Engine.Core.Conditions` — `checkCondition`, `getCharacterStat`, `hasCharacterStat`.
- `Engine.Core.Time` — `currentHour`, `currentTimePhase`, `TimePhase`.
- `Engine.Core.World` — `characterLocation`, environment queries, relationship setup (`setCharacterStat`, `mkRelationship`, `addRelationship`).

**Rendering / shipping**
- `SDL.Launcher` — `ScenarioEntry`, `runLauncher`.
- `SDL.Layout` — `ScenarioDisplay`, `defaultDisplay`, `LayoutConfig`.
- `SDL.Palette` — semantic colors (`textColor`, `dialogueColor`, `narratorColor`, etc.) and `PaletteMode`.
- `SDL.Text` — string utilities (`stripAnsi`, `wrapWords`, `padRight`) and ANSI colour wrappers (`ansiBold`, `ansiGrey`, ...).

## Working examples

Read these in order, smallest first:

- `app/Scenarios/Customer/` — pure movement, three locations, no actions beyond walking.
- `app/Scenarios/Diner/` — characters, dialogue, conditional actions, ending prose.
- `app/Scenarios/TopBuy/` — multiple scenes, scene-gated actions, scenario-defined axioms.
- `app/Scenarios/DeerHunt/` — full-fledged: procedural map, transition narration, discovery catalog, rumors, common axioms, custom rendering.

If you find yourself reaching for something that isn't in the public API, that's a signal — flag it and we'll either expose the symbol or add a DSL helper to cover the case.
