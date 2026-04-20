# throughline

[![CI](https://github.com/glendonjklassen/throughline-engine/actions/workflows/ci.yml/badge.svg)](https://github.com/glendonjklassen/throughline-engine/actions/workflows/ci.yml)

Throughline is a Haskell narrative engine for deterministic simulation, replayable event logs, and local-first story sync.

It is a personal R&D project focused on modeling narrative state with precision: effects, conditions, relationships, and time are first-class, and scenarios are authored declaratively in Haskell.

## What it is

- Deterministic world updates and replayable logs
- A small DSL for authoring scenarios in Haskell
- Narrative state built from effects, conditions, axioms, and relationship data
- A spatial HUD that renders movement as a compass of neighbors around the player, with biome tints, density cues, and a fading trail of where you've been
- Optional local-first sync that merges separate play histories into a shared world

Instead of centering hit points, inventories, or XP bars, Throughline models capacity and context first. A change in `Strength` might mean injury, fatigue, panic, or grief; the scenario decides how to interpret it.

## What it isn't

Throughline is not a commercial engine, a generic RPG toolkit, or a real-time multiplayer framework. It is optimized for learning, experimentation, and strong tests rather than product completeness.

## Quick start

Requires [Stack](https://docs.haskellstack.org/) and SDL2 system libraries.

**System dependencies** (Ubuntu/Debian/WSL):

```bash
sudo apt-get install libsdl2-dev libsdl2-ttf-dev pkg-config
```

**macOS** (via Homebrew):

```bash
brew install sdl2 sdl2_ttf pkg-config
```

Then build and run:

```bash
stack build
stack run     # opens the SDL2 window; pick a scenario from the menu
stack test
```

## Scenarios

Pick one from the launcher:

- **Deer Hunt** — mid-November in southern Manitoba, one square mile, one buck. The richest scenario. Uses the spatial HUD, zone tints, sparkle hints for deer sign, and a directional neighbor-selection model.
- **Top Buy** — a retail ethics dilemma. Your coworker is stealing.
- **Late Night Diner** / **Diner: Maya** — the same 2 AM scene from two different seats at the counter.
- **Customer** — a prototype walking scene.

Scenario modules live under `app/Scenarios/`. Deer Hunt is the most developed starting point for authoring; Top Buy is a simpler read.

## Authoring

Authors mainly work with:

- `Action`s that emit effects
- `Condition`s that gate actions and outcomes
- `Axiom`s that watch world diffs and react each tick
- Scenario-specific tags, characters, locations, and terminal conditions

A `ScenarioDisplay` hook lets a scenario customize the SDL HUD (status line, end screen, shiny-sense sparkle, per-zone tint).

## Project layout

- `src/Engine/` — core engine: effects, conditions, axioms, world state, sync
- `src/GameTypes/` — public types (`GameWorld`, `Action`, `Effect`, etc.)
- `src/SDL/` — SDL2 frontend: renderer, font, spatial HUD, palette, input
- `app/Scenarios/` — scenarios that consume the engine
- `test/` — hspec suites for engine behavior, scenarios, and sync
- `bench/` — tasty-bench performance fixtures
- `docs/` — design notes and proposals
- `proposals/` — forward-looking design work

## Local-first sync

The event log is the source of truth; `GameWorld` is a replayable cache.

Each player writes signed local events, can exchange session directories out-of-band, and then deterministically replays the merged log into the same world state. This is not real-time multiplayer. The interesting case is emergent shared state: two independent histories converge into something neither player authored alone.

See [docs/multiplayer.md](./docs/multiplayer.md) and [proposals/shared-universe.md](./proposals/shared-universe.md).

## More

- Design notes and authoring philosophy: [CLAUDE.md](./CLAUDE.md)
- Sync internals: [docs/multiplayer.md](./docs/multiplayer.md)
- Forward-looking design: [proposals/](./proposals/)

## On AI usage

I use Claude and Codex as pair programmers on this project. Architecture, design decisions, and narrative direction are mine; the AI translates intent into idiomatic Haskell, catches type errors, and writes the prose you're reading right now. Mileage varies.
