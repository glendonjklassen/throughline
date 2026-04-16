# throughline

[![CI](https://github.com/glendonjklassen/throughline/actions/workflows/ci.yml/badge.svg)](https://github.com/glendonjklassen/throughline/actions/workflows/ci.yml)

Throughline is a Haskell narrative engine for deterministic simulation, replayable event logs, and local-first story sync.

It is a personal R&D project focused on modeling narrative state with precision: effects, conditions, relationships, and time are first-class, and scenarios are authored declaratively in Haskell.

## What it is

- Deterministic world updates and replayable logs
- A small DSL for authoring scenarios in Haskell
- Narrative state built from effects, conditions, axioms, and relationship data
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
stack run              # SDL2 window (default)
stack run -- --terminal  # terminal fallback
stack test
```

## Authoring scenarios

Scenario modules live in `app/Scenarios/`.

Authors mainly work with:

- `Action`s that emit effects
- `Condition`s that gate actions and outcomes
- `Axiom`s that watch world diffs and react each tick
- Scenario-specific tags, characters, locations, and terminal conditions

A good starting point is `app/Scenarios/TopBuy.hs` plus the files in `app/Scenarios/TopBuy/`.

## Project layout

- `src/` contains the engine and sync machinery
- `app/` contains scenarios that consume the engine
- `test/` covers engine behavior and sync scenarios

## Local-first sync

The event log is the source of truth; `GameWorld` is a replayable cache.

Each player writes signed local events, can exchange session directories out-of-band, and then deterministically replays the merged log into the same world state. This is not real-time multiplayer. The interesting case is emergent shared state: two independent histories converge into something neither player authored alone.

## More

- Design notes and authoring philosophy: [CLAUDE.md](./CLAUDE.md)
- Multiplayer details: [docs/multiplayer.md](./docs/multiplayer.md)

## On AI Usage

I use Claude and Codex to help build this project. I'm focused on the engine right now so I usually let AI write the prose in this repository. The mileage varies.