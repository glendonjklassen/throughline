# throughline

[![CI](https://github.com/glendonjklassen/throughline-engine/actions/workflows/ci.yml/badge.svg)](https://github.com/glendonjklassen/throughline-engine/actions/workflows/ci.yml)

Throughline is a Haskell engine for authoring shared narrative worlds. Two people can play a scenario independently, merge their signed event logs, and end up in the same world — with history neither of them wrote alone.

It sits in the lineage of interactive fiction and MUDs. The ambition is to give an author the tools to do what Tolkien called subcreation — to build a secondary world with the kind of depth a reader can live in, not just move through.

## What success would look like

An author somewhere uses this to build a world, a small group of people fall in love with it, and it dovetails with other worlds through shared engine state. I don't have a 1.0 target. The closer signal is that I can sit down and author a scenario in flow — when that happens, the kit is probably real enough to hand to someone else.

## What it does differently

Narrative state is modeled as effects, conditions, axioms, and relationships, not HP or inventory. A Strength drop might be injury, fatigue, panic, or grief; the scenario decides which.

The event log is the source of truth; `GameWorld` is a replayable cache. Five paths through state — fresh replay, snapshot plus tail, merged divergent logs, snapshot merge, and active-effect union — are tested to converge to the same world. See [test/ConvergenceSpec.hs](test/ConvergenceSpec.hs).

Merges surface contradictions instead of hiding them. If one history has a character that another doesn't, the engine names the conflict and hands it to the scenario to narrate.

Identity is cryptographic: a character is an Ed25519 keypair. A scenario is pure data unless it ships custom axioms, which are Haskell plugins. Data is safe to accept from anyone; axioms are code.

How the shared universe actually shows up inside a given world — a stranger in your save, a rumor, a dream — is up to the scenario author.

## Quick start

Requires Stack and SDL2 system libraries.

Ubuntu/Debian/WSL:

```bash
sudo apt-get install libsdl2-dev libsdl2-ttf-dev pkg-config
```

Arch:

```bash
sudo pacman -S sdl2 sdl2_ttf pkgconf
```

macOS:

```bash
brew install sdl2 sdl2_ttf pkg-config
```

Then:

```bash
stack build
stack run     # opens the SDL2 window; pick a scenario
stack test
```

## Scenarios

- **Deer Hunt** — mid-November in southern Manitoba, one square mile, one buck. The richest scenario: spatial HUD, zone tints, sparkle hints for deer sign, directional neighbor selection.
- **Top Buy** — a retail ethics dilemma. Your coworker is stealing.
- **Late Night Diner** / **Diner: Maya** — the same 2 AM scene from two seats at the counter.
- **Customer** — a walking prototype.

Scenario modules live under `app/Scenarios/`. Deer Hunt is the deepest read; Top Buy is the simplest.

## Authoring

Authors work with:

- `Action`s that emit effects
- `Condition`s that gate actions and outcomes against any world state (stats, tags, locations, relationships, time)
- `Axiom`s that watch world diffs and react each tick
- `ScenarioDisplay` for per-scenario HUD customization (status line, end screen, zone tint, sparkle)

The engine is supposed to handle most of what narratives need — hunger, fatigue, mood, shelter, social dynamics, psychology. Scenarios should compose those primitives, not reinvent them.

## Layout

- `src/Engine/` — effects, conditions, axioms, world state, CRDTs, sync
- `src/GameTypes/` — public types (`GameWorld`, `Action`, `Effect`, etc.)
- `src/SDL/` — renderer, font, spatial HUD, palette, input
- `app/Scenarios/` — scenarios that consume the engine
- `test/` — hspec suites and QuickCheck properties
- `bench/` — tasty-bench fixtures
- `docs/`, `proposals/` — design

## More

- [ARCHITECTURE.md](./ARCHITECTURE.md) — three-layer model
- [CLAUDE.md](./CLAUDE.md) — vision, pillars, priorities
- [docs/multiplayer.md](./docs/multiplayer.md) — sync internals
- [proposals/shared-universe.md](./proposals/shared-universe.md) — forward-looking design

## On AI usage

I use Claude and Codex as pair programmers. Architecture, design decisions, and narrative direction are mine; the AI translates intent into idiomatic Haskell, catches type errors, and writes prose like this.
