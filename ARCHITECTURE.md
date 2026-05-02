# Architecture

Throughline is a text-based narrative engine written in Haskell. The player navigates situations through choices, and the world responds through effects on characters, relationships, and internal states. There is no combat loop, no inventory, no XP bar — this is a **storytelling engine**, not a game engine in the traditional sense.

This document describes how the code is organized to support that goal.

---

## Three layers

The codebase is strictly layered. Each layer has a single concern, and dependencies only flow downward.

### 1. Engine builder — `src/Engine/`, `src/GameTypes/`

Primitives: `EffectBody`, `Condition`, `EngineTag`, the `App` monad stack, execution semantics, CRDT merge logic. Changes here affect every scenario.

The engine owns any pattern that exists in reality and would otherwise be reinvented per scenario — hunger, fatigue, mood, social dynamics, shelter, time, weather. Scenario authors get simulation for free.

Public surface: `GameTypes` (data), `Engine.Core.*` (logic), `Engine.Author.*` (DSL for composing scenarios), `Engine.CRDT.*` (sync primitives).

### 2. Scenario author — `app/Scenarios/`

Composes engine primitives to build a specific story. An author's job is *declaring what happens*, not managing sequencing. Each scenario is its own module tree: actions, axioms, characters, locations, narrative pools.

A scenario with no custom axioms is pure data — generable, distributable, replayable without code. Axioms are the only place scenario code escapes into compiled Haskell; that's the trust boundary.

### 3. Player runtime — `src/SDL/`

Sees only prose. Stats, tags, and effects surface through `Engine.Narrative` and authored beats, never through raw UI chrome. The renderer reads `GameWorld` via public facades (`playerLocationName`, `exitBearings`, `engineTimeStatus`) and has no dependency on `Engine.Author` or `Engine.CRDT` internals.

---

## Key design commitments

### Conditions as state gates

A `Condition` guard can check any world state: a stat threshold, a tag, a location, a trust value, a time of day, a prior event. `HasWorldTag`, `AtLocation`, and `RelationAbove` are structurally equivalent — one mechanism, many reads.

### Relationships as first-class data

Trust, affinity, and other relational stats live in `RelationshipGraph` as directed edges, using the same `StatType` and threshold machinery as capacity stats. "Does Alice trust Bob enough to…" is the same query shape as "does Alice have enough strength to…".

### Axiom boundary = data / code

Everything except axioms is serializable: actions, effects, conditions, characters, locations, terminal conditions. Axioms are compiled Haskell plugins. This is simultaneously the **authorship boundary** (what can be generated vs. what must be written) and the **trust boundary** (data is safe to accept from anyone; code is not).

### Engine models reality, not fantasy

When deciding what to add to the engine, the question is "does this exist in reality?" not "would this be a cool game feature?" Strength is human physical capacity across its full range, not a combat stat.

### Identity is cryptographic

A character IS an Ed25519 keypair. Ownership is mathematical, not social. This matters for the next pillar.

---

## Cross-scenario state (the "shared universe")

Scenarios don't run in isolation. World-level diffs — EngineTag changes, stat shifts, relationship updates — flow to a shared world log via CRDTs. ScenarioTag stays private to its scenario; classification is automatic by `Tag` type. Two players running different scenarios in the same world can still affect each other.

This surfaces system weirdness that traditional engines hide: timeline mismatches, existence conflicts, stat divergence from merges. The design leans into these — each one gets named, made lore, and handled narratively rather than papered over.

Detection primitives live in `Engine.CRDT.*`: PN-Counter bucket comparison for stat provenance, ORSet set difference for existence. The vision and priorities live in [`CLAUDE.md`](CLAUDE.md).

---

## Module map

```
src/
├── Engine/
│   ├── Core/          — Effects, Conditions, World, NarrativeMessage
│   ├── Author/        — DSL surface for scenario authors
│   ├── CRDT/          — ORSet, PN-Counter, merge primitives
│   ├── Headless.hs    — coreLoop, StepHook, ActionSource (runtime-agnostic)
│   ├── Runtime.hs     — RuntimeUI facade — how any frontend plugs in
│   └── Sync/          — Session management, snapshot serialization
├── GameTypes/         — Public type surface (data only)
├── SDL/               — SDL2 renderer; never imported by Engine or app
└── MonadStack.hs      — App = ReaderT Env (StateT GameWorld IO)

app/
├── Main.hs            — picks a scenario, hands it to SDL frontend
└── Scenarios/         — one module tree per scenario

test/
├── Engine/            — mirrors src/Engine
└── Scenarios/         — mirrors app/Scenarios
```

See [`CLAUDE.md`](CLAUDE.md) for style guidelines, build prerequisites, and the testing protocol.
