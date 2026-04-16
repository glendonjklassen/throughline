# Shared Universe: Vision & Architecture

## The Vision

Humans and AI create scenarios. They share a rich vocabulary of engine tags, axioms, and model primitives. Scenarios are played and their states amalgamated and distributed — everyone acts in the same world over time.

The engine models reality. Not fantasy, not game mechanics — how life and the world actually work. Hunger, weather, fatigue, social dynamics, disease, shelter. The richer the engine's model of reality, the more scenarios share without coupling to each other.

When the distributed system produces weirdness — timeline mismatches, existence conflicts, stat divergences from merges — that weirdness becomes part of the world's lore. Name it, lean into it, make it a feature of how this universe works.

A character is defined by an Ed25519 keypair. Identity is cryptographic, not social.

---

## Current State

### What works

| Component | Notes |
|-----------|-------|
| CRDT merge (ORSet, PN-Counter) | Commutative, idempotent, property-tested |
| Ed25519 identity + signed event logs | Per-player attribution, tamper-evident |
| Lamport clocks + deterministic replay | Canonical ordering across players |
| Axiom system reacts to diffs, not actions | This is what makes cross-scenario compatibility possible |
| Perceived vs Truth stats | Asymmetric knowledge already modeled |
| Common axioms (fatigue, hunger, mood, weather) | Good patterns, but wired to scenario-specific tags |
| DSL for effects, conditions, actions | Expressive, compositional |

### What's missing

| Gap | Description |
|-----|-------------|
| Engine richness | 5 EngineTag concepts total. Everything else is opaque `ScenarioTag String`. Two scenarios can't share fatigue state because "tired" isn't an engine concept. |
| Cross-scenario state | Sync works within one scenario. No mechanism for state to flow between scenarios that share the same world. |
| Scenario serializability | `scenarioActions` and `axiomEvaluate` are Haskell functions — can't be serialized, distributed, or generated as data. |
| World persistence | No world state that outlives a single scenario session. |

---

## Key Architectural Decisions

### The engine IS the shared universe

Not a server, not a protocol. The engine's type system, tag vocabulary, and axiom library define what's real. Two scenarios sharing the engine share reality. CRDTs don't care whether two divergent states came from the same scenario or different ones — `mergeWorlds` is already scenario-agnostic.

### Scenario serializability: the axiom boundary

Everything except axioms becomes serializable data: actions, effects, conditions, characters, locations, terminal conditions. These are already ADTs with JSON instances (or close to it). The `GameWorld -> Action` function wrapper on actions needs to go — the real logic is in `Condition`, which is already data.

Axioms remain compiled Haskell. They inspect diffs, check conditions, and emit effects — that's genuinely arbitrary logic. They're plugins.

A scenario with no custom axioms (relying entirely on engine axioms) is pure data. It can be generated, serialized, distributed, and replayed without shipping code. A scenario with custom axioms ships Haskell.

This creates a trust boundary: serializable scenario data is safe to accept from anyone (it can only express things the engine already understands). Axiom plugins are code and need a different trust model.

### No artificial authorship ceilings

One DSL, one authorship model. Claude writes Haskell. Humans write Haskell. The DSL should be compositional enough that both find it easy. No separate "simple" format that caps what you can express. Axioms are the only boundary (data vs. code), and both are available to all authors.

### Model reality

The engine models how life works. Physical needs, weather, social dynamics, psychology, time. Not game mechanics, not fantasy. When deciding what to add to the engine, the question is "does this exist in reality?" not "would this be a cool game feature?"

---

## Engine Richness: What Needs to Exist

Currently the engine knows about 5 things: Weather, Clock, Tension, DialogueInProgress, ActionTaken. Common axioms handle hunger, fatigue, weather influence, and mood drift — but they're parameterized by scenario-specific tags, so two scenarios can't share that state.

The engine needs to own these domains directly, with EngineTag constructors and engine-level axioms that run automatically in every scenario.

### Physical world

Additions to EngineTag: Temperature, Terrain, Shelter, LightLevel, Noise. Weather already exists — extend it with wind, precipitation intensity, visibility.

Engine axioms: temperature follows time-of-day + season + weather. Light tracks sun position. Shelter modifies temperature/weather exposure on characters.

### Biological needs

Additions to CapacityStat: Thirst, Warmth, Health.

Engine axioms: thirst depletes over time (faster than hunger), restored by water sources. Warmth drains when temperature is low and shelter is insufficient. Health drains from disease/injury, recovers slowly with rest and shelter. Promote existing hunger and fatigue CommonAxioms to engine-level.

### Social dynamics

Additions to StatType: Obligation, Reputation, Influence, Familiarity.

Engine axioms: Familiarity increases with co-location over time. Reputation drifts toward Trust (what you do becomes what people think of you). Unpaid obligations erode Trust.

Current system has Trust. Attention and Respect were removed (unused). The additions model the parts of social reality Trust doesn't cover.

### Psychological state

Additions to CapacityStat: Stress, Morale.

Additions to EngineTag: MoodState, Motivation.

Engine axioms: stress accumulates from adverse physical conditions (cold, hungry, threatened) and social conflict. Stress drains other capacities. Morale affected by social interaction quality, shelter, food. Mood transitions based on stat thresholds.

### Resources

Additions to EngineTag: ResourceAvailable (type + location), ResourceDepleted (type + location).

Engine axioms: resources at a location decrease with use. Some resources regenerate seasonally. This is deliberately minimal — "there is water at the river" and "the river dried up" are world-level facts. The engine is not an inventory system.

---

## Cross-Scenario State

### The problem

State lives in `sessions/<scenarioName>/<playerId>/`. Two scenarios produce separate state trees. If both modify the same character's Hunger stat, those changes never meet.

### The mechanism

Introduce a world log alongside scenario logs:

```
sessions/
  world/
    <playerId>/
      events.jsonl       -- engine-level diffs only
      snapshot.json
  scenarios/
    <scenarioName>/
      <playerId>/
        events.jsonl     -- scenario-specific diffs
        snapshot.json
```

When a scenario executes an effect that modifies engine-level state (EngineTag, CapacityStat, relationship stats), the diff goes to both the scenario log and the world log. ScenarioTag changes go only to the scenario log.

Classification is automatic based on `Tag` type — `EngineTag` changes are world-level, `ScenarioTag` changes are scenario-level.

When a scenario starts, it loads world state first, then overlays scenario-specific state. World logs merge across all scenarios. Scenario logs merge only within their scenario.

---

## System Weirdness as Lore

When the distributed system does something strange, we name it and make it part of how this world works. We don't know all the weirdnesses yet — they'll reveal themselves as we build. But the posture is: lean in, don't paper over.

Some we can anticipate:

**Timeline mismatches**: Player A is at hour 14, Player B is at hour 6. After merge, the world has state from both times. We don't know yet how to handle this cleanly. Early versions will get it wrong. That's fine — backwards compatibility is the constraint, not correctness on day one.

**Existence conflicts**: A character dies in one scenario, alive in another. The merge produces a world where both happened. This is genuinely interesting and we want to see what it feels like before deciding how to handle it.

**Stat divergence from merge**: After PN-Counter merge, a stat value lands somewhere neither player put it. The mechanism to detect this exists (compare pre-merge and post-merge values per player bucket). What to do with that detection is an open question.

**Foreign-origin state**: After ORSet merge, tags appear that weren't in the local set. The mechanism to detect this exists (set difference before/after merge). Same open question on what to do with it.

These are concrete detection problems with known mechanisms. The narrative and lore side will develop as we encounter them in practice.

---

## Implementation Priorities

### 1. Rich engine model (highest leverage)

Every concept added to the engine is one less thing every scenario reinvents. This is the work that makes everything else possible — cross-scenario state is meaningless if the engine only knows about 5 things, and AI authorship is hard if the author has to rebuild basic reality from scratch.

- Expand EngineTag, CapacityStat, StatType as described above
- Promote CommonAxioms to engine axioms
- Add new engine axioms for the new domains
- Update Narrative.hs for all new stats and tags

### 2. Scenario serializability

- Make Action a pure data type (remove `GameWorld ->` wrapper, rely on `Condition` for gating)
- Ensure all non-axiom scenario components have JSON serialization
- Define the boundary: serializable scenario data vs. axiom plugin

### 3. Cross-scenario state

- Split WorldDiff into engine-level and scenario-level
- Introduce world log
- World state loading at scenario start
- Test convergence across scenarios

### 4. System weirdness handling

- Build detection mechanisms for merge artifacts (stat divergence, foreign-origin state, timeline mismatch)
- Name and document each weirdness as it's encountered
- Build narrative/axiom responses as the lore develops

---

## Open Questions

1. **Temporal consistency**: If Player A is at hour 14 and Player B is at hour 6, what does the merged world look like? Lamport clocks handle causal order but not real-time alignment. We'll get this wrong early and iterate.

2. **Existence conflicts**: Character dead in one scenario, alive in another. We want to lean into this but don't yet know what that looks like mechanically.

3. **World scale**: How big does the world get? The CRDT merge is O(n) in world size. What are the practical limits before merge becomes expensive?

4. **Scenario-to-engine promotion**: When should a `ScenarioTag` become an `EngineTag`? If multiple authors independently define `ScenarioTag "tired"`, that's a signal. Is promotion manual or is there a mechanism?
