# CLAUDE.md — throughline

## Ongoing Work

The work in this section is to be addressed by Claude during its iteration loop.

### Augmentations
Augmentations will be listed here by the human designer to be worked upon iteratively:
- All completed.

### Standard Procedure
The following should be continuously validated - either between implementing augmentations or when there are no augmentations to implment:
- The project effectively models increasingly complex and interesting scenarios
- The project matches the architectural standards set forth in [skills](./.claude/skills/)
- The project remains well-documented and professional-grade in all aspects

When no other tasks are available, the AI may create a proposal in markdown in this repository to further the project based on the above goals, then pushed those recommendations to a `proposals/{feature-name}` branch for review. These will be reviewed by a human and approved explicitly by being added to the CLAUDE.md "Augmentations" section in main branch.

### For Clarity

During the AI loop, you may only implement work that is approved under "Augmentations" in latest `main`.

---

## Vision: Shared Universe

Humans and AI create scenarios. They share engine-level state via CRDTs. Scenarios are played and their states amalgamated and distributed — everyone acts in the same world over time.

### Pillars

1. **Rich engine model** — promote scenario-level patterns (hunger, fatigue, mood, social dynamics, shelter, disease, resources, psychology) to engine-owned tags and axioms. The engine models reality — not fantasy, not game mechanics. Scenario authors get simulation for free.
2. **Cross-scenario state** — world-level diffs (EngineTag, stats, relationships) flow to a shared world log. ScenarioTag stays private to the scenario. Classification is automatic by Tag type.
3. **Lean into system weirdness** — timeline mismatches, existence conflicts, stat divergence from merges, foreign-origin state after ORSet merge. Name each weirdness, make it lore. Detection mechanisms exist (PN-Counter bucket comparison, ORSet set difference). Narrative responses develop as we encounter them.
4. **Scenario serializability** — the axiom boundary. Everything except axioms is serializable data: actions, effects, conditions, characters, locations, terminal conditions. Axioms are compiled Haskell plugins. A scenario with no custom axioms is pure data — generable, distributable, replayable without code. This is also a trust boundary: data is safe to accept from anyone; axiom plugins are code.

### Design decisions

- **Identity is cryptographic**: a character IS an Ed25519 keypair. Ownership is mathematical, not social.
- **Time will be wrong early**: open to getting temporal sync wrong in early versions. Backwards compatibility is the constraint, not correctness on day one.
- **Existence conflicts are features**: lean into the weirdness, name it, don't paper over it.
- **No artificial authorship ceilings**: one DSL, one authorship model. Claude writes Haskell. No separate "simple" format. Axioms are the only boundary (data vs. code), both available to all authors.
- **Engine models reality**: when deciding what to add to the engine, the question is "does this exist in reality?" not "would this be a cool game feature?"

### Implementation priority order

1. Rich engine model (highest leverage — every engine concept is one less thing every scenario reinvents)
2. Scenario serializability (remove `GameWorld ->` wrapper from actions, JSON for all non-axiom types)
3. Cross-scenario state (world log, diff classification, world state loading at scenario start)
4. System weirdness handling (detection mechanisms, naming, narrative responses as lore develops)

Full proposal with technical details at `proposals/shared-universe.md`.

---

## What this is

A text-based narrative engine written in Haskell. It is a learning project for the language, but the design goals are serious: this is meant to be a **storytelling engine**, not a game engine in the traditional sense.

The player navigates situations through choices, and the world responds through effects on characters, relationships, and internal states. There is no combat loop, no inventory management, no XP bar.

## What this is NOT

This is not a standard RPG. Avoid suggesting patterns from that tradition unless explicitly asked.

Specifically:
- **No default health points.** A `Strength` drop is not a hit point system. Something like `health--` isn't forbidden, but it should emerge from specific causes (fight-or-flight overload, prolonged cognitive stress) — not be the default reach. Blunt mechanical shortcuts are valid as emergent outcomes of precise causes, not as primary levers.
- **No combat stats as primary purpose.** Strength is not for hitting things. Intelligence is not for spell damage. These stats model capacity — cognitive, physical, social — across their full range.
- **No loot, levels, or progression in the gamey sense.** Character change is state, not score.
- **No HUD.** Don't render stats directly. Effects surface through prose via `Engine.Narrative`.

---

## Design philosophy

### Three layers

1. **Engine builder** (`src/`). Primitives — `EffectBody`, `Condition`, `EngineTag`, monad stack, execution semantics. Changes here affect all scenarios.
2. **Scenario author** (`app/Scenarios/`). Composes primitives to build a specific story. The author's job is declaring what happens, not managing sequencing.
3. **Player** (runtime). Sees only prose. Stats, tags, and effects surface through `Engine.Narrative` and authored beats.

### Conditions as state gates

A `Condition` guard can check any world state: a stat threshold, a tag, a location, a trust value, a time of day, a prior event. `HasWorldTag`, `AtLocation`, and `RelationAbove` are structurally equivalent.

### Relationships as first-class data

Trust and other relational stats live in `RelationshipGraph` as directed edges, using the same `StatType` and `RelationAbove` conditions as capacity stats.

---

## Style guidelines

### Build prerequisites

SDL2 system libraries are required before `stack build`:
- **Ubuntu/Debian/WSL**: `sudo apt-get install libsdl2-dev libsdl2-ttf-dev pkg-config`
- **macOS**: `brew install sdl2 sdl2_ttf pkg-config`

### Haskell

- Use `stack build` and `stack run` — not `cabal`.
- This is a learning project. Prefer readable, idiomatic Haskell over clever one-liners.
- Use `where` clauses freely for local helpers.
- Pattern match exhaustively; GHC is configured with `-Werror=incomplete-patterns`.
- Prefer `Map.lookup` + `Maybe` chaining over partial functions.
- Don't add type annotations or comments to code that wasn't changed.

### Terminal output

- Dialogue (`Say`): cyan speaker name, grey colon, normal text
- Narrator beats (`Narrate`): green with `>` prefix
- Internal thoughts (`Think`): dim with `~` prefix
- UI chrome (prompts, action list numbers): grey or dim
- Warnings/errors: yellow or red respectively
- No raw `show` on game types in player-facing output

---

## Testing

Run tests with `stack test`.

### Protocol: write the test before the implementation

When a change is complex enough that assertions could be tainted by your implementation — i.e. a change where it'd be tempting to nudge the test to match whatever you just coded — write the test first, watch it fail, *then* implement. Committing source and test together in one commit is fine; the rule is about write-order, not commit granularity.

### When logic gets complex, test first

When reasoning about interactions between multiple engine primitives (effects, conditions, chains, axioms), **stop thinking and write a test**. Express the desired behaviour as a concrete assertion, run it, and iterate.

### Testability requirement

Engine functions should be testable without the full game loop:
- Pure functions (`checkCondition`, `diffWorlds`, etc.) take no monad — test them directly
- `App` functions (`executeBody`, etc.) should be runnable via `runApp` with a minimal `Env` from `TestFixtures.mkEnv`
- Avoid baking I/O concerns into logic that should be pure

---

## On AI usage

Architecture, design decisions, and narrative direction are the developer's. Claude is used as a Haskell tutor and pair programmer — translating intent into idiomatic code, catching type errors, and suggesting patterns. When in doubt, ask before refactoring.

### Multi-agent workflow

Custom agent definitions live in `.claude/agents/`. These define focused roles with scoped responsibilities:

- **engine-coder** — `src/Engine/`, `src/GameTypes/`, `src/MonadStack.hs`
- **terminal-coder** — `src/Terminal/`, `src/Engine/Layout.hs`
- **scenario-coder** — `app/`, `test/`, config
- **qa** — runs tests, fixes only expected failures, pushes unexpected breaks back

The flow: route to the right coder agent, coder makes changes and runs `stack build`, route to QA with predicted failures, QA runs `stack test` and fixes only predicted failures, repeat if unexpected failures.

### Work incrementally

Don't plan large changes before writing any code. Write a small, working piece first, build it, then extend. The loop is: write something small -> build -> iterate.

---

## Skills

Detailed reference material has been extracted into skills that auto-load when relevant. You can also invoke them manually.

- **engine-architecture** — Module structure, App monad, effects/conditions, tags, layer ownership, stats/truth/perception, performance. Auto-loads when working in `src/`.
- **design-checklist** — Implementation checklist, compose-before-inventing rules, ScenarioTag-first policy. Invoke with `/design-checklist [feature]`.
- **current-work** — Terminal visual effects status (done/TODO). Auto-loads when working in `Terminal.*`.
- **sync-design** — Local-first remote state: identity, sessions, snapshots, merge. Auto-loads when working on `Engine.Sync` or `Engine.Runtime`.

---

