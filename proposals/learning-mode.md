# Learning Mode

## Status: V1 Implemented

The core feature is built and usable. Press `d` to cycle debug modes until "learning mode." appears.

## The Problem

The engine runs a growing set of system axioms that silently modify world state each tick: fatigue drains Strength, hunger depletes, social energy shifts based on personality and company. Scenario authors need to understand what the engine is doing in order to write effective narration hooks — but the existing debug modes dump raw state or diffs without attributing changes to their source axioms.

## What V1 Does

After each tick, learning mode displays:

**Axioms fired** — each axiom that produced effects, identified by `AxiomId` (SystemAxiom or ScenarioAxiom), with a readable summary of every effect: stat deltas with +/- signs, tag adds/removes, narrations, location changes. Axioms that produced no effects are omitted.

**Hookable state** — the player character's current engine tags and capacity stat values. These are the things a scenario author can write `Condition` guards against for narration.

### What it does NOT do

- Does not change game behaviour — learning mode is observation only
- Does not appear in player-facing output
- Does not persist to logs or affect CRDT state

## Implementation

- `AxiomTrace` type in `GameTypes.Types` — pairs `AxiomId` with `[Effect]`
- `runAxiomsTraced` in `Engine.Core.Axioms` — evaluates axioms preserving identity; `runAxioms` is defined in terms of it
- `executeStep` stashes traces in `envAxiomTrace` (IORef on Env) each tick
- `debugLearning` in `Terminal.Debug` reads the traces and renders when mode == Learning
- `typewriterHook` in `Terminal.Render` calls `debugLearning` after existing debug output

## Future Work

These extensions would deepen learning mode without changing its core architecture:

- **Axiom descriptions**: each `Axiom` could carry an `axiomDescription :: String` field for learning mode to display alongside the ID
- **Human-readable summaries**: `AxiomTrace` could include a `traceSummary :: String` with context like "introvert, untrusted company" rather than just raw effect data
- **Dry-run preview**: "if the clock ticks now, here's what would happen" without actually advancing — useful for understanding axiom interactions before committing to an action
- **Condition explorer**: given a character's current state, which `Condition` guards would pass? Surface the full list of true conditions, not just the tags
- **Hook suggestions**: based on which tags just changed, suggest DSL patterns the author could use (e.g., "Maya now has `SocialEnergy Drained` — you could gate narration on `HasTag maya (socialEnergyTag Drained)`")
