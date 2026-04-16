# Proposal: Adversarial Trust Model (Notes)

## Status: Thinking — not ready for implementation

## The core tension

For merge validation to work, someone has to be honest — either:
- A central oracle signs valid state transitions (contradicts local-first design)
- Participants co-sign snapshots (requires mutual online presence, which we don't have)
- The engine validates diffs against rules (but the engine runs locally — who trusts the engine?)

## What we have now

- Ed25519 keypairs per player (identity is cryptographic)
- Log entries can be signed and verified (`signEntry`/`verifyEntry`)
- `verifyEntry` rejects tampered entries during replay

## What's been discussed

- **Snapshot co-signing**: N participants sign a snapshot → consensus checkpoint. Problem: requires all N to be online simultaneously or to have an async signing protocol.
- **Log chaining**: Each entry includes hash of previous entry (blockchain-style). Prevents insertion/deletion mid-log. Doesn't prevent a dishonest player from forking their own log.
- **Diff validation**: Reject impossible state changes. But "impossible" depends on axioms, which run locally.

## Open questions

1. Can we detect dishonesty after the fact without preventing it? (Name it as lore rather than block it?)
2. Is there a meaningful difference between "adversarial" and "buggy"? A corrupted save and a malicious edit look the same.
3. What's the minimum that makes honest players confident their state won't be corrupted by merging with a dishonest one?
4. Does the "data is safe, axioms are code" boundary from the design doc already handle this? Data can encode malicious state changes even without custom axioms.

## Direction to explore

The most honest approach might be: detect anomalies in merged state (statistical, structural) and surface them as narrative — "something about this doesn't add up" — rather than trying to prevent them cryptographically. This aligns with the "system weirdness as lore" pillar.

Revisit when the merge path is exercised enough to know what real attacks look like.
