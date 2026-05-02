# Adversarial Sync: Threat Model for Logs & Snapshots

Concrete threat model and defense plan for the peer-to-peer merge
layer — shared event logs and world snapshots exchanged between
players with no central arbiter.  Companion to
[`adversarial-trust.md`](./adversarial-trust.md) (which captured the
earlier exploratory thinking); this document replaces its "Revisit
when the merge path is exercised" placeholder with a concrete plan.

**Scope.** This is about what happens when Player A's game pulls
Player B's logs or snapshots out of a shared folder (or Nostr relay,
or future sync transport) and attempts to merge.  It is **not**
about the relic oracle — that has a trusted arbiter and is covered
by `Engine.Sync.Relic` and `api/relic-oracle.openapi.yaml`.

**Status.** Proposal.  Describes what to build, not code that's
landed.  Existing signature verification in
`Engine.Sync.Identity.verifyEntry` is the starting baseline.

---

## 1. Why this needs its own model

The relic oracle's threat model relies on a trusted third party.
Peer-to-peer merge has no one to ask.  Every client has to decide
unilaterally whether an incoming log entry or snapshot is
trustworthy, using only:

- The entry's own signature.
- Cryptographic identity of the signer (PlayerId = Ed25519 pubkey).
- Structural checks against the local state.
- An authorization model (to-be-defined) that constrains what any
  given signer is allowed to modify.

The central tension is captured in the old trust notes: for merge
validation to work, **someone has to be honest**.  Without an
oracle, "someone" has to be the engine itself, running locally on
every player's machine — which means the rules it enforces have to
be the same rules on every machine, or two honest players will
disagree about what's valid.

This document commits to a concrete rule set, and to making those
rules part of the engine's correctness contract rather than an
author-by-author choice.

---

## 2. What's in scope

### Log entries

`LogEntry` (`src/GameTypes/Types.hs`):

- `entryId`, `entryClock :: LamportClock`, `entryPlayerId`, `entryActionId`
- `entryDiff :: WorldDiff` — the actual mutation
- `entrySignature :: Maybe ByteString` — Ed25519 over `entryId <> entryActionId <> JSON(diff)`
- `entryFrontier :: CausalFrontier` — each peer's last seen entry id
- `entrySchemaVersion`

Entries arrive via `replayFrom` (`Engine.Sync.EventLog`) from three
sources: the local save, the launcher's shared-folder scan, and
(future) live sync transports.

### Snapshots

`Snapshot` carries the full `GameWorld` plus metadata.  Loaded via
`loadSnapshot`; exchanged alongside logs in `scanSharedLogs`.
Snapshots are currently not signed.  CRDT merge runs over them.

### Shared folder as trust boundary

`SDL.SharedFolder.scanSharedLogs` walks a directory the user
configured via settings and pulls every foreign player's log and
snapshot.  Everything in that directory is implicitly trusted to be
at most what that peer meant to share — the OS filesystem is the
only access control.

---

## 3. Threats

| # | Threat | Where it lives | Worst outcome |
| - | ------ | -------------- | ------------- |
| **T1** | Unsigned entry slips through | `verifyEntry` returns `True` for `entrySignature = Nothing` | Foreign peer writes arbitrary diffs into your world |
| **T2** | Authorization bypass | Any signed diff is accepted as-is | Peer B's entry modifies your character's stats |
| **T3** | Lamport-clock inflation | No upper bound on `lcTick` | Adversary's entries always sort last, overwrite your state |
| **T4** | Replay | Old legitimate entry re-injected | Duplicate effects, double actions |
| **T5** | Causal-parent forgery | `entryFrontier` claims entries you never wrote | Merge ordering manipulated |
| **T6** | Key compromise / no revocation | Leaked private key stays valid forever | Attacker impersonates a player indefinitely |
| **T7** | Identity collision via derived CharId | `playerCharId` uses only the first 12 hex chars | ~1 in 2^48 — theoretical, but possible once the universe has millions of players |
| **T8** | Huge-entry DoS | No size cap on `entryDiff` | Single multi-GB entry stalls replay |
| **T9** | Privacy leak | Diff content is verbatim in the shared file | A shared log reveals how you play in detail |
| **T10** | Untrusted-folder confusion | User adds a malicious folder expecting it to be safe | Same as T1–T5 at scale |
| **S1** | Snapshot tombstone crafting | ORSet tombstones delete entries you added | Mass-removal of your own state at merge time |
| **S2** | Snapshot/log divergence | No explicit rule for which is authoritative | Non-determinism; two honest replays disagree |
| **S3** | Unsigned snapshot | Snapshots have no signature field at all | Foreign peer substitutes arbitrary world state |
| **S4** | Snapshot size DoS | No size cap | Single multi-GB snapshot stalls the merge |

---

## 4. Defenses

Each defense maps to one or more threats above.

### D1. Strict verification mode (T1, T10)

Introduce a `SyncStrictness` config setting with two levels:

- `Local` — current behaviour.  Unsigned entries pass.  Fine for
  single-player and test fixtures that construct entries by hand.
- `SharedFolder` — enabled whenever a non-empty shared folder is
  configured.  **Unsigned entries are rejected outright.**  Non-key
  PlayerIds (short strings like `"test"`, `"player-a"`) are also
  rejected — production play only accepts 64-hex-char pubkey
  PlayerIds with verifying signatures.

Implementation: `verifyEntry` becomes `verifyEntry :: SyncStrictness -> LogEntry -> Bool`.
The strictness value threads through `replayFrom` and the
shared-folder scanner.  `Local` is the default in tests and
solo-local play; the launcher flips to `SharedFolder` when it sets
up a non-empty `sSharedFolder`.

### D2. Authorization model — ownership of state (T2)

This is the largest piece of work in the proposal and the one that
touches every merge path.  The rule set:

1. **Characters have owners.**  Each `CharId` in `worldCharacters`
   carries a `characterOwner :: Maybe PlayerId`.  `Nothing` means
   "world-owned / NPC" (authority on engine-level rules).  `Just p`
   means only signer `p` may emit diffs that mutate this
   character's state (stats, location, relationships, effects).
2. **Scenario-tag namespaces.**  `ScenarioTag` values get an
   authorization class:
     - *Player-owned*: tags whose semantics tie to a specific
       character.  Only that character's owner may add/remove.
     - *World-owned*: tags authored by the scenario for global
       state (weather, clock derivatives, shared flags).  Only
       entries that the scenario's merge axioms authorise may
       mutate.
     - *Shared-additive*: tags that accumulate add-only info from
       many players.  Any signer may add; nobody may remove.
   Default when a scenario doesn't classify is *player-owned for
   tags that name a character; world-owned otherwise*.  Explicit
   classification lives on the `Scenario` record (new field).
3. **EngineTag authorship rules.**  Engine-level tags
   (`Weather`, `Clock`, …) are world-owned; a merge entry that
   tries to rewrite them is rejected.  Exception: `ForeignOrigin`
   tags are self-describing and always authorised from their
   named player.
4. **Rejection shape.**  An unauthorised diff fragment is dropped
   (per-field, not per-entry) with a soft-lore surfacing hook
   ("the woods didn't quite agree with you on this one") rather
   than a hard error, so honest-but-buggy mutual merge doesn't
   corrupt either side.

Implementation sketch:

```haskell
-- In Engine.Sync.Authorization (new module).
data AuthRule = AuthRule
  { authMayMutateCharacter :: PlayerId -> CharId -> Bool
  , authMayAddTag          :: PlayerId -> Tag    -> Bool
  , authMayRemoveTag       :: PlayerId -> Tag    -> Bool
  }

-- Default rule used by every scenario unless overridden.
defaultAuthRule :: GameWorld -> AuthRule

-- Applied before 'applyDiff' during replay/merge.
filterAuthorized :: AuthRule -> PlayerId -> WorldDiff -> (WorldDiff, [AuthViolation])
```

`AuthViolation` is surfaced via the existing `AppError` channel so
debug-mode UI can show it; in `Release` builds the violation is
journalled and the fragment is dropped.

### D3. Clock bounds (T3)

`LamportClock` remains unbounded *mathematically*, but a **skew
cap** is applied on ingest:

- Compute `localMaxTick` = the highest tick this client has seen
  locally.
- Any foreign entry whose `lcTick > localMaxTick + maxSkew` is
  rejected.  `maxSkew` is a session config (default ~10,000 ticks
  = a few in-world days for most scenarios).
- Legitimate peers catching up from a long offline gap bump their
  clock to `max(local, theirs)` on merge, as today.  Skew cap
  still applies because no single peer can legitimately be more
  than `maxSkew` ahead of what we've already seen; sustained
  offline gaps reconcile via periodic snapshots anyway.

Rejected entries surface as `Left (ClockSkewExceeded …)` so debug
tools can distinguish them from signature failures.

### D4. Dedup + replay-proofing (T4)

The existing `(entryPlayerId, entryId)` tuple is already effectively
a dedup key (entryId carries tick + player).  Formalise:

- `replayFrom` keeps a per-player `seenEntryIds :: Set String`.
- Any entry whose id is already in the set is silently skipped.
- Tests in `Engine.Sync.EventLogSpec` cover this path.

Replay attacks across games (replaying a signed entry from scenario
X into scenario Y) are prevented by the entry's `scenarioName`
salt, which should be included in the signing message.  Currently
it's not; fixing that is part of this proposal (see §6 phasing).

### D5. Causal-parent verification (T5)

On ingest, for each entry:

- Every `PlayerId → entryId` in `entryFrontier` must either:
  - refer to an entry already in the local log, OR
  - refer to an entry that *arrives in the same batch*.
- Entries whose frontier points to unknown parents go into a
  pending queue; flushed once parents arrive; dropped with a soft
  error if they stay pending past a session boundary.

Defeats reordering attacks where a malicious peer claims a later
entry was causally prior to one of yours.

### D6. Revocation manifest (T6)

A signed file (`revocations.json`) listing `(pubkey, reason, after)`
triples.  Any entry signed by a revoked pubkey after the `after`
timestamp is rejected on ingest.

Two sources:

- **Self-revocation.** A player signs a revocation of their own
  pubkey (e.g. because they lost the key) from a new, trusted
  pubkey.  The new pubkey is announced through the same channel
  (shared folder, relay, etc.).
- **Community revocation.** For communities running self-hosted
  oracles, a community admin's pubkey can co-sign revocations.
  Clients trust revocations from pubkeys in their per-community
  trust list.

Revocations are additive; a pubkey once revoked stays revoked.
No unrevoke.

### D7. Identity is the full pubkey (T7)

Enforced at the authorization layer: `AuthRule` checks always use
the full `PlayerId` (full hex pubkey), never the derived CharId.
The short CharId stays as a display/rendering convenience but never
participates in trust decisions.  Tests assert that no
authorization-layer function takes a `CharId` directly.

### D8. Size caps (T8, S4)

Hard limits, enforced on ingest:

- `maxEntrySize = 64 KB` — an entry bigger than this is rejected.
- `maxSnapshotSize = 16 MB` — a snapshot bigger than this is
  rejected.

Both are conservative ceilings on real play state; the scenarios in
this repo produce entries in the hundreds of bytes and snapshots
in the tens of KB.  The caps exist to stop an adversary from
stalling merge with a prepared payload.

### D9. Snapshot signing (S3)

Snapshots gain a mandatory `snapshotSignature :: Maybe ByteString`
and a `snapshotSignerPubkey :: PlayerId` field.  `verifyEntry`'s
sibling `verifySnapshot` runs the same Ed25519 check.

In `SharedFolder` strict mode (D1), unsigned snapshots are
rejected.  In `Local` mode they pass — solo-local saves don't
need the overhead.

### D10. Tombstone attribution (S1)

Extends the ORSet change from the earlier tombstone-GC work:

- Each tombstone carries `removedBy :: PlayerId` alongside
  `removedAt`.
- `filterAuthorized` (D2) filters both adds AND removes on ingest.
  A tombstone produced by a signer not authorised to remove the
  underlying value is dropped.

This is the ORSet-level implementation of D2 for the remove path.

### D11. Log-is-canonical rule (S2)

Explicit design rule: **the log is authoritative; snapshots are
caches.**  On any apparent divergence, the log wins.  Snapshot
loads are treated as "start from this cached world and replay any
log entries with greater `entryClock` than the snapshot's
`snapshotClock`".  Mismatches between a replayed-from-log world
and a snapshot are logged as `SnapshotDivergence` warnings; the
snapshot is discarded.

### D12. Privacy disclaimer (T9)

Not a cryptographic defense — a product rule.  Any UI that lets
the user add a shared folder shows a one-sentence note: *"Other
players in this folder will be able to read how you played."*
Documented here so the product decision doesn't get lost.

---

## 5. What's already in the codebase

Baseline coverage, from a skim of `src/Engine/Sync/`:

- **Signatures on entries.**  `signEntry` + `verifyEntry` exist.
  `EventLogSpec` covers "rejects an entry with invalid signature"
  and "unsigned entries always pass" (the T1 hole).
- **Per-entry dedup.**  Entry ids are already effectively unique
  per `(playerId, tick, actionId)`.  Tests cover no-double-apply.
- **Lamport merge.**  Entries are sorted by clock on replay.  No
  skew cap yet (T3 open).
- **Causal frontier field.**  `entryFrontier` is on every entry,
  but nothing currently verifies it — the field is written, not
  read.  D5 flips that.
- **CRDT tombstones with timestamps.**  Landed in the tombstone-GC
  work on `features/unique-finds`.  `orTombstoneAges` gives us the
  hook for D10 attribution — just needs the `removedBy` field.
- **Snapshot load/save.**  Works but is unsigned and unverified
  (S3 open).

Everything else in §4 is new work.

---

## 6. Implementation phasing

Ordered by dependency.  Each phase is a mergeable PR that leaves
the engine working.

### Phase 1 — Strict verification + signed snapshots (D1, D9)

- Add `SyncStrictness` config; thread through `replayFrom` and
  `scanSharedLogs`.
- Add `snapshotSignature` + `snapshotSignerPubkey` to
  `Snapshot`; update `saveSnapshot`/`loadSnapshot`.
- New tests: unsigned-entry-rejected-in-strict-mode,
  unsigned-snapshot-rejected, tampered-snapshot-rejected.
- No authorization yet — this just closes the "unsigned slips in"
  hole.

### Phase 2 — Skew cap + dedup formalisation + scenario-salt (D3, D4)

- Add `maxSkew` to session config; enforce in `replayFrom`.
- Extract `seenEntryIds` into `replayFrom`'s state explicitly.
- Add scenario-name salt to the signing message; bump
  `entrySchemaVersion`.
- Migration: v1 entries are accepted without the salt check (for
  existing shared folders); v2 entries require it.

### Phase 3 — Causal frontier verification (D5)

- Add the pending-queue data structure.
- Flush on batch completion.
- Drop pending entries on session end.

### Phase 4 — Authorization layer (D2, D7, D10)

This is the big one.  Likely ~1k lines touching every scenario.

- Add `characterOwner` to `Character`.
- Add scenario-tag authorization classification to `Scenario`.
- Build `Engine.Sync.Authorization` with `AuthRule`,
  `filterAuthorized`, and default rules.
- Wire through `replayFrom`, all merge axioms, and the snapshot
  merge.
- Extend ORSet tombstones with `removedBy`.
- Update every scenario to declare its tag ownership (a mechanical
  change; default rule covers most cases).
- New test module per scenario asserting that a foreign peer
  can't mutate player-owned state.

### Phase 5 — Revocation + size caps (D6, D8)

- Revocation manifest format + load/verify.
- Size caps (cheap; a few lines each).
- Privacy disclaimer in the shared-folder settings UI (D12).

### Phase 6 — Log-is-canonical formal rule (S2, D11)

- Document in `CLAUDE.md` and the runtime module headers.
- Implement `SnapshotDivergence` warning path.

---

## 7. What stays deferred

- **Full adversarial test harness.**  Property-based tests that
  fuzz a malicious peer against an honest one to catch mutation
  paths the authorization model misses.  Worth having; not in this
  proposal's scope.
- **Online-interactive protocols** (live sync over a WebSocket,
  etc.).  Everything in this proposal is phrased in terms of
  batch ingest from a folder or a pull.  Online transports can
  layer on the same rules, but their timing model is different
  enough to warrant their own doc.
- **Content moderation.**  An authorised peer can still author
  offensive content in the diffs they legitimately own.  That's a
  social problem, not a cryptographic one; out of scope.
- **Post-quantum migration.**  Ed25519 is fine for now.  If
  post-quantum cryptography becomes necessary, the signing
  algorithm becomes a per-entry field rather than a hard-coded
  assumption — another separate proposal.

---

## 8. Relationship to the relic threat model

The two threat models complement each other and share some DNA:

| Concern | Relic oracle (`Engine.Sync.Relic` + `api/relic-oracle.openapi.yaml`) | This doc |
| ------- | --- | --- |
| Trust root | Pinned oracle pubkeys in binary | Pinned revocation manifest + per-session signer trust |
| Identity | Ed25519 pubkey | Ed25519 pubkey (same) |
| Content integrity | Server bundle signature + pinned content hash | Per-entry signature + authorization model |
| Replay defense | Nonce + monotonic serial per share | Dedup on entry id + scenario salt in signing msg |
| Rejection philosophy | Hard — a failed check drops the response | Soft per-field — an unauthorised fragment drops but doesn't poison the entry |
| Deferred gracefully | HTTP transport, server code | Authorization classification, revocation manifest |

Both operate on the same cryptographic primitives and the same
identity system.  Neither depends on the other existing.
