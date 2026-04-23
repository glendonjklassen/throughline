# Unique Finds

A tiered system for rare, identity-bound discoveries that make each hunt feel personal and connect players across the shared world. Tunes the probability distribution so finds are rewarding without grinding, and uses partial-key cryptography to turn world-scarce finds into a social object rather than a trophy.

The core tension this proposal resolves: true uniqueness pulls against per-hunt reward. A BTC-style lottery is globally scarce but individually unsatisfying; a guaranteed per-hunt reward is satisfying but not scarce. The resolution is to **stratify rarity into three tiers**, each with a different uniqueness guarantee and a different probability shape.

## Tier 1 — Signature Find (per-hunt unique)

### What it is

Every hunt seeds exactly one "signature find" — a buck with a rack pattern, a clearing with a layout, or a sign (rub, scrape, track set) whose exact form is derived from `hash(hunt_seed)`. It exists for sure. Whether you find it is the question.

### Probability shape

Find probability per hunt is tuned to **40–70%**. High enough that most hunts produce one; low enough that finding it feels like an achievement. The draw is deterministic from `hunt_seed`, so replaying the same seed yields the same signature find in the same place — no reroll farming within a hunt.

### Why it's non-grindy

One hunt, one chance. You can't farm the signature find by repeating a hunt because the hunt itself is the cost. If you miss it, that specific signature is gone.

### Implementation sketch

- Add a `SignatureFind` scenario tag seeded once at hunt start from `hunt_seed`.
- Reuse the existing rare-find placement code but elevate one slot to "signature" with a distinguishing render (unusual antler geometry, oversized tracks, an uncommon sign type).
- On discovery, record the signature's hash in the player's journal as a permanent entry.

## Tier 2 — Lifetime Find (per-player unique, ages with patience)

### What it is

Each player identity has exactly one "white stag" across their entire lifetime of play. It is gated by the player's Ed25519 public key. **The stag ages with the wait**: the longer it takes to appear, the older, rarer, and more legendary it becomes. An early sighting is the same stag it would have been — but a *younger, lesser* version of itself. A late sighting is the elder, the one that will be remembered.

The design intent: **finding it early should feel like regret, not triumph**. Players who see it on hunt #3 should feel they glimpsed something that could have been grander. Players who encounter it at hunt #40 meet a creature that has grown into its legend.

### Value function

The stag's identity is fixed by `hash(player_pubkey || world_epoch)` — its coat pattern, its territory, its behavior. But a second hash dimension — **the hunt number `N` at which it appears** — determines its *stature*:

- **N = 1–5 (Yearling):** a small white stag, unremarkable rack, thin journal entry. "You saw a white deer once." Low-tier catalog prestige.
- **N = 6–15 (Prime):** a proper stag, distinct rack, a real story. Mid-tier prestige.
- **N = 16–30 (Elder):** a heavy-bodied legend, characterful rack, multi-paragraph journal entry, unlocks a named narrative beat.
- **N = 31–60 (Ancient):** scarred, white through-and-through, carries a unique scenario capability when encountered (e.g., marks a location on future hunts' maps).
- **N > 60 (Myth):** drops a fragment of a relic set on discovery, tying Tier 2 into Tier 3. At this point it is legitimately a once-in-a-playtime event.

The rendering (coat, antler form) is determined by `pubkey` so it's recognizably *yours* at every stature. The scale, age markers, and accompanying narrative grow with `N`.

### Probability shape

Flipped from the prior geometric design. Early appearances should be **both rare and sad** — bad luck in two dimensions. A gamma-ish distribution with the mass centered around hunt 12–25:

- P(N = 1–3): ~4% (rare, Yearling — true bad luck)
- P(N = 4–10): ~22% (Prime, early end)
- P(N = 11–25): ~45% (Prime/Elder, the common case)
- P(N = 26–50): ~23% (Elder/Ancient, excellent)
- P(N = 51–100): ~5% (Ancient/Myth, legendary)
- P(N > 100): ~1% (Myth, story for the ages)

Expected value ~22 hunts. 95th percentile ~60 hunts. A newcomer can still hit early (dreaded Yearling outcome); a patient veteran is statistically more likely to meet a fully-grown legend.

Once encountered, `world_epoch` rotates with a long real-world cooldown (~1 month) so long-term players can pursue another lifetime stag — whose age clock resets.

### The "let it pass" choice

When the stag is encountered, the player sees it in narration before any action is forced — it's described at its current stature. The player chooses:

- **Claim it** (shoot/tag/photograph, depending on scenario). The stag is locked in at its current stature and `world_epoch` rotates. Journal entry is written.
- **Let it pass.** The stag is not claimed, the epoch does not rotate. It will reappear later, **at a later `N`, grown further**. The probability of re-encounter in subsequent hunts drops (say, 20% per hunt after), so passing is a real gamble: you might never see it again in this epoch.

This is where the sadness becomes mechanical. A player who sees a Yearling and reflexively claims it has foreclosed the Elder. A player who lets a Prime pass hoping for Elder may come up empty. The "correct" move depends on how much the player trusts their own patience and their remaining play horizon.

### Missing the stag

Yes, it's possible to miss it — and "miss" has three distinct meanings, each handled differently:

1. **Fail to encounter during an eligible hunt.** The hunt was seeded to contain the stag, but the player ranged the wrong territory and never crossed its path before hunt-end. The stag **lingers**: the next hunt is auto-eligible with no change to `N`. The stag doesn't "grow" on a missed-encounter hunt — it's been there waiting, not aging in the wild. This prevents the frustrating case where someone's lifetime stag is forever wasted on a hunt they didn't even see it in.

2. **Fail the claim.** Encountered and attempted (shot missed, spooked by wind, camera fumbled). Treated as a forced Pass: the stag flees, reappearance probability follows the same decay curve (20% per subsequent hunt, compounding downward). Narrative surface: "you saw it for an instant — it's gone." This is a real loss of opportunity, and it should sting.

3. **Consciously let it pass or repeatedly fail to reclaim.** After ~8–10 passes/failures, the decay curve makes reappearance effectively <1% per hunt. At that point the stag is **lost for this epoch**. The player's lifetime stag in the current epoch is effectively gone. No claim, no journal entry — just a quiet `LifetimeStagLost` scenario tag that a future scenario author can hook into (a campfire mention years later, a rumor another hunter brings).

A lost stag does **not** rotate the epoch automatically. The player is stuck between two unpleasant options: wait out a very-long-tail reappearance (say, a 1-in-500 hunts chance the stag resurfaces as an Ancient regardless of pass history — one last mercy), or manually request an epoch rotation (available only after, say, a real-world year of the stag being lost). The second option is a clean break: you accept the loss, and a new stag starts its clock.

The "Myth" outcome (N > 60) and the "Lost" outcome are two sides of the same patience coin. Restraint pays off or it doesn't.

### Why it's non-grindy

You can't target it. Grinding hunt #47 doesn't increase the chance; the seed is set. The age-with-wait mechanic further removes grinding incentive: the only way to get a better stag is to play *fewer* hunts once the clock is ticking, which inverts grinding entirely. The social incentive becomes restraint, not repetition.

### Implementation sketch

- Requires persistent player identity (Ed25519 keypair stored locally).
- On hunt start, compute `eligible = hash(pubkey || epoch || N) mod 10000 < gamma_threshold(N)` where `N` is the player's hunt counter under the current epoch.
- If eligible, inject the white stag with stature = `statureTier(N)`. Rendering is `pubkey`-derived (identity) plus `N`-derived (scale/age markers/narrative).
- Encounter surfaces a `LifetimeFindChoice` scenario tag the author can hook into: `Claim` rotates epoch + writes journal; `Pass` records a `stagPassedAt N` tag and the stag gains eligibility for reappearance with a decayed probability.
- On myth-tier discovery (N > 60), emit a Tier-3 fragment in addition to the journal entry.

## Tier 3 — Relic Find (world-scarce, multi-set partial keys)

### What it is

A finite, deterministic set of **relic caches** seeded at world genesis. Each relic contains a fragment of an Ed25519 keypair. Fragments combine via Shamir's Secret Sharing (SSS) to reconstruct full keys that unlock world-level capabilities.

### Multiple sets, not one

Instead of a single "master key" split across the world, seed **N independent relic sets**, each with its own k-of-n threshold. Concretely:

- **Set A (Compass, 3-of-5):** fragments scattered globally. Full key unlocks a world location (a hidden valley, a lost cabin).
- **Set B (Whisper, 2-of-3):** full key unlocks a character — a hermit NPC with scenario-specific dialogue and capabilities.
- **Set C (Ledger, 4-of-7):** full key unlocks a cross-scenario capability (e.g., a scenario author can reference "the Ledger" as a gated precondition).
- **Set D–Z:** more sets, tuned so there are ~100 fragments globally across all sets.

A player who finds relics over time will accumulate **pieces of several sets** — maybe one Compass fragment and two Whisper fragments. This makes coordination interesting: you can unlock Whisper alone or with one partner, but Compass requires finding two other Compass-holders. Ledger requires a small guild.

Fragments are **tradeable in-world** (a scenario action: gift a fragment to another character), which creates a social economy without explicit trading menus.

### Probability shape

Per-hunt probability of finding *any* relic fragment is tuned against expected global player-hours. If the target is that the global supply depletes over ~2 years at 100 active players averaging 2 hunts/week:

- 100 players × 2 hunts/week × 104 weeks = 20,800 hunts over 2 years
- 100 fragments to place → 0.48% per-hunt base rate
- Round to 0.5% and tune down as claim rate observed

A player averaging 2 hunts/week has ~50% chance of finding at least one fragment within a year. Concentration of fragments into sets means even a lucky player probably can't solo-unlock anything except the smallest sets.

### Why it's non-grindy

The rate is low enough that grinding isn't rewarding in expectation — you'd need hundreds of hunts for a second fragment on average. The interesting gameplay isn't *finding* fragments (that's slow drip), it's *combining* them with other players, which is social and can't be grinded.

### The cheat problem

Tier 3 is the only tier with a cheat surface worth defending:

- **Fragment forgery** — a player claims to hold a fragment they never found.
- **Double-claim** — a found fragment is already "spent" in another player's merge.
- **Double-spend on transfer** — a player transfers the same fragment to two recipients.
- **Collusion against the supply cap** — fabricated fragments exceed the 100-global-cap.

Tiers 1 and 2 are immune: the signature find and white stag are seeded deterministically from data the player already has (hunt_seed, player_pubkey), so there's nothing to forge — the client can reconstruct the truth locally.

### Trading fragments: crypto mechanics

Fragments are tradeable, which means the ownership of a fragment must be a first-class, verifiable fact. The core insight: **the SSS share data stays constant, but the oracle's (or log's) attestation of current ownership rotates on each transfer.** A share is only combinable if its current attestation binds it to the combining player.

Each fragment exists in two parts:

- **Share data** (`s`): a point on the SSS polynomial. Deterministic from `world_seed`. Never changes across transfers. Alice and Bob and Carol could all hold copies of `s` as a blob — it doesn't matter, because...
- **Attestation** (`a`): a signed statement `sig_oracle(share_id || current_owner_pubkey || serial)`. Only one attestation is valid at a time per share. Combination math requires `k` shares, each with a **current** attestation bound to the combining party (or a coalition whose pubkeys are all attested).

A share without a current attestation is a dead blob. Holding the bytes doesn't grant ownership; holding the current attestation does.

#### Transfer flow (Mode A, Oracle)

Alice wants to gift a fragment to Bob:

1. **Alice signs a transfer intent**: `intent = sig_alice(Transfer(share_id, from=Alice_pk, to=Bob_pk, nonce, ts))`. Unilateral — doesn't take effect yet.
2. **Bob countersigns**: `accept = sig_bob(Accept(intent_hash))`. Prevents Alice from "gifting" fragments to people without their knowledge (which matters because a transferred fragment might carry narrative obligations).
3. **Submit to oracle**: client posts `(intent, accept)` to `POST /transfer`. Oracle verifies both signatures, checks that Alice holds the current attestation, and atomically:
   - Revokes Alice's attestation (bumps `serial` in the ledger).
   - Issues a new attestation binding `share_id` to `Bob_pk`.
4. **Oracle returns the new attestation to Bob.** Bob now holds `(s, a_new)`. Alice still has `s` and `a_old`, but `a_old` is superseded — any combination attempt using it fails verification.

This defeats double-spend by making the oracle the serialization point. If Alice tries to simultaneously transfer to Bob and to Carol, whichever reaches the oracle first wins; the second is rejected because Alice no longer holds the current attestation.

#### Transfer flow (Mode B, decentralized)

When the shared-universe CRDT log is live, the oracle is replaced by the log:

1. Alice signs the intent and broadcasts it to the log.
2. Bob signs the accept and broadcasts it.
3. The combined `(intent, accept)` tuple is a log entry. On merge, entries are ordered by a deterministic rule (e.g., hash-tiebroken wall clock); the first valid transfer of a given `(share_id, serial)` wins. Subsequent transfers with the same serial are rejected on merge.
4. The "current attestation" is then *implicit*: whoever the log's latest valid transfer points to.

Combination verifies by walking the log for each participating share. Double-spend is resolved by the merge rule: one of the recipients wins, the other sees their copy invalidated. This is a worse UX than Mode A (someone may think they have a fragment and find out later they don't) but requires no trusted third party.

#### What crypto identity specifically does here

Every security property of the trade rests on Ed25519 identity:

- **Ownership = public key.** A fragment is "owned by" a specific `Ed25519_pubkey`. Not a username, not an account.
- **Transfer requires the current owner's signature.** No one but Alice can initiate a transfer of Alice's fragment, because forging her signature is computationally infeasible.
- **Recipient consent requires Bob's signature.** Prevents unilateral "dumping" of fragments onto unwilling recipients.
- **Non-repudiation.** Alice's signed intent is a permanent record. She can't later claim she didn't transfer it.
- **Lost keys = lost fragments.** If Alice loses her private key, her fragments are unrecoverable. This is the sharp edge of crypto identity and must be surfaced in onboarding. Consider encrypted key backup with a passphrase, stored locally.

#### Offline gifting

If Alice is offline (or the oracle is), she can still produce a signed `intent` and hand it to Bob (in the game: a gift action during shared play; physically: a QR code, a pasted blob). Bob signs the `accept` later and submits the tuple when he has oracle access. The oracle will honor it if Alice hasn't meanwhile transferred the same fragment to someone else who got to the oracle first.

This means Alice **can** cheat a casual player by writing two offline intents and handing them to two different people — but only one will be honored, and the cheater's identity is on the losing intent, which Bob (or the log) can publish as proof of bad faith. Social consequence replaces cryptographic impossibility for this edge case.

#### What a "gift" looks like in the game

A scenario-level action `GiftFragment targetCharacter shareId` is available when two player characters are co-located (or in an async scenario, when Alice includes Bob in the hunt's guest list). Narratively it's a small ceremony: handing over a stone, carving a mark, burying a token together. The crypto is invisible — the client constructs `intent`, requests signature from Bob's client, submits to oracle, and the UI shows the fragment moving from one inventory to the other.

## Gating Tier 3: modest, modular centralization

The user has indicated a preference for **modest and modular centralization — take it or leave it**. The design therefore offers a centralized mode and a decentralized fallback, and the engine uses whichever is configured without semantic change at the scenario layer.

### Mode A — Relic Oracle (centralized, modular)

A small HTTP service (the "relic oracle") that holds the genesis seed for all relic sets and signs fragment-claim attestations. Flow:

1. Player encounters a relic cache in a hunt. Client computes the cache's canonical identifier: `cache_id = hash(world_seed || hunt_seed || location)`.
2. Client submits `(cache_id, player_pubkey, proof_of_hunt)` to the oracle.
3. Oracle checks: is this `cache_id` valid under the genesis seed? Has it been claimed? Is the proof_of_hunt well-formed?
4. If valid and unclaimed, oracle returns a signed fragment `(fragment_data, oracle_signature)` and records the claim.
5. Fragment is signed with the oracle's key; combination into a full key verifies both the SSS threshold and the oracle signatures.

This defeats all three cheat vectors: forgery fails signature check, double-claims are rejected at the oracle, supply cap is enforced by the oracle's ledger.

**Modularity**: the oracle is a single endpoint with a narrow API (`POST /claim`, `GET /cache/:id`). Scenarios that don't care about Tier 3 simply don't call it. The engine treats oracle-signed fragments as a specific tag type; absence is fine.

**Take it or leave it**: players who run offline or against a local oracle get a parallel relic space that doesn't merge with the canonical one. Their finds are still real to them and their group.

### Mode B — Threshold attestation (decentralized fallback)

If/when the shared-universe CRDT world log is live, Tier 3 can migrate to an attestation scheme:

- Fragments are still seeded deterministically from `world_seed`.
- Claim is broadcast to the world log as `(cache_id, player_pubkey, found_at_tick)`.
- The fragment data itself is only revealed when a player can produce a VRF proof that `hash(player_pubkey || hunt_seed || tick) < threshold` — proving they had legitimate opportunity to find it during a valid hunt.
- Double-claim is resolved by "first writer wins" in the CRDT merge order.

This doesn't prevent all cheating (a determined attacker who controls hunt_seed generation can bias) but it makes casual cheating hard and aligns with the shared-universe vision. Mode A is the pragmatic starting point; Mode B is the long-term.

### Mode C — Nostr relays (leading candidate)

Nostr is a protocol for signed JSON events published to "relays" (dumb pipes that store events and serve queries). Hundreds of public relays exist and are free to use; anyone can run their own as a single small binary. **No per-action fees, no wallet UX, no service obligation on the author.** This is important: if the engine required players to pay gas to claim or trade fragments, coordinating a group of friends becomes a pay-to-play headache. Nostr avoids that entirely — players just publish signed events.

**How it would map to fragments:**

- **Claim**: client publishes a signed event (`kind` = custom, e.g. 30001) containing `{cache_id, pubkey, proof}` to N relays.
- **Transfer intent/accept**: two signed events referencing the cache_id and each other, published to relays.
- **Combination check**: client queries multiple relays for events relating to the target cache_ids, merges results locally, verifies the signature chain of custody before allowing combination.
- **Double-claim / double-spend resolution**: "first-seen-by-quorum" convention — whichever signed event reaches a quorum of queried relays first is treated as canonical. Not cryptographically enforced.

**Fit with the crypto-identity pillar:**

Nostr uses Schnorr/secp256k1 rather than Ed25519. Options:

- Have each player hold both an Ed25519 identity (for scenario/character ownership per the shared-universe vision) and a linked secp256k1 identity (for Nostr publication). The link is itself a signed attestation ("this secp256k1 key belongs to this Ed25519 character").
- Or reconsider the curve choice for player identity — secp256k1 is also fine cryptographically and aligns with a broader ecosystem (Bitcoin, Ethereum, Nostr). Worth reopening that decision before it ossifies.

**Honest weaknesses:**

- **Soft supply cap.** Without consensus, two players can both "claim" the same cache via different relay quorums. Reconciliation is best-effort. For a 100-fragment cap, this means the actual number of "claimed" fragments in circulation might briefly diverge. Eventually clients reading from wide relay sets will converge, but not atomically.
- **Relay availability over decade timescales.** The protocol will persist; specific relays come and go. Clients should rotate across a configurable relay pool.
- **Relays don't filter semantically** — anyone can publish a malformed or fake claim event. Clients must verify signatures and the `cache_id`-under-`world_seed` check locally before trusting.

**Why it's probably the right answer for this project:**

- Zero hosting cost, zero ops, no service to babysit.
- No transaction fees — friends can play together without anyone opening a wallet.
- Matches the "cryptographic identity + signed events" model already at the core of the shared-universe vision.
- Protocol has survived its initial hype cycle and has real, ongoing adoption.
- The soft-consensus weakness is acceptable for a narrative system where "someone somewhere briefly thinks they have a fragment that turns out to belong to someone else" becomes lore, not a crisis (see pillar 3: lean into system weirdness).

**Hybrid option:** Nostr for all claim/transfer/discovery events + a self-signed author-maintained ledger (just a signed JSON file published periodically to GitHub or IPFS) that declares canonical resolution of supply-cap disputes. This is modest, modular, and low-effort — basically "the author publishes a quarterly dispute resolution" rather than running a live service.

### Which mode is active

A scenario-level config flag `relicMode :: RelicMode = Oracle URL | Nostr [RelayURL] | Decentralized | Disabled`. The engine exposes `claimFragment :: CacheId -> App (Maybe Fragment)` which routes based on the mode. Scenario code is mode-agnostic.

**Explicitly rejected: smart-contract oracle (L2 or otherwise).** Although an L2 contract would provide strong supply-cap enforcement with no hosted service, it requires every player to hold a funded wallet and pay per-action gas. For a cooperative narrative engine where friends coordinate to combine fragments, forcing each participant to open a wallet and fund it would be a significant friction — pay-to-play for shared storytelling. Not the right tradeoff for this project. Keeping the note here so the decision doesn't get re-litigated.

## Cross-tier design notes

### Journal integration

All three tiers write to the player's journal on discovery. The journal already has the catalog view from Phase 7 — add a "Finds" tab with three sections (Signature / Lifetime / Relic). Relic entries show which set the fragment belongs to and how many pieces the player currently holds.

### Narrative surface

Finds must surface through prose, not HUD (per `CLAUDE.md`). A signature buck is described by what makes its rack unusual. The white stag is described by its coat. A relic cache is a physical object in the world — a cairn, a carved stone, a buried tin — described in narration and discovered through interaction, not rolled against in a hidden table.

### Probability control: hunt-budget draws

All three tiers share a single principle: **the RNG is consumed once per hunt at seeding time, not per-tick**. This makes hunts deterministic given seed, eliminates within-hunt grinding, and makes replays reproducible. A hunt's "rarity draw" is a fixed vector of hashes derived from `hunt_seed`, each comparing against a tier-specific threshold.

## Implementation phases

### Phase 1 — Tier 1 (Signature Find)

- Purely local, no identity required.
- Extend existing rare-find placement to elevate one slot to signature.
- Add hash-derived rack/coat/sign rendering.
- Add journal entry.
- Fully rewarding in isolation — can ship without Tiers 2 or 3.

### Phase 2 — Persistent identity

- Ed25519 keypair generation and local storage.
- `world_epoch` tracking per identity.
- Prerequisite for Tiers 2 and 3.

### Phase 3 — Tier 2 (Lifetime Find)

- Geometric distribution over hunts.
- Identity-derived rendering.
- Epoch rotation on discovery.

### Phase 4 — Tier 3 mode A (Relic Oracle)

- Implement oracle service (small, can be a single Haskell binary).
- Define SSS fragment format and set metadata.
- Client-side `claimFragment` call and signature verification.
- Unlock mechanics for the first two or three sets (Compass, Whisper).

### Phase 5 — Tier 3 mode B (decentralized, later)

- Migrate after the shared-universe CRDT log is live.
- VRF-based legitimacy proofs.
- Oracle becomes optional / archival.

## Open questions

1. **Fragment trading**: explicit action (`GiftFragment playerId`) or emergent (fragment is a tag, scenarios decide transfer rules)? Leaning emergent.
2. **Unlock semantics**: when a set is fully combined, does the unlock apply globally (everyone sees the hidden valley) or only to the combiners? Probably global for Compass-type unlocks, combiner-only for capability unlocks.
3. **Oracle hosting**: self-hosted per-community or single canonical instance? The modularity makes either viable; start self-hosted to avoid infrastructure commitment.
4. **Tier 2 re-roll on long-inactive players**: if a player plays 3 hunts in 2026 then returns in 2028, should their white stag eligibility still track from the original epoch, or reset? Probably track, but cap the geometric tail so it can't go infinite.
