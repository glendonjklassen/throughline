# Unique Finds

A tiered system for rare, identity-bound discoveries that make each hunt feel personal. Tunes the probability distribution so finds are rewarding without grinding.

The core tension this proposal resolves: true uniqueness pulls against per-hunt reward. A BTC-style lottery is globally scarce but individually unsatisfying; a guaranteed per-hunt reward is satisfying but not scarce. The resolution is to **stratify rarity into two tiers**, each with a different uniqueness guarantee and a different probability shape.

> **Status (2026-05-02): Tier 1 shipped, Tier 2 in flight.** A previous Tier 3 (cross-player relic finds via oracle/Nostr) was removed from scope — the engine-side relic infrastructure stays in `Engine.Sync.Relic.*` but no scenario integration is planned here. Reintroduce when shared-universe demand is real.

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
- **N > 60 (Myth):** at this point it is legitimately a once-in-a-playtime event. The journal entry becomes a multi-paragraph centerpiece.

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

### Why it's non-grindy

You can't target it. Grinding hunt #47 doesn't increase the chance; the seed is set. The age-with-wait mechanic further removes grinding incentive: the only way to get a better stag is to play *fewer* hunts once the clock is ticking, which inverts grinding entirely. The social incentive becomes restraint, not repetition.

### Implementation sketch

- Requires persistent player identity (Ed25519 keypair stored locally) — already exists at `~/.local/share/throughline/identity.key`.
- New persistent per-identity profile store: hunt count, current epoch, lifetime stag state (pending / encountered / passed-N-times / claimed / lost). Lives alongside the identity file.
- On hunt start, load profile and increment hunt count.
- Eligibility: `eligible = hash(pubkey || epoch || N) mod 10000 < gamma_threshold(N)` where `N` is the player's hunt counter under the current epoch.
- If eligible, inject the white stag with stature = `statureTier(N)`. Rendering is `pubkey`-derived (identity) plus `N`-derived (scale/age markers/narrative).
- Encounter surfaces a `LifetimeFindChoice` scenario tag the author can hook into: `Claim` rotates epoch + writes journal; `Pass` records a `stagPassedAt N` tag and the stag gains eligibility for reappearance with a decayed probability.

## Cross-tier design notes

### Journal integration

Both tiers write to the player's journal on discovery. The journal already has the catalog view from Phase 7 — add a "Finds" tab with two sections (Signature / Lifetime).

### Narrative surface

Finds must surface through prose, not HUD (per `CLAUDE.md`). A signature buck is described by what makes its rack unusual. The white stag is described by its coat.

### Probability control: hunt-budget draws

Both tiers share a single principle: **the RNG is consumed once per hunt at seeding time, not per-tick**. This makes hunts deterministic given seed, eliminates within-hunt grinding, and makes replays reproducible. A hunt's "rarity draw" is a fixed vector of hashes derived from `hunt_seed`, each comparing against a tier-specific threshold.

## Implementation phases

### Phase 1 — Tier 1 (Signature Find)

- Purely local, no identity required.
- **SHIPPED** in `app/Scenarios/DeerHunt/Signature.hs` and `Discoveries.hs`.

### Phase 2 — Persistent identity wiring

- Ed25519 keypair generation and local storage already exist (`Engine.Sync.Identity`).
- New per-identity profile store: hunt count, `world_epoch`, lifetime stag state.
- Prerequisite for Tier 2.

### Phase 3 — Tier 2 (Lifetime Find)

- Gamma-ish probability over hunts.
- Identity-derived rendering, N-derived stature.
- Encounter narration + Claim/Pass choice.
- Miss / fail-claim / lost-stag handling.
- Epoch rotation on claim.

## Open questions

1. **Tier 2 re-roll on long-inactive players**: if a player plays 3 hunts in 2026 then returns in 2028, should their white stag eligibility still track from the original epoch, or reset? Probably track, but cap the gamma tail so it can't go infinite.
2. **Profile file format and location**: live alongside `~/.local/share/throughline/identity.key` as `profile.json`, or under `sessions/`? Identity-adjacent feels right — it's per-identity state, not per-session.
