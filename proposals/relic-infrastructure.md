# Relic Infrastructure (Tier 3)

Concrete design for the Tier-3 partial-key system described in
[`unique-finds.md`](./unique-finds.md).  This proposal pins down:

1. What the keys unlock (the **unlock model**).
2. The wire contract between the client and the arbitrating service
   (the **OpenAPI spec** at [`api/relic-oracle.openapi.yaml`](../api/relic-oracle.openapi.yaml)).
3. The client-side Haskell module layout
   (`Engine.Sync.Relic` + `Engine.Sync.Relic.Shamir`).
4. What stays the same, what's new, and what's deferred.

This is **Mode A (Relic Oracle)** from the parent proposal.  Mode B
(decentralized CRDT log) and Mode C (Nostr relays) are orthogonal
transports; the client abstraction here is designed so either can be
dropped in later without scenario-facing changes.

---

## 1. Unlock model — what the keys actually get you

The parent proposal lists three example sets (Compass / Whisper /
Ledger).  This proposal commits to a single, uniform unlock model
for all sets: **signed content bundles**.

### A bundle is

```json
{
  "bundleId":   "whisper-001",
  "setId":      "whisper",
  "title":      "The Hermit of the North Quarter",
  "kind":       "named-character",
  "body":       { "...": "..." },
  "createdAt":  "2026-04-23T00:00:00Z",
  "signature":  "<oracle Ed25519 signature over everything above>"
}
```

`kind` is one of a small, open enum.  Four shipping kinds:

| kind              | `body` shape                                 | What a scenario does with it                                                                                                |
| ----------------- | -------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------- |
| `lore`            | `{ paragraphs: [String] }`                   | Writes the paragraphs into the player's journal as a "Legends" section.  Read-only flavor.                                  |
| `named-character` | `{ charId, charName, dialogue: [Exchange] }` | Scenarios call `relicCharacterAvailable charId` to decide whether to spawn or merge in this character.  Dialogue is scripted. |
| `map-reveal`      | `{ regionName, placement }`                  | Scenarios call `relicLocationKnown regionName` — future hunts render the region on the map from the start.                  |
| `capability`      | `{ capId, label }`                           | Scenarios gate an otherwise-unavailable action on `relicCapability capId`.  Authoring hook; very sparing use.                |

**Why not raw world-state mutations?**  Because scenarios are
serializable data per the shared-universe pillar, and a raw world
mutation would have to travel through the event log.  Bundles are
*references* the client consults at scenario-render time — they don't
insert into the event log, they gate rendering/availability decisions.
That keeps the log clean and makes unlocks survive scenario edits.

### Why server-held?

The parent proposal raises the question: "should the content be
client-shipped-but-gated, or server-held-and-delivered?"  Ship-and-gate
has a nice offline story but a bad secret story — the content is right
there in the binary, and a determined player has everything before
anyone's combined a key.  Server-held trades offline-until-unlock for
genuine "nobody has this until a group has combined the key".

Once unlocked, the bundle is cached locally (`$XDG_DATA_HOME/throughline/bundles/<bundleId>.json`)
with the oracle's signature, so subsequent play is offline.  Verification
at load time prevents tampering.

### Unlock mechanics

1. Player A holds attestations for k fragments of set S.
2. Client builds a `CombineRequest` — the k `(shareId, attestation)`
   pairs plus Player A's signature over the request.
3. Server verifies each attestation names Player A as current owner,
   reconstructs the set's master secret via Shamir, and — if the
   secret matches the set's pre-committed hash — returns the bundle
   (with an oracle signature over its contents).
4. Client caches the bundle.  Future `relicLocationKnown` / etc.
   calls consult the cache plus verify the oracle signature.

### Not just the combiner gets the unlock

For `map-reveal` and `lore` bundles, the unlock is globally visible
once any group combines — everyone's oracle call returns the same
bundle.  For `named-character` and `capability`, the unlock is
**combiner-scoped** by default (the oracle binds the bundle to the
combining-player pubkeys).  Scenarios decide whether to honor the
combiner-scope or fan it out.

The distinction matches the parent proposal's open question #2
("global vs. combiner-only") with a concrete answer per kind.

---

## 2. The oracle service — what it arbitrates

The server is the **serialization point** for three things:

1. **Claims.** A player found a fragment cache in a hunt; did they
   really?  The oracle checks `cacheId = hash(worldSeed || huntSeed || location)`
   is valid under the genesis seed and hasn't been claimed yet.
2. **Transfers.** Alice wants to gift her fragment to Bob; did Bob
   agree, and does Alice still hold the current attestation?
3. **Combines.** k attestations arrive with an outer player signature;
   do they all name the same player(-set), and does the reconstructed
   secret hash match?

Everything else (narrative flavor, UI, sprites) stays client-side.
The server's role is cryptographic gatekeeping plus delivering bundles.

See `api/relic-oracle.openapi.yaml` for the full contract.

### State the server holds

- Genesis seed for the world (secret; used to derive share data).
- Pre-committed secret-hash per set.
- Current-attestation ledger: `(shareId, serial) → ownerPubkey`.
- Combine history: `(combinerPubkey, setId) → bundleId`.
- Static bundle blobs, indexed by `bundleId`.
- Oracle keypair (Ed25519; used to sign attestations and bundles).

### State the server does *not* hold

- Player private keys.
- Scenario state, world state, hunt seeds (those are player-derived).
- Anything about how often a player plays.

### Operating modes

- **Self-hosted.** Any group can run an oracle against their own
  genesis seed.  Bundles they unlock are real to them.  No merging
  with a canonical oracle — this is the "take it or leave it" shape.
- **Canonical.** A single author-run instance is the default for
  shipped builds.  Opt-out via the `relicMode` scenario config from
  the parent proposal.
- **Player-hosted via Steam (future; spec only).** Steam's
  Dedicated Game Server (SDGS) support or the Steamworks
  GameServerItemsCreate / GameServerBrowse APIs let the game ship
  a server binary that players can launch from the Steam client.
  This is a real shape for communities that want their own private
  relic-world.  Out of scope to *build* right now — the note here
  is a deliberate carve-out so the architecture doesn't accidentally
  preclude it.

  Rough shape when we build it:

  1. Ship a `throughline-oracle-server` binary alongside the game.
  2. It takes a genesis-seed file, a set-catalog JSON, a bundles
     directory, and an Ed25519 keypair on disk.
  3. Steam's server browser lists instances; players can also paste
     a URL into the game's relic-settings screen.
  4. Each server is a separate relic world: claims/transfers from
     server A don't apply on server B.  The game's UI surfaces which
     oracle you're connected to (analogous to a game-server name).
  5. Steam's VAC / community reporting tools handle the trust layer
     around who's running the server.

  Implication for this commit: the OpenAPI spec is the only thing
  a player-run server needs to match; `Engine.Sync.Relic.Types` is
  the canonical Haskell encoding of that spec.  Anyone (us or a
  third party) can implement a conforming server against the YAML.

- **Nostr (orthogonal).** Mode C from the parent proposal.  Uses
  relays instead of a dedicated oracle.  Same `Fragment` /
  `Attestation` / `Bundle` types; different transport.  Not in
  this commit.

### Rate-limits, abuse, and fairness

- Per-IP and per-pubkey rate limits on `POST /claim` and `POST /transfer`.
  Enough to deter brute-forcing cache IDs; loose enough that a group of
  friends combining fragments in an evening isn't throttled.
- `cacheId` space is uniformly distributed and large
  (SHA-256 prefix, ~2^48 used slots out of 2^256); brute-force is
  already impractical without rate-limits.  Rate-limits are belt-and-suspenders.
- No account system.  PlayerId = pubkey is the identity.

---

## 3. Client-side — `Engine.Sync.Relic`

Three modules, three scopes:

### `Engine.Sync.Relic.Shamir`

Minimal GF(256) + Shamir's Secret Sharing, no dependencies beyond
`bytestring` and `random`.  ~150 lines.

- `splitSecret :: SecretBytes -> Threshold -> ShareCount -> [Share]`
  — for the author's genesis tool, not the runtime client.
- `combineShares :: [Share] -> Maybe SecretBytes` — Lagrange
  interpolation at `x=0`.
- `GF256` arithmetic: `gfAdd`, `gfMul`, `gfInv`, precomputed exp/log.

Rolling our own here because:
- The API surface is small.
- Every Haskell SSS package on Hackage is unmaintained or
  vendor-specific.
- We want precise control over the share serialization format so
  the oracle and client agree bit-for-bit.

### `Engine.Sync.Relic.Types`

Pure data types, JSON instances, and the wire contracts that match the
OpenAPI spec field-for-field:

- `SetId`, `CacheId`, `ShareId`, `BundleId`
- `Share`, `Attestation`, `Fragment` (= `Share` + `Attestation`)
- `ClaimRequest`, `ClaimResponse`
- `TransferIntent`, `TransferAccept`, `TransferResponse`
- `CombineRequest`, `CombineResponse`
- `Bundle`, `BundleKind`

Every type has a `FromJSON`/`ToJSON` pair that round-trips against
the OpenAPI schema.

### `Engine.Sync.Relic`

Top-level client API.  Functions are `IO`-typed but the HTTP
transport is pluggable via a `RelicTransport` record so we can stub it
in tests and wire in an `http-client` backend later.

- `RelicTransport` — record of 4 functions (`claim`, `transfer`,
  `combine`, `fetchBundle`) returning `IO (Either RelicError a)`.
- `stubTransport :: RelicTransport` — returns `Left NotConfigured`
  for every call; lets the engine build without an oracle configured.
- `loadFragments :: FilePath -> IO (Map SetId [Fragment])` — read
  owned fragments from local disk.
- `saveFragment :: FilePath -> Fragment -> IO ()` — persist a newly
  claimed/transferred fragment.
- `loadBundle :: FilePath -> BundleId -> IO (Maybe Bundle)` — read a
  cached bundle.
- `saveBundle :: FilePath -> Bundle -> IO ()`

Local storage layout (under `$XDG_DATA_HOME/throughline/`):

```
relic/
  fragments/
    <shareId>.json         one per held fragment
  bundles/
    <bundleId>.json        one per unlocked bundle
```

Both directories are append-only in normal play.  Transfer out deletes
the fragment file only after the server confirms the transfer.

### What's *not* in this commit

- Actual HTTP calls.  The `http-client` dep + concrete
  `httpTransport :: URL -> RelicTransport` ships in a follow-up PR
  once a real oracle instance is up.  `stubTransport` is enough for
  the engine build.
- Server implementation.  OpenAPI spec is the contract; a reference
  server (Haskell `servant` or small `warp` app) is a separate repo.
- Ship-able player-hosted server binary.  Design sketched in §2
  "Player-hosted via Steam"; full implementation deferred.
- Scenario-level DSL for `relicLocationKnown` / `relicCapability` /
  etc.  Those land when the first scenario actually gates something
  on a relic bundle.
- Nostr transport (Mode C).  Parallel to HTTP; reuses the same types.

---

## 4. Deferred decisions

Carrying over from the parent proposal, explicitly unresolved:

- **Oracle hosting.** Start self-hosted; decide on canonical instance
  after a real player has combined a real key.
- **Fragment trading UX.** The gift action is emergent from a
  scenario-level `GiftFragment` DSL verb — not an OS-level inventory
  screen.  Leaves the narrative layer in charge.
- **Myth-tier Tier-2 → fragment bridge.** The proposal says a
  lifetime-find at N > 60 drops a Tier-3 fragment.  That's a future
  hook: when Tier 2 lands, its discovery code calls into `Engine.Sync.Relic`
  to mint a fragment for the player.  Spec'd here; wired there.

---

## 5. Threat model — designing against a malicious oracle

The hard problem with an open protocol is: **once anyone can run a
server that speaks your wire format, anyone can run a lying one.**
Steam community servers, self-hosted instances, third-party clones —
the spec has to make it *infeasible* to implement a malicious oracle
that an honest client will accept as canonical.

The approach, in one sentence: **make the client verify everything
the oracle says against commitments that ship inside the game
binary.**  The oracle is a serialization point for ownership
bookkeeping, not a source of truth for narrative content.

### 5.1 Threats

| # | Threat                                                 | Why it matters                                            |
| - | ------------------------------------------------------ | --------------------------------------------------------- |
| T1 | Fake-claim — oracle allocates fragments to a player who didn't find them | Cheaters bypass discovery                           |
| T2 | Content forgery — oracle returns a bundle with attacker-written body | Malicious bundles in "shared" play: spoilers, griefing, hostile content |
| T3 | Set-swap — oracle pretends set S's unlock is actually set T's (cheaper) content | Cheaters get premium unlocks for easy fragments    |
| T4 | Impersonated canonical — a player-hosted server claims to *be* the canonical oracle and issues canonical-scoped bundles | Pollutes the shared universe's content space |
| T5 | Silent replacement — oracle swaps in new content after launch that wasn't part of the commitment | Narrative drift; author/player disagreement about what unlocks "are" |
| T6 | Double-spend — two recipients both accept a transferred fragment | Ledger inconsistency, angry users                      |
| T7 | DoS / refusal — oracle refuses to serve a bundle the player earned | Mild; players keep local cache                         |
| T8 | Player-activity leak — oracle operator reads who combined what and when | Privacy concern more than a gameplay one               |

### 5.2 Defenses — what the spec must mandate

These are the non-negotiable "if a client doesn't do this it isn't
a conforming client" rules.  The OpenAPI spec links to this section
at the top.

#### D1. Pinned trust anchors — "which oracles speak for which world?"

The shipped game binary embeds:

- `canonicalOraclePubkeys :: [Ed25519Pubkey]` — the author-run
  oracle's signing keys.
- `canonicalGenesisSeedHash :: Hash` — the hash the canonical
  oracle must return from `GET /health`.

On first contact the client verifies that the oracle's `/health`
response's `genesisSeedHash` matches `canonicalGenesisSeedHash`, and
that every signature chain terminates in a key listed in
`canonicalOraclePubkeys`.  If either check fails, the oracle is
"a different world" — still fully usable, but flagged in the UI and
**its bundles never count as canonical**.

This defeats **T4**: a player-hosted impersonator can't get its
bundles treated as canonical because it doesn't have the canonical
private key.  Clients simply don't attach canonical semantics to
its output.

#### D2. Content commitments in the binary

The game binary embeds, per set:

```haskell
data SetCommitment = SetCommitment
  { cmSetId             :: SetId
  , cmThreshold         :: Int
  , cmTotalShares       :: Int
  , cmSecretHash        :: Hash          -- hash of the reconstructable secret
  , cmBundleId          :: BundleId
  , cmBundleContentHash :: Hash          -- hash of the canonical bundle body
  , cmUnlockKind        :: BundleKind
  }
```

After a successful `/combine`, the client:

1. Verifies the oracle's signature on the response.
2. Reconstructs the secret locally via `combineShares`.
3. Checks `hash(secret) == cmSecretHash`.
4. Checks `hash(bundle.body) == cmBundleContentHash`.
5. Only if both match, writes the bundle to the local store and
   treats it as authoritative.

This defeats **T2** (forged body — body hash won't match), **T3**
(set-swap — the wrong set's `cmSecretHash` won't match the
reconstructed secret), **T5** (silent replacement — the commitments
are frozen at binary build time; changing them requires a store
patch, which is visible to players).

#### D3. Hash-chain the commitments

The per-set commitments from D2 live in a single manifest whose
top-level hash is also pinned in the binary:

```
relic-commitments-manifest.json  →  hash →  relicCommitmentsHash
                                               (baked into Haskell source)
```

The manifest itself is published alongside the game (git-tagged,
signed by the author).  This turns the "did someone sneak in a
bad commitment" question into "does the shipped binary agree with
the public manifest", which is a thing external reviewers can
verify independently.

#### D4. Mandatory client-side Shamir verification

The spec requires (not suggests) that the client reconstruct the
secret locally.  The oracle never sends the reconstructed secret
over the wire.  Sending the secret would let a lying oracle skip
the Shamir math entirely and claim any combine "worked".  Keeping
the math client-side means an honest client catches **T1**: if the
claimed fragments don't actually reconstruct a secret that matches
`cmSecretHash`, the client refuses the bundle regardless of what
the oracle says.

#### D5. Serial numbers are monotonic per share

Every `Attestation` carries a `serial`.  The client tracks the
highest serial it has ever seen per `shareId` and refuses any
attestation with a lower or equal serial.  This defeats **T6**
(double-spend): the oracle can't simultaneously issue two valid
attestations with the same serial to different recipients.  If it
tries, one client has already cached a later serial and refuses the
stale one.

#### D6. Signed nonces on all authenticated GETs

`GET /bundles/{id}` requires a signed nonce from a challenge
endpoint, not a session cookie or bearer token.  Defeats replay
attacks and means the oracle operator can't harvest long-lived
credentials.

#### D7. Minimum-disclosure request bodies

Claim/transfer/combine requests contain only the fields the oracle
needs for its decision.  No free-form telemetry, no UA strings.
Soft mitigation for **T8** (activity leak): a curious operator
can still observe who transferred what to whom, but the oracle
doesn't receive information about *how* the player plays — just
about ownership events.

### 5.3 What's residual

- An oracle operator running a player-hosted server still sees its
  users' transfer graph.  That's inherent to being the serialization
  point; the mitigation is "don't trust a random oracle with sensitive
  play state".  Documented in the client's relic-settings UI.
- A determined reverse-engineer can pull the encrypted bundle body
  out of the binary and re-derive the content without ever running
  a legitimate combine.  That's the usual DRM floor and not
  something this spec pretends to fix.  The honest-player path
  stays honest.
- An `unknown` bundle `kind` (a kind the client doesn't recognize)
  is stored but never applied.  Defeats forward-compat attacks where
  a malicious oracle invents a kind with "free loot" semantics an
  unpatched client might honor.

### 5.4 Where to enforce in the spec

The OpenAPI document's top-level description includes a non-normative
pointer to this section, and every endpoint that returns a signed
artifact ("on 200, clients must verify…") carries an inline note.
The Haskell client's public API mirrors this: every function that
touches an oracle response returns `Either RelicError Verified` —
the `Verified` newtype is only constructible through the verification
pipeline, so bypassing the check is a type error rather than a
forgotten one-liner.  That last part lands with the `http-client`
transport PR; for now the stub transport produces `NotConfigured`
before the verification step is even reached.

---

## 6. Security checklist

- Server private key stored in an HSM or at minimum out of the web
  tier (`secrets/oracle.key`, 0600, loaded once at boot).
- Every oracle response carries a signature; the client verifies
  before trusting.
- `cacheId` brute-force is bounded by rate limits + the large ID
  space.
- Share data is not secret on its own — only the attestation binds
  it to an owner.  Leaking share bytes is fine; leaking an attestation
  is not (it represents current ownership).
- Bundles are signed over their entire contents including `createdAt`,
  so a compromised bundle blob can't be served under a forged signature.
- Oracle-key rotation: future oracle responses sign with the new key;
  clients verify against a rolling trust list of (keyId → pubkey)
  entries shipped with the game binary and updated via store patches.

---

## 7. Why this is the minimum

Every component here maps to an explicit concern from the parent proposal:

- **OpenAPI spec** — nails the wire contract so Mode A doesn't drift.
- **Shamir primitives** — needed by both the server (split at genesis)
  and the client (combine at unlock); splitting ownership of the
  code would mean divergent implementations of GF(256).
- **Unlock model** — resolves the parent's open question #2 with a
  per-kind rule.
- **Client stubs** — lets the engine build and the types pass tests
  before any oracle exists, per the "write small, build, iterate"
  policy in CLAUDE.md.

What's explicitly *not* here: the server code, the HTTP transport
implementation, the scenario DSL integration.  Those are separate,
sequenceable commits.
