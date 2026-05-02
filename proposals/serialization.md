# Proposal: Serializable Scenarios

> **Status (2026-05-02): structural goals shipped; full axiom conversion deferred.**
>
> What landed:
> - `LocationGraph` + region/coordinate data (`Region`, `lgEdges`, `lgRegions`, `lgCoords`) on `GameWorld`
> - Condition extensions: `CoLocated`, `InRegion`, `InSameRegion`, `Chance`, `HasCoLocated`
> - Spatial random `EffectBody` variants: `SetLocationRandom`, `SetLocationAdjacent`, `SetLocationAdjacentPrefer`
> - `Narration` data type (with `NarrationPool`)
> - `AxiomRule` / `MergeAxiomRule` evaluator (`Engine.Core.AxiomRule`) with the `self` substitution convention
> - `scenarioActions :: [AnyAction]` — the `GameWorld ->` wrapper is gone; gating runs through `actionCondition`
> - `Snapshot` extended with `snapActions`, `snapRules`, `snapMergeRules` (legacy snapshots without these fields still load — they default to empty)
> - JSON instances for the full handoff package, with round-trip tests in `Engine.JSONRoundTripSpec`
> - `mergeActions` / `mergeRules` / `mergeMergeRules` / `mergeLocationGraphs` wired into `offerMerge` and the live merge path
> - `Eq` derivations on `Character`, `GameWorld`, `Snapshot` (round-trip tests need them; harmless otherwise)
>
> What did NOT land — and why:
> - **Scenario axiom conversion is partial.** `dawnRule`, `smallAskRule`, `kyleAuditRule`, `earlyReportRule` are `AxiomRule`s. The rest of TopBuy and DeerHunt's axioms remain as code `Axiom`s.
> - The original "19/19 convert" audit was aspirational. Two recurring blockers turned up:
>   1. **"Set absolute" stat semantics are CRDT-incompatible.** `shiftAxiom`'s `strDelta = 5 - currentStrength` cannot be expressed as a serializable effect, because `Capacity` stats are `PNCounter PlayerId` and PN-Counters don't have a set operation that survives merge. Adding `SetStat` to `EffectBody` would silently break convergence under multi-player merge. (See note below.) This is a **modeling problem**, not a serialization problem.
>   2. **State-reading triggers.** `tensionAxiom` (TopBuy + DeerHunt) reads 8 world tags in priority order to compute a single tension level; `deerMovementAxiom` and `spookAxiom` carry HuntWorld closures and probabilistic logic that don't fit the declarative rule shape without substantially expanding the trigger language.
> - Per the proposal's own "What stays as code" section, this is acceptable. Code axioms remain a supported escape hatch.

## CRDT note: "set absolute" is not a missing primitive

`Capacity` and relationship stats are stored as `PNCounter PlayerId`. PN-Counters merge by per-player max of additive and subtractive buckets. There is no set operation that converges:

> Player A sees Strength = 3 and "sets to 5" → emits +2 in A's bucket.
> Player B sees Strength = 7 and "sets to 5" → emits −2 in B's bucket.
> After merge, neither player has Strength = 5 — the buckets sum independently.

Any axiom that wants reset-style semantics (`shiftAxiom` is the canonical example) is **already subtly wrong under multi-player merge**, regardless of serialization. Single-player works because there's only one bucket. Fixing this is a separate modeling project (see `proposals/shared-universe.md` — capacity-as-derived, LWW registers, or shift-tagged overage are the candidates).

## Motivation

A scenario today is a Haskell value with embedded functions — it can't be saved to disk, transferred between players, or handed off between scenarios at runtime. The goal is to make everything — actions, axiom rules, state, and topology — into pure, serializable data so that a scenario handoff package can travel as JSON. Randomness is included via deterministic `Chance` conditions and random spatial effects, both seeded from the Lamport clock.

## Current state

### Already serializable (pure data, no functions)

- `Effect` / `EffectBody` — every constructor is data
- `Condition` — pure ADT (`HasTag`, `RelationAbove`, `All`, `Any`, `Not`, ...)
- `Action f` — record of `ActionId`, `String`, `Maybe Entity`, `Condition`, `[Effect]`
- `GameWorld`, `WorldDiff`, `MergeDiff` — pure data
- All tags, stats, characters, locations, relationships

### Function barriers (what blocks serialization today)

| Type | Field | Current signature | Why it's a function |
|------|-------|-------------------|---------------------|
| `Scenario` | `scenarioActions` | `GameWorld -> [AnyAction]` | Filters actions by world state |
| `Axiom` | `axiomEvaluate` | `GameWorld -> [AnyAction] -> WorldDiff -> [Effect]` | Reactive logic |
| `MergeAxiom` | `mergeAxiomEvaluate` | `GameWorld -> MergeDiff -> [Effect]` | Merge-reactive logic |
| `Scene` | `sceneActions` | `CharId -> [GameWorld -> AnyAction]` | Generates actions per character |
| `SceneEdge` | `edgeNarration` | `GameWorld -> String` | Dynamic narration text |
| `Env` | `envActions` | `GameWorld -> [AnyAction]` | Runtime action factory |

### Axiom audit

12 system axioms ship with the engine binary and don't need serialization. Of the 19 scenario axioms across all scenarios:

- **19 of 19** become serializable under this proposal
- 0 require code-only treatment

The last two holdouts (`deerMovement`, `spook`) were blocked only by randomness, which this proposal addresses with `Chance` conditions and random effect variants.

---

## Design

### 1. LocationGraph — topology as engine data

Locations exist today as opaque `Location` (newtype over `String`). Spatial concepts like adjacency and zones live in scenario code (e.g., `DeerHunt.Locations`). This proposal promotes topology to engine-level data.

```haskell
newtype Region = Region { regionName :: String }
  deriving (Eq, Ord, Show, Read, Generic)

data LocationGraph = LocationGraph
  { lgEdges   :: Set (Location, Location)   -- undirected adjacency
  , lgRegions :: Map Location Region         -- each location belongs to a region
  } deriving (Show, Eq, Generic)
```

`LocationGraph` is added to `GameWorld`:

```haskell
data GameWorld = GameWorld
  { worldCharacters     :: Map CharId Character
  , worldGraph          :: RelationshipGraph
  , worldLocations      :: Map CharId Location
  , worldActiveEffects  :: [LiveEffect]
  , worldTags           :: ORSet Tag
  , worldClock          :: LamportClock
  , worldLocationGraph  :: LocationGraph       -- NEW
  }
```

**Adjacency helpers** become engine functions that read the graph:

```haskell
adjacentTo :: Location -> LocationGraph -> [Location]
inRegion :: Location -> LocationGraph -> Maybe Region
sameRegion :: Location -> Location -> LocationGraph -> Bool
```

**DeerHunt's `Locations.hs`** collapses from 282 lines of code into one `LocationGraph` value — all the adjacency pairs and zone assignments become data.

**Merge weirdness**: When two scenarios' location graphs merge (ORSet union on edges, Map union on regions), characters may arrive at locations the receiver's graph doesn't contain. The engine detects this ("unknown location") and narrates it rather than erroring. Disconnected locations that arrive via merge are valid — places you've heard of but don't know how to reach.

### 2. Condition extensions

```haskell
data Condition
  = HasTag CharId Tag
  | HasWorldTag Tag
  | RelationAbove CharId CharId StatType Int
  | AtLocation CharId Location
  | CoLocated CharId CharId              -- NEW: same location
  | InRegion CharId Region               -- NEW: character in a named region
  | InSameRegion CharId CharId           -- NEW: characters in same region
  | Chance Int Double                    -- NEW: deterministic probability (salt, p)
  | Not Condition
  | All [Condition]
  | Any [Condition]
  deriving (Show, Eq, Generic)
```

**`CoLocated`**: checks `worldLocations` for matching values. No lookup of specific location needed — the condition checker handles it.

**`InRegion`**: checks `worldLocationGraph` for region membership. Generalizes DeerHunt's zone concept.

**`InSameRegion`**: combines the above — both characters' locations resolve to the same region.

**`Chance salt probability`**: deterministic from Lamport clock. `checkCondition` computes `rollCheck` using `lcTick (worldClock g) * 7919 + salt`. Same PRNG as today, just expressed as data. A `Chance 42 0.3` fires ~30% of ticks, deterministically.

### 3. AxiomRule — declarative axiom type

```haskell
data Trigger
  = WhenTagAdded Tag                -- tag appeared in diff (character or world)
  | WhenWorldTagAdded Tag           -- world tag specifically
  | WhenStatChanged StatType        -- any character's stat of this type changed
  | WhenRelationChanged StatType    -- any relationship stat of this type changed
  | WhenLocationChanged             -- any character moved
  | EveryTick                       -- fires unconditionally
  deriving (Show, Eq, Generic)

data Target
  = EachCharacter                   -- bind Self to each character in world
  | SpecificChar CharId             -- bind Self to one character
  | ChangedChars                    -- bind Self to chars that changed in diff
  | CoLocatedWith CharId            -- bind Self to chars at same location
  | CharsAtLocation Location        -- bind Self to chars at specific location
  deriving (Show, Eq, Generic)

data AxiomRule = AxiomRule
  { ruleId       :: AxiomId
  , rulePriority :: Int
  , ruleTrigger  :: Trigger
  , ruleGuard    :: Condition         -- reuses existing Condition type
  , ruleTarget   :: Target
  , ruleEffects  :: [Effect]          -- reuses existing Effect type
  } deriving (Show, Eq, Generic)
```

#### The Self convention

When a rule has a multi-character target (`EachCharacter`, `ChangedChars`, `CoLocatedWith`), effects and conditions need to refer to "the current target character." We introduce a sentinel:

```haskell
self :: CharId
self = CharId "§self"
```

The rule evaluator substitutes `self` with the actual `CharId` when producing effects. This means a rule like "for each character, if not sleeping, drain Strength" looks like:

```haskell
AxiomRule
  { ruleId       = SystemAxiom "fatigue"
  , rulePriority = 3
  , ruleTrigger  = WhenWorldTagAdded (EngineTag (Clock (TimeOfDay 0)))  -- any hour
  , ruleGuard    = Not (HasTag self (Engine Sleeping))
  , ruleTarget   = EachCharacter
  , ruleEffects  = [immediate (ModifyRelation self self (Capacity Strength) (-1))]
  }
```

Note: this simplified example doesn't capture the circadian curve (hour-dependent drain amounts). The real fatigue axiom could be expressed as multiple rules with hour-range guards, or it can stay as a system code axiom since it ships with the engine anyway. See "What stays as code" below.

#### Evaluator

New module `Engine.Core.AxiomRule` with:

```haskell
evaluateRule :: GameWorld -> [AnyAction] -> WorldDiff -> AxiomRule -> [Effect]
```

The evaluator:
1. Checks `ruleTrigger` against `WorldDiff`
2. Resolves `ruleTarget` to `[CharId]`
3. For each target char, substitutes `self` in `ruleGuard` and checks it
4. For each passing char, substitutes `self` in `ruleEffects` and collects them

#### Merge rules

Merge axioms follow the same pattern but trigger on `MergeDiff`:

```haskell
data MergeTrigger
  = WhenMergeRelationChanged
  | WhenMergeLocationChanged
  | WhenMergeTagChanged
  | WhenMergeWorldTagChanged
  | OnAnyMerge
  deriving (Show, Eq, Generic)

data MergeAxiomRule = MergeAxiomRule
  { mergeRuleId       :: AxiomId
  , mergeRulePriority :: Int
  , mergeRuleTrigger  :: MergeTrigger
  , mergeRuleProvenance :: Maybe Provenance  -- Nothing = any, Just Unaware = only unaware
  , mergeRuleGuard    :: Condition
  , mergeRuleEffects  :: [Effect]
  } deriving (Show, Eq, Generic)
```

### 4. New EffectBody variants for spatial randomness

```haskell
data EffectBody
  = ... -- all existing constructors unchanged
  | SetLocationRandom CharId Int [Location]          -- pick uniformly from list (salt)
  | SetLocationAdjacent CharId Int                    -- pick random neighbor (salt)
  | SetLocationAdjacentPrefer CharId Int Region       -- prefer neighbors in region (salt)
  deriving (Show, Eq, Generic)
```

These cover `deerMovement` (adjacent with zone preference) and `spook` (random from list). The salt + Lamport clock produces the same deterministic PRNG as `rollChoice` today. The interpreter reads `worldLocationGraph` for adjacency.

### 5. Narration as data

`SceneEdge.edgeNarration :: GameWorld -> String` becomes:

```haskell
data Narration
  = Static String
  | Conditional [(Condition, String)] String   -- [(guard, text)] with fallback
  deriving (Show, Eq, Generic)
```

Most edges already use `const narration` (static text). The `Conditional` variant handles "if raining, say X; otherwise say Y."

### 6. Actions as data (remove GameWorld -> wrapper)

**`Scenario.scenarioActions`** changes from `GameWorld -> [AnyAction]` to `[AnyAction]`. Each action already carries `actionCondition :: Condition` — the runtime filters by checking conditions against current world state. The function wrapper is redundant.

**`Env.envActions`** changes correspondingly to `[AnyAction]`.

**`Scene`** changes:
```haskell
data Scene = Scene
  { sceneLocation :: Location
  , sceneActions  :: [AnyAction]         -- was: CharId -> [GameWorld -> AnyAction]
  }
```

**`SceneEdge`** changes:
```haskell
data SceneEdge = SceneEdge
  { edgeId        :: ActionId
  , edgeFrom      :: Location
  , edgeTo        :: Location
  , edgeLabel     :: String
  , edgeNarration :: Narration           -- was: GameWorld -> String
  , edgeCondition :: Condition
  }
```

**`buildActions`** returns `[AnyAction]` instead of `[GameWorld -> AnyAction]`. Location-gating is baked into each action's condition.

**DSL changes**:
- `atScene` returns `[AnyAction]` with `AtLocation` conditions pre-composed
- `togglePair` returns `(Action 'Repeatable, Action 'Repeatable)` directly

### 7. Scenario type with rules

```haskell
data Scenario = Scenario
  { scenarioName         :: String
  , scenarioInitial      :: GameWorld
  , scenarioActions      :: [AnyAction]          -- was: GameWorld -> [AnyAction]
  , scenarioAxioms       :: [Axiom]              -- code axioms (if any)
  , scenarioRules        :: [AxiomRule]           -- NEW: declarative rules
  , scenarioMergeAxioms  :: [MergeAxiom]          -- code merge axioms (if any)
  , scenarioMergeRules   :: [MergeAxiomRule]      -- NEW: declarative merge rules
  , scenarioTerminal     :: Condition
  , scenarioDebugDefault :: DebugMode
  , scenarioPlayerCharId :: CharId
  }
```

---

## The handoff package

When scenario A completes and hands off to scenario B, the portable data is:

| Component | Type | Status |
|-----------|------|--------|
| World state | `GameWorld` | already serializable |
| Location topology | `LocationGraph` (in GameWorld) | new, serializable |
| Available actions | `[AnyAction]` | serializable (wrapper removed) |
| Reactive rules | `[AxiomRule]` | new, serializable |
| Merge rules | `[MergeAxiomRule]` | new, serializable |
| Code axiom refs | `[AxiomId]` | serializable (just IDs) |
| Terminal condition | `Condition` | already serializable |

A scenario with no code axioms is **pure portable data**. A scenario with code axioms lists them as requirements — the receiving end either has the plugin or gets a warning.

---

## Snapshot merge — pulling in foreign scenario data

### Extended Snapshot

Today `Snapshot` is just `GameWorld` + offset. With serializable scenario data, snapshots carry the full portable package:

```haskell
data Snapshot = Snapshot
  { snapWorld      :: GameWorld           -- includes LocationGraph
  , snapOffset     :: Int
  , snapActions    :: [AnyAction]         -- NEW
  , snapRules      :: [AxiomRule]         -- NEW
  , snapMergeRules :: [MergeAxiomRule]    -- NEW
  }
```

When you accept a foreign player's snapshot, their scenario data merges into yours alongside the world state.

### Merge semantics

**LocationGraph** (already in `GameWorld`, handled by `mergeWorlds`):

```haskell
mergeLocationGraphs :: LocationGraph -> LocationGraph -> LocationGraph
mergeLocationGraphs a b = LocationGraph
  { lgEdges   = Set.union (lgEdges a) (lgEdges b)
  , lgRegions = Map.union (lgRegions a) (lgRegions b)  -- left-biased on conflict
  }
```

Set union on edges: their locations and connections appear in your world. Map union on regions: if the same location is in different regions across graphs, left-bias keeps yours (and the conflict is detectable weirdness). This is one additional line in the existing `mergeWorlds`.

**Actions** — union by `actionId`:

```haskell
mergeActions :: [AnyAction] -> [AnyAction] -> [AnyAction]
mergeActions mine theirs = mine ++ filter isNew theirs
  where
    myIds = Set.fromList [actionId a | AnyAction a <- mine]
    isNew (AnyAction a) = actionId a `notMember` myIds
```

Same ID = same action, keep ours. New ID = new action added to the available pool. Actions carry their own conditions, so a foreign action that references a location or tag your scenario doesn't have simply never becomes available (its condition fails). Harmless.

**AxiomRules** — union by `ruleId`:

```haskell
mergeRules :: [AxiomRule] -> [AxiomRule] -> [AxiomRule]
mergeRules mine theirs = mine ++ filter (\r -> ruleId r `notElem` myIds) theirs
  where myIds = map ruleId mine
```

Same pattern. New rules from the foreign scenario start firing alongside yours. A foreign rule whose trigger/guard references tags your scenario doesn't use will simply never fire. A foreign rule that *does* fire because your scenario happens to match its conditions — that's cross-scenario bleed, and it's intentional. Name the weirdness later.

**MergeAxiomRules** — identical pattern, union by `mergeRuleId`.

### Runtime integration

`offerMerge` in `Engine.Runtime` currently merges `GameWorld` values and runs merge axioms. The change:

1. After accepting a merge, extract `snapActions`, `snapRules`, `snapMergeRules` from the foreign snapshot
2. Merge them into the live `Env` using the union-by-ID functions above
3. The `Env` fields (`envActions`, `envRules`, etc.) become `IORef`s so they can grow mid-session

```haskell
-- In offerMerge, after accepting:
let newActions    = mergeActions currentActions (snapActions foreignSnap)
    newRules      = mergeRules currentRules (snapRules foreignSnap)
    newMergeRules = mergeMergeRules currentMergeRules (snapMergeRules foreignSnap)
-- Update the live Env
writeIORef (envActionsRef env) newActions
writeIORef (envRulesRef env) newRules
```

This works for both the startup merge (`offerMerge`) and between-turn live merge (`mkLiveMerge`). Foreign scenario data arrives exactly when foreign world state does — same prompt, same acceptance, same moment.

### What this enables

Player A plays DeerHunt. Player B plays Diner. They exchange snapshots. Player A's world now has:
- Diner locations (counter, booth, parking lot) alongside hunting locations — disconnected subgraphs in the LocationGraph
- Diner-specific actions (order coffee, talk to Frank) — available if Player A ever ends up at a Diner location
- Diner axiom rules — fire if their conditions happen to match

Whether this produces interesting narrative or noise is a design question for later. The mechanism is here; the curation is separate work.

---

## What stays as code

**System axioms** (12) ship with the engine. They don't need serialization. Some have logic complex enough that expressing them as rules would be awkward (circadian fatigue curve, perception drift iterating over stat types, day-advance modular arithmetic). These are fine as code — they're part of the binary.

**Scenario code axioms** can still exist for genuinely complex logic that doesn't fit the rule model. The `Axiom` type is unchanged. But based on the audit, no current scenario axiom actually needs this — all 19 are expressible as rules.

---

## Implementation order

Each step should build and test before moving to the next.

### Step 1: LocationGraph + Condition extensions
- Add `Region`, `LocationGraph` types to `GameTypes.Types`
- Add `worldLocationGraph` to `GameWorld`
- Add `CoLocated`, `InRegion`, `InSameRegion`, `Chance` to `Condition`
- Update `checkCondition` in `Engine.Core.Conditions`
- Update all `GameWorld` constructors (scenarios, tests, fixtures) with empty/default graph
- Convert DeerHunt `Locations.hs` to build a `LocationGraph` value
- Tests: condition checks for new constructors

### Step 2: AxiomRule type + evaluator
- Add `Trigger`, `Target`, `AxiomRule`, `self` to `GameTypes.Types`
- Add `MergeTrigger`, `MergeAxiomRule`
- New module `Engine.Core.AxiomRule` with `evaluateRule`
- Wire rule evaluation into the axiom loop (alongside existing `Axiom` evaluation)
- Add `scenarioRules` / `scenarioMergeRules` to `Scenario`, `envRules` to `Env`
- Tests: convert one simple scenario axiom to a rule, verify identical behavior

### Step 3: Spatial effects + Narration
- Add `SetLocationRandom`, `SetLocationAdjacent`, `SetLocationAdjacentPrefer` to `EffectBody`
- Implement execution in `Engine.Core.Effects`
- Add `Narration` type, update `SceneEdge`
- Tests: random location effects produce deterministic results

### Step 4: Remove GameWorld -> wrapper from actions
- Change `scenarioActions` from `GameWorld -> [AnyAction]` to `[AnyAction]`
- Update `Env.envActions` correspondingly
- Update `Scene.sceneActions`, `buildActions`, DSL helpers
- Update runtime to filter actions by `actionCondition` at display time
- Update all scenarios and tests
- Tests: all existing playthrough tests pass unchanged

### Step 5: Convert scenario axioms to rules
- Convert each scenario's axioms from `Axiom` to `AxiomRule` where possible
- Remove now-unused `Axiom` instances from scenarios
- Verify with e2e playthrough tests

### Step 6: JSON serialization
- Derive or write `ToJSON`/`FromJSON` for all new types
- Round-trip tests: serialize a scenario's portable data and deserialize it
- Verify the deserialized scenario produces identical behavior

### Step 7: Snapshot merge of scenario data
- Extend `Snapshot` with `snapActions`, `snapRules`, `snapMergeRules`
- Write `mergeActions`, `mergeRules`, `mergeMergeRules` (union-by-ID)
- Add `mergeLocationGraphs` to `mergeWorlds`
- Make `Env` action/rule fields mutable (`IORef`) so they can grow mid-session
- Wire into `offerMerge` and `mkLiveMerge` in `Engine.Runtime`
- Tests: merge two snapshots from different scenarios, verify actions/rules combine correctly

---

## Weirdness inventory

Merge interactions that this design deliberately enables:

| Weirdness | Detection | Narrative response |
|-----------|-----------|-------------------|
| Character at unknown location | Location not in `lgEdges` or `lgRegions` | "Someone is here from a place you've never heard of." |
| Non-adjacent arrival | Character moved between locations with no edge | "They arrived from somewhere that shouldn't connect to here." |
| Conflicting region membership | Same location in different regions across graphs | "This place feels different than you remember." |
| Orphan rules | `AxiomRule` references tags/locations not in receiving scenario | Rule silently doesn't fire (conditions fail) — harmless |
| Missing code axiom | `AxiomId` in handoff has no registered implementation | Warning logged; rule skipped |
| Foreign actions available | Action from another scenario's snapshot becomes choosable | Player sees choices they didn't author — new agency from merge |
| Cross-scenario rule fires | Foreign rule's conditions happen to match local state | Unexpected narrative emerges — bleed between scenarios |
| Disconnected location subgraph | Foreign locations merge in with no edges to local graph | Places exist on your map but you can't walk there |
| Duplicate action ID, different effects | Two scenarios define the same ActionId differently | Left-bias keeps yours; theirs is silently dropped |

These are features, not bugs. Name them, let them become lore.
