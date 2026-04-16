# Proposal: Event Parent Pointers (Lightweight Causal Tracking)

## Problem

The engine currently uses a LamportClock (tick + PlayerId) for ordering. This is sufficient for log replay and CRDT merge, but it can't answer: **"did player A know about player B's state when they acted?"**

That question is narratively interesting ("they acted without knowing") and mechanically useful for detecting existence conflicts and information asymmetry in the shared universe. Full vector clocks don't scale. We want something lightweight.

## Design principle: author-first

The plumbing exists to serve the authoring surface. A scenario author should never touch log entries, frontiers, or entry IDs. They think in characters, tags, stats, and diffs — the same vocabulary as everything else.

## What the author sees

### MergeDiff: what arrived from elsewhere

After a merge, the engine computes a `MergeDiff` — what changed as a result of absorbing foreign state. This is structurally similar to `WorldDiff` but carries **provenance**: who caused each change, and whether they knew about our state when they did.

```haskell
data Provenance = Aware | Unaware | Stale
  deriving (Show, Eq, Ord, Generic)

data MergeDelta a = MergeDelta
  { mdValue      :: a           -- the delta itself (same shape as WorldDiff fields)
  , mdOrigin     :: PlayerId    -- who caused this
  , mdProvenance :: Provenance  -- did they know about us?
  } deriving (Show, Eq, Generic)

data MergeDiff = MergeDiff
  { mergeStats     :: [MergeDelta StatDelta]
  , mergeRelations :: [MergeDelta RelationDelta]
  , mergeTags      :: [MergeDelta (CharId, Tag)]
  , mergeWorldTags :: [MergeDelta Tag]
  , mergeLocations :: [MergeDelta LocationDelta]
  } deriving (Show, Eq, Generic)
```

### Merge axioms: a new axiom type

Regular axioms fire every tick: `GameWorld -> [AnyAction] -> WorldDiff -> [Effect]`.

Merge axioms fire once per merge: `GameWorld -> MergeDiff -> [Effect]`.

```haskell
data MergeAxiom = MergeAxiom
  { mergeAxiomId       :: AxiomId
  , mergeAxiomPriority :: Int
  , mergeAxiomEvaluate :: GameWorld -> MergeDiff -> [Effect]
  }
```

### DSL helpers for scenario authors

```haskell
-- Filter a MergeDiff to only unaware changes
unawareChanges :: MergeDiff -> MergeDiff

-- Did any stat/relation change arrive from someone unaware of us?
hasUnawareRelation :: CharId -> CharId -> StatType -> MergeDiff -> Bool

-- Did a character arrive at a location without knowing we were there?
hasUnawareArrival :: CharId -> Location -> MergeDiff -> Bool

-- Get all provenance-tagged deltas for a specific relation
mergeRelationDeltas :: CharId -> CharId -> StatType -> MergeDiff -> [MergeDelta RelationDelta]

-- Convenience: fire effects only when unaware state arrives
whenUnaware :: MergeDiff -> [Effect] -> [Effect]
whenUnaware md effs
  | any ((== Unaware) . mdProvenance) (mergeRelations md) = effs
  | otherwise = []
```

### What axioms look like

These are engine-level system axioms — they apply universally, not to any specific scenario.

```haskell
-- "A relationship changed from a timeline that didn't know about ours"
divergentRelationAxiom :: MergeAxiom
divergentRelationAxiom = MergeAxiom
  { mergeAxiomId       = SystemAxiom "divergentRelation"
  , mergeAxiomPriority = 5
  , mergeAxiomEvaluate = \_world md ->
      let unaware = [d | d <- mergeRelations md, mdProvenance d == Unaware]
      in if null unaware then []
         else [ immediate (Narrate "Something feels doubled — like two conversations wrote over each other.") ]
  }

-- "A character arrived at our location from a timeline that didn't know we were here"
strangerArrivalAxiom :: MergeAxiom
strangerArrivalAxiom = MergeAxiom
  { mergeAxiomId       = SystemAxiom "strangerArrival"
  , mergeAxiomPriority = 3
  , mergeAxiomEvaluate = \_world md ->
      let arrivals = [d | d <- mergeLocations md, mdProvenance d == Unaware]
      in if null arrivals then []
         else [ immediate (Narrate "Someone is here who wasn't before. You don't remember them arriving.") ]
  }

-- "A world tag appeared that we never set — foreign state bled in"
foreignStateAxiom :: MergeAxiom
foreignStateAxiom = MergeAxiom
  { mergeAxiomId       = SystemAxiom "foreignState"
  , mergeAxiomPriority = 4
  , mergeAxiomEvaluate = \_world md ->
      let foreign = [d | d <- mergeWorldTags md, mdProvenance d == Unaware]
      in if null foreign then []
         else [ immediate (Narrate "The world shifted. Something changed that you didn't cause.") ]
  }
```

The author never mentions logs, frontiers, or entry IDs. They write conditions over `MergeDiff` the same way they write conditions over `WorldDiff`. Scenario authors can add their own `MergeAxiom`s for scenario-specific reactions to merge events.

### Engine-level system merge axioms

The engine itself can ship merge axioms that handle universal weirdness:

```haskell
-- Auto-tag characters who arrived via unaware merge
foreignPresenceAxiom :: MergeAxiom
-- Tags characters with EngineTag (ForeignOrigin playerId) when they
-- arrive from an unaware source. Other axioms and conditions can
-- check for this tag.
```

This means the `Condition` system can gate on foreign-origin state:

```haskell
-- A new EngineTag
| ForeignOrigin PlayerId

-- A new Condition (or just use HasTag with the above)
-- "This character has state from a timeline that didn't know about us"
HasTag frankId (EngineTag (ForeignOrigin somePlayer))
```

## Plumbing (invisible to authors)

### CausalFrontier

```haskell
type CausalFrontier = Map PlayerId String  -- PlayerId -> last seen entryId
```

Stored on `LogEntry` as `entryFrontier :: CausalFrontier`. Updated only at sync time. Stamped onto every log entry (cheaply — it only changes at sync boundaries).

### Frontier tracking

Lives in `Env` as `envFrontier :: IORef CausalFrontier`. Updated during merge in `Engine.Sync.EventLog`. When player A merges player B's log:

1. Record B's latest entry ID in the frontier
2. A's next log entry carries the updated frontier

### Provenance computation

During merge, the engine compares the foreign log's frontier against our own log to determine `Provenance` for each delta:

```haskell
computeProvenance :: CausalFrontier  -- theirs, at the time of their entry
                  -> PlayerId        -- us
                  -> [LogEntry]      -- our log
                  -> Provenance
computeProvenance theirFrontier us ourLog =
  case Map.lookup us theirFrontier of
    Nothing -> Unaware          -- they never synced with us
    Just eid
      | eid >= lastEntryId ourLog -> Aware  -- they saw our latest
      | otherwise                 -> Stale  -- they saw some of our state
```

This runs once per merge, not per tick. The `MergeDiff` it produces is then passed to merge axioms.

### Merge axiom execution

After `mergeWorlds` and `applyWorldDiff`, the runtime:

1. Computes `MergeDiff` by diffing pre/post merge world + annotating with provenance
2. Runs all `MergeAxiom`s against the merged world + `MergeDiff`
3. Executes their effects (same as regular axiom effect execution)

## Scenario registration

```haskell
data Scenario = Scenario
  { scenarioName         :: String
  , scenarioInitial      :: GameWorld
  , scenarioActions      :: GameWorld -> [AnyAction]
  , scenarioAxioms       :: [Axiom]
  , scenarioMergeAxioms  :: [MergeAxiom]      -- NEW
  , scenarioTerminal     :: Condition
  , scenarioDebugDefault :: DebugMode
  , scenarioPlayerCharId :: CharId
  }
```

## Summary of layers

| Layer | Sees | Doesn't see |
|-------|------|-------------|
| Scenario author | `MergeDiff`, `Provenance`, DSL helpers, `MergeAxiom` | Logs, frontiers, entry IDs |
| Engine merge axioms | Same as scenario + `EngineTag ForeignOrigin` | Raw frontier data |
| Sync plumbing | `CausalFrontier`, `LogEntry.entryFrontier`, provenance computation | Nothing hidden |

## Scope

### In scope
- `CausalFrontier` type, `entryFrontier` field on `LogEntry`
- Frontier tracking in `Env`, updated at sync time
- `MergeDiff` and `MergeDelta` types with `Provenance`
- `MergeAxiom` type and execution in runtime
- `scenarioMergeAxioms` field on `Scenario`
- DSL helpers: `whenUnaware`, `hasUnawareRelation`, `hasUnawareArrival`, `unawareChanges`
- `ForeignOrigin` engine tag
- Provenance computation from frontier comparison
- JSON serialization for new types
- Tests for frontier tracking, provenance computation, and merge axiom execution
- Augment existing merge tests (two-player merge, N-player merge, co-location snapshot/log/agreement) to verify provenance on each merge path
- Extend co-location timing tests (customer present vs. left-before-merge) to verify provenance reflects merge timing: `Aware` when the other player's state was current at sync, `Unaware` when they acted independently before sync occurred. Snapshot and log paths must agree on provenance.

### Not in scope
- Transitive causality resolution (resolving `Stale` → `Aware`/`Unaware`)
- UI for displaying causal information
- Specific narrative content for system weirdness (authored per-scenario)

## Cost

- **Storage**: one `Map PlayerId String` per log entry (changes infrequently)
- **Merge-time**: one provenance computation per foreign delta, one pass of merge axioms
- **Per-tick**: zero additional cost (merge axioms don't run on normal ticks)
