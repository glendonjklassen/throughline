# Deer Hunt Enrichment

Five systems that deepen the hunting experience by making the landscape, the deer, and the player's knowledge of both feel real.

## 1. Wind

### What it does

Wind has a **direction** (continuous angle in degrees, 0–360, where 0° = north, 90° = east) and **strength** (0.0–1.0 continuous scale). Direction drifts smoothly through the day. The deer's primary detection sense is smell — wind direction determines whether your scent reaches it.

### How it works

**State representation**: Wind is stored as two values in the scenario state:

- `windAngle :: Double` — degrees, 0–360. The direction the wind is blowing **toward** (i.e., wind at 90° blows eastward, carrying scent east).
- `windStrength :: Double` — 0.0 (dead calm) to 1.0 (strong wind). Scales how much direction matters.

These are stored as scenario tags encoding the current values. Since tags are discrete, encode as integer hundredths: `WindAngle 2735` = 273.5°, `WindStrength 65` = 0.65. The axiom reads and writes these each tick.

**Wind axiom** (new, priority 1): Each tick (5 minutes), the wind drifts:

- **Direction**: `newAngle = oldAngle + drift`, where `drift` is a small random value (normally distributed, ~±2° per tick, so ~±24° per hour). Occasionally larger gusts shift 10–20° in a single tick. Over 12 hours of hunting, the wind might rotate 90–180° total, with periods of stability and sudden shifts.
- **Strength**: drifts similarly, biased by weather. "Windy" weather pushes strength toward 0.8–1.0. "Light Snow" pushes toward 0.3–0.5. "Clear and Cold" morning pushes toward 0.1–0.2 (calm dawn), rising to 0.4–0.5 by midday.
- **Weather changes** force a larger direction shift (30–60° random) and snap strength toward the weather's bias. This gives the player a reason to pay attention to weather transitions.

**Integration with spook axiom**: Every location has an `(x, y)` coordinate on the section (see coordinate map below). The wind angle produces a unit vector: `windVec = (sin(angle°), cos(angle°))`. The spook check computes the **dot product** between the wind vector and the (player → deer) vector:

```
playerToDeer = normalize (deerCoords - playerCoords)
windVec      = (sin (windAngle * pi/180), cos (windAngle * pi/180))
alignment    = dot playerToDeer windVec
```

The alignment is a continuous value from -1.0 (perfectly downwind) to +1.0 (perfectly upwind). The wind modifier is a **continuous function** of alignment and strength, not a bucketed lookup:

```
windModifier = 1.0 + alignment * windStrength
-- alignment +1.0, strength 1.0 → modifier 2.0 (upwind, strong: doubled)
-- alignment  0.0, any strength → modifier 1.0 (crosswind: no effect)
-- alignment -1.0, strength 1.0 → modifier 0.0 (perfectly downwind, strong: no scent)
-- alignment +0.5, strength 0.4 → modifier 1.2 (partially upwind, light: slight penalty)
```

Clamped to `[0.1, 2.0]` — even perfectly downwind in strong wind, there's a small residual chance. Even perfectly upwind in calm air, the effect is mild.

**Player information**: The player sees wind narrated in human terms. The continuous angle maps to the nearest cardinal/intercardinal label for narration:

- "Wind out of the northwest. Steady."
- "Wind shifting. Swinging around to the south."
- "Barely any wind this morning. Could come from anywhere."

The `lookForDeer` action uses the player-to-deer alignment to give intuitive feedback when the deer is nearby:
- "Wind's in your face. Good." (alignment < -0.3)
- "Wind's at your back. It'll smell you." (alignment > 0.3)
- "Wind's crossing. You might be okay." (|alignment| < 0.3)

Wind strength scales the modifier: calm = alignment ignored (no modifier), light = modifier at 50%, moderate = modifier at 75%, strong = full modifier.

When player and deer are at the **same location**, use the player's previous location as the approach vector. If the player has been sitting still, they've been there long enough that wind is carrying their settled scent — use a fixed mild upwind penalty (1.3x) regardless of direction.

**Player information**: The player already sees weather narration. Wind direction and strength get folded into that:

- "Wind out of the northwest. Light but steady."
- "Wind shifted. Coming from the south now."
- The `lookForDeer` action can mention wind: "Wind's in your face. Good." or "Wind's at your back. Careful."

### Location coordinate map

Every location gets an `(x, y)` position on the section. The section is 1 mile × 1 mile, so coordinates range from `(0.0, 0.0)` (southwest corner) to `(1.0, 1.0)` (northeast corner). These are spatial facts about where each place sits on the land. Zones are just metadata for deer behavior — they don't define spatial relationships. Coordinates do.

```
locationCoords :: Location -> (Double, Double)
```

**North Road** (along the north boundary, y ≈ 1.0):
| Location | x | y |
|---|---|---|
| truckNorth | 0.40 | 1.00 |
| ditchNorth | 0.40 | 0.97 |

**South Road** (along the south boundary, y ≈ 0.0):
| Location | x | y |
|---|---|---|
| truckSouth | 0.35 | 0.00 |
| ditchSouth | 0.35 | 0.03 |

**West Road** (along the west boundary, x ≈ 0.0):
| Location | x | y |
|---|---|---|
| truckWest | 0.00 | 0.40 |
| ditchWest | 0.03 | 0.40 |

**North Field** (north-center, open stubble between north road and bush):
| Location | x | y |
|---|---|---|
| nFieldEdge | 0.38 | 0.90 |
| stubbleRows | 0.42 | 0.85 |
| hayBale | 0.48 | 0.82 |
| drainageDitch | 0.35 | 0.80 |

**South Field** (southwest, open canola stubble between west road and south road):
| Location | x | y |
|---|---|---|
| sFieldEdge | 0.12 | 0.35 |
| stubbleFlat | 0.18 | 0.28 |
| fenceLine | 0.25 | 0.20 |
| sloughEdge | 0.22 | 0.38 |

**Bush Edge** (center-north, transition from field to forest):
| Location | x | y |
|---|---|---|
| thinPoplars | 0.35 | 0.75 |
| brushPile | 0.40 | 0.72 |
| gameTrailEntrance | 0.45 | 0.70 |
| oldFence | 0.42 | 0.65 |
| clearing | 0.38 | 0.68 |
| deadfall | 0.48 | 0.67 |

**Oak Ridge** (east-center, the ridge running north-south with heavy timber):
| Location | x | y |
|---|---|---|
| ridgeTop | 0.72 | 0.70 |
| oakThicket | 0.68 | 0.65 |
| scrapeLine | 0.65 | 0.60 |
| mossyHollow | 0.62 | 0.55 |
| blowdown | 0.70 | 0.58 |
| deerTrail | 0.60 | 0.65 |

**Willow Bottom** (east-south, low wet ground below the ridge):
| Location | x | y |
|---|---|---|
| cattailMarsh | 0.60 | 0.48 |
| willowTangle | 0.55 | 0.42 |
| creekCrossing | 0.52 | 0.38 |
| mudFlat | 0.58 | 0.35 |
| beaverDam | 0.62 | 0.32 |
| dryHummock | 0.65 | 0.40 |

**Poplar Stand** (south-center, travel corridor connecting bush edge to willow bottom):
| Location | x | y |
|---|---|---|
| poplarAlley | 0.38 | 0.55 |
| birchClump | 0.42 | 0.50 |
| rubLine | 0.48 | 0.45 |
| openUnderstory | 0.45 | 0.52 |
| gameTrailFork | 0.40 | 0.58 |
| windbreak | 0.32 | 0.45 |

These coordinates should be validated against the adjacency graph — adjacent locations should generally be within ~0.15 of each other, and cross-zone edges should connect locations that are spatially close. The coordinates also imply that some adjacency edges represent longer walks than others, which could be useful for future systems (travel time, sound propagation, shot distance).

### Why this matters for gameplay

Wind creates a **planning layer on top of movement**. You find fresh sign at the scrape line (0.65, 0.60 — east side of the section). The wind is from the west. If you approach from the bush edge (0.45, 0.70 — west and north of the deer), your scent blows straight to it. You need to loop around — come in from the willow bottom to the south, or from the ridge top above, keeping the wind in your face. The geometry is real, not a heuristic.

### Files touched

- `Constants.hs`: new `DeerHuntTag` variants for `WindAngle` and `WindStrength` (integer-encoded), wind-related salts, initial wind state in `initialWorld`
- `Axioms.hs`: new `windAxiom` (drift + weather interaction), modified `spookAxiom` to read wind state and compute dot product
- `Probability.hs`: new `windSpookModifier :: Double -> Double -> Double` (alignment, strength → multiplier 0.1–2.0), `windAlignment :: Location -> Location -> Double -> Double` pure geometry helper (playerLoc, deerLoc, windAngle → alignment -1.0 to +1.0)
- `Actions.hs`: wind-aware narration variants in `lookForDeer` and movement narration, cardinal label helper for narrating angle
- `Locations.hs`: populate `lgCoords` on the scenario's `LocationGraph` with all location coordinates (moved from standalone function to graph data, see system 5)

### Tests

- Wind drift: verify angle changes each tick within expected bounds, stays in 0–360 range
- Weather interaction: "Windy" weather forces larger direction shift and pushes strength toward 0.8+
- Wind modifier math: verify `windSpookModifier` is continuous — alignment 1.0/strength 1.0 → 2.0, alignment 0.0 → 1.0, alignment -1.0/strength 1.0 → 0.1 (clamped floor)
- Calm conditions: strength near 0 makes alignment irrelevant (modifier ≈ 1.0 regardless of direction)
- Cardinal label: 273.5° → "west", 45° → "northeast", etc.
- Narration: correct "wind in your face" / "wind at your back" based on alignment sign

---

## 2. Terrain properties: noise and visibility

### What it does

Each location gets two static properties: **noise** (how loud your movement is there) and **visibility** (how far you can see / how exposed you are). These affect spook chance and the `lookForDeer` information quality.

### Terrain profiles

| Terrain type | Locations | Noise | Visibility | Why |
|---|---|---|---|---|
| Road | Trucks, ditches | Low | High | Gravel but you're expected; wide open |
| Open field | All field locations | Low | High | Stubble is quiet; nothing blocks sight |
| Field edge | nFieldEdge, sFieldEdge | Low | Medium | Transition zone |
| Thin bush | thinPoplars, clearing, openUnderstory, poplarAlley | Medium | Medium | Some cover, some noise |
| Dense bush | oakThicket, willowTangle, brushPile, deadfall, blowdown | High | Low | Loud underfoot, can't see far |
| Trail | gameTrailEntrance, deerTrail, gameTrailFork, rubLine | Low | Medium | Worn path, quieter footing |
| Wet ground | cattailMarsh, mudFlat, creekCrossing | Medium | Medium | Squelchy but not crunchy |
| Ridge/elevation | ridgeTop | Low | High | Above the canopy, quiet footing |
| Scrape/sign area | scrapeLine, mossyHollow | Medium | Low | Dense, deer-worn but thick |

Represented as a pure function `terrainNoise :: Location -> TerrainNoise` and `terrainVisibility :: Location -> TerrainVisibility` in `Locations.hs`, where `TerrainNoise = Quiet | Moderate | Loud` and `TerrainVisibility = Open | Partial | Dense`.

### Integration with spook axiom

The spook check gains a terrain modifier applied **at the player's current location**:

- **Noise**: Loud terrain adds +0.15 to spook chance. Quiet terrain subtracts -0.05. Moderate is neutral. Only applies when the player is moving (not sitting).
- **Visibility on the deer's side**: If the deer is at an Open location, it sees you coming — spook chance +0.10. If Dense, the deer can't see you coming — spook chance -0.10.

These stack with wind and experience. A veteran hunter moving slowly downwind on a trail into dense bush is very hard to detect. A green hunter moving fast upwind through dry oak leaves is going to spook everything in the zone.

### Integration with weather

Weather modifies terrain noise:

- **Light Snow**: all terrain noise drops one step (Loud → Moderate, Moderate → Quiet, Quiet stays Quiet). Snow muffles everything.
- **Windy**: all terrain noise drops one step (wind covers your sound). This is already hinted at in the existing weather narration: "Good — it covers your noise."
- **Clear and Cold**: frozen leaves are louder. Dense bush noise gets +0.05 extra.

### Integration with lookForDeer

Visibility determines what the player learns from looking:

- **Open** location: can detect deer 2 zones away (see movement at distance), 1 zone away (identify deer), same zone (clear sighting). Current field behavior extends.
- **Partial**: can detect deer in same zone (sign or glimpse), same location (sighting).
- **Dense**: can only detect deer at same location. You won't see it until it's right there.

This creates a real tension the player navigates: **you can see from the field, but the deer can see you too. You can hide in the bush, but you're blind and loud.** The optimal approach becomes: use the field to locate, use trails to approach, set up where the deer's path crosses a quieter zone.

### Files touched

- `Locations.hs`: `data TerrainNoise = Quiet | Moderate | Loud`, `data TerrainVisibility = Open | Partial | Dense`, `terrainNoise :: Location -> TerrainNoise`, `terrainVisibility :: Location -> TerrainVisibility`
- `Probability.hs`: `terrainSpookModifier :: Location -> Location -> Double` (player loc, deer loc → additive modifier)
- `Axioms.hs`: `spookAxiom` reads terrain at both locations
- `Actions.hs`: `lookForDeer` checks visibility at player's location to determine detection range
- `Constants.hs`: weather → terrain interaction (snow/wind reduce noise level)

### Tests

- Terrain classification: verify every location has expected noise/visibility
- Spook modifier: loud+open terrain increases spook, quiet+dense decreases
- Weather interaction: snow reduces effective noise level
- Look range: open terrain lets player detect further than dense terrain

---

## 3. Sitting as a real mechanic

### What it does

Sitting isn't just "pass time." It becomes a **stateful position** with a duration that matters. The longer you sit, the more you fade into the landscape.

### How it works

**New tag**: `SittingTicks` — a scenario tag with an associated count, or simpler: `SittingSince` storing the tick when the player sat down. Actually, simplest: use a counter approach. Add `Sitting` as a DeerHuntTag, and track duration via the axiom.

**Revised approach** — use the existing engine stat system:

Add a new `DeerHuntTag` value `PlayerSitting`. When the player chooses "Sit and wait," `PlayerSitting` tag is added. When the player takes any movement action, it's removed. A new **stillness axiom** increments a scenario stat (or we just compute it from "ticks since PlayerSitting was added" if the diff history supports it).

Simpler: track a `Stillness` stat on the player (0–10 scale). The stillness axiom:

- If `PlayerSitting` is set: `Stillness += 1` per tick (cap at 10)
- If `PlayerSitting` is not set: `Stillness = 0` (immediate reset on any movement)

**Spook integration**: Stillness directly reduces spook chance when the deer walks into the player's location:

| Stillness | Spook modifier | Narrative equivalent |
|---|---|---|
| 0 | +0.0 (base) | Just arrived, standing around |
| 1–2 | -0.02 | Settled in, getting quiet |
| 3–5 | -0.05 | Part of the landscape. Breathing slow. |
| 6–8 | -0.08 | You haven't moved in half an hour. Your scent has settled. |
| 9–10 | -0.10 | You're a stump. The deer walks past you at 20 yards. |

This stacks with wind and terrain, so a patient hunter sitting downwind on a quiet trail for an hour is nearly undetectable. But they're committing to *this spot* — if the deer goes elsewhere, that's an hour burned.

**Narration**: The `sitAndWait` action gains stillness-aware narration pools:

- Low stillness: "You find a spot against a poplar and settle in. Adjust your collar. Try to get comfortable."
- Medium: "Your legs are stiff. A chickadee lands on the branch above you. Doesn't notice you're there."
- High: "You haven't moved in an hour. The cold has worked into your knees. A squirrel runs across your boot."

These aren't internal monologue — they're what's happening around you. The world starts treating you like part of the scenery.

**Shot integration**: Stillness also affects shot accuracy. A player who has been sitting still for 30+ minutes (Stillness 6+) has cold, stiff hands:

- Stillness 0–3: no modifier
- Stillness 4–6: -0.02 accuracy (slight stiffness)
- Stillness 7–10: -0.05 accuracy (hands are numb)

This creates a genuine tension: sitting long improves your encounter odds but slightly worsens your shot. Real hunting tradeoff.

**Sit and wait becomes a toggle**: Like pace, sitting becomes a state you enter and exit rather than a one-off action. "Sit down and wait" / "Stand up and move." This way each tick while sitting naturally fires the stillness axiom.

### Files touched

- `Constants.hs`: `PlayerSitting` added to `DeerHuntTag`, `Stillness` used as a `Capacity` stat (or a separate scenario stat if the engine supports it)
- `Axioms.hs`: new `stillnessAxiom` (priority 4), modified `spookAxiom` reads stillness
- `Probability.hs`: `stillnessSpookModifier :: Int -> Double`, `stillnessShotModifier :: Int -> Double`
- `Actions.hs`: `sitAndWait` becomes a toggle pair with "Stand up" action, narration pools keyed on stillness level

### Design question

Should Stillness be a `Capacity` stat on the player (using `Understanding` as precedent), or a world tag? Using a stat means it's visible in the relationship graph and modifiable by `modifyCharacterStatEffect`. Using a tag sequence (Sitting1, Sitting2, ... Sitting10) is clunkier but keeps it in tag space. **Recommendation**: use a stat. It's a numeric value that changes frequently — that's what stats are for. Call it `Capacity Stillness` or add a new `StatType` variant.

### Tests

- Stillness increments each tick while sitting, resets on movement
- Stillness caps at 10
- Spook chance decreases with high stillness
- Shot accuracy decreases slightly with high stillness
- Narration changes at stillness thresholds

---

## 4. Sign-reading as information

### What it does

Currently `FreshSign` is a binary tag: you're in the deer's zone or you're not. Sign-reading becomes a **multi-type information system** where different kinds of sign tell the player different things about the deer's behavior.

### Sign types

New scenario tags:

| Tag | What it means | When it appears | What the player learns |
|---|---|---|---|
| `SignTracks` | Tracks in the location | Deer passed through this location within the last 2 hours | **Direction**: which adjacent location the deer came from or went to |
| `SignBed` | Matted grass / body impression | Deer was stationary at this location for 3+ ticks | **Timing**: this is a bedding spot, deer uses it midday |
| `SignRub` | Tree rub, antler marks | Deer has visited this location 3+ times total across the hunt | **Routine**: this location is on the deer's regular circuit |
| `SignScrape` | Ground scrape, broken branches | Deer was here within the last hour | **Recency**: the deer is close, very recently departed |
| `FreshSign` | (existing) Generic proximity | Same zone as deer | Kept as-is for zone-level awareness |

### How it works

**Sign-placement axiom** (new, priority 3): Each tick, evaluates the deer's position and history to place sign at appropriate locations.

- **Tracks**: When the deer moves, add `SignTracks` to the location it **left**. Tracks expire after 24 ticks (2 hours). Weather interaction: snow covers old tracks faster (expire in 12 ticks) but fresh tracks in snow are more visible (narration is more specific about direction).
- **Bed**: When the deer has been at the same location for 3+ consecutive ticks, add `SignBed` to that location. Beds don't expire during the hunt — they're physical evidence.
- **Rub**: Track a visit count per location (via an axiom-managed counter or world tag per location). When count >= 3, add `SignRub`. Rubs don't expire — they're permanent marks on trees.
- **Scrape**: When the deer leaves a location, add `SignScrape` to that location. Scrapes expire after 12 ticks (1 hour). More urgent than tracks.

Implementation detail: Track expiry using `timed` effects rather than manual tick counting. `timed 24 (AddWorldTag signTracks_LocationName)` naturally expires. The sign tag encodes the location to avoid ambiguity: `SignTracks_OakThicket` or, cleaner, maintain a `Map Location (Set SignType)` in scenario state. Since the engine uses world tags (not per-location metadata), the simplest approach is compound tags: each sign type + location pair is a distinct tag value, e.g., `scenarioTag (SignAt Tracks oakThicket)`.

**Alternative (simpler)**: Since the player can only observe sign at their *current* location, the axiom only needs to place sign where the player currently is (or has been). When the player enters a location, check the deer's recent history relative to that location and narrate accordingly. This is stateless — no sign tags needed, just axiom logic that checks deer movement history.

**Recommended approach**: Hybrid. Use timed world tags for **tracks** and **scrapes** (they need to expire and persist independently of the player's location). Use axiom-computed checks for **beds** and **rubs** (these are about cumulative behavior, checked when the player looks).

### Integration with lookForDeer

The `lookForDeer` action becomes much richer. Instead of four tiers (spotted / field movement / fresh sign / nothing), it reads the sign at the player's location:

**At a location with SignScrape**: "Ground torn up here. Dirt's still dark — hasn't dried. He was here less than an hour ago. Heading..." + directional hint based on deer's last known movement.

**At a location with SignTracks**: "Tracks. Pointed northeast, so he came from..." + source direction. "Edges are still sharp — recent." or "Edges are crumbling. Older."

**At a location with SignBed**: "Flattened grass in a body-shaped oval. This is where he lays up. Midday, probably."

**At a location with SignRub**: "Bark stripped off this poplar at shoulder height. Antler rub. He comes through here regular."

**Multiple signs at one location**: Narrate the most informative one, or combine: "Rub on the poplar, fresh tracks heading east, and a bed in the grass behind it. This is his spot. He'll be back."

### Integration with experience

Experience (Understanding stat) gates how much the player can read from sign:

| Understanding | What sign reveals |
|---|---|
| 0–2 | Presence only ("Something's been here") — no direction, no timing |
| 3–4 | Type ("Tracks" / "Bed" / "Scrape") + rough timing ("recent" / "old") |
| 5–6 | Type + direction + timing ("Fresh tracks heading east, less than an hour old") |
| 7–8 | Full read: type + direction + timing + pattern inference ("He beds here midday and feeds in the north field at dawn — same route every day") |

This means the Understanding stat isn't just improving dice rolls — it's improving the *quality of information the player receives*. A veteran reads the same sign and gets a prediction. A novice reads it and gets "something was here."

### Experience gains from sign

Finding new sign types for the first time should grant Understanding, replacing the current flat `+1 for FreshSign`:

- First time finding any sign at all: +1 (current behavior, via FreshSign)
- First time finding tracks: +1 (learning to read direction)
- First time finding a bed: +1 (learning deer patterns)
- First time finding a rub: +1 (learning regular routes)
- First time finding a scrape: +1 (learning recency)

This replaces the current uncapped `+1 per FreshSign discovery` with a bounded system where each sign type teaches you something once. Combined with the daily `+1`, Understanding grows from 2 to ~8 over a 2-day hunt if the player actively explores and finds all sign types.

### Files touched

- `Constants.hs`: new `DeerHuntTag` values (`SignTracks`, `SignBed`, `SignRub`, `SignScrape`, and compound tags encoding location if needed), new salts
- `Axioms.hs`: new `signPlacementAxiom` (priority 3), modified `experienceAxiom` for per-type first-discovery bonuses
- `Actions.hs`: `lookForDeer` rewritten with sign-type-aware narration, gated on Understanding level
- `Probability.hs`: possibly a helper for determining direction hints from deer movement history

### Tests

- Sign placement: tracks appear at location deer left, scrapes appear and expire, beds appear after 3+ stationary ticks, rubs appear after 3+ visits
- Sign expiry: tracks gone after 24 ticks, scrapes gone after 12, beds and rubs persist
- Weather interaction: snow halves track lifetime
- Look narration: correct sign type reported at each Understanding tier
- Experience: first discovery of each sign type grants +1 Understanding
- Direction hints: tracks point in correct direction relative to deer's movement

---

## 5. Compass rose (engine feature)

### What it does

When a `LocationGraph` has coordinates, the terminal header displays a small ASCII compass rose showing which directions have exits. Movement stops being a list of names and starts feeling spatial — you know the ridge is east and the field is north without reading every action label.

### Engine change: coordinates on LocationGraph

`Location` stays a newtype over `String`. The spatial data lives on the graph, not the location itself — a location can exist in multiple graphs or have no coordinates at all.

```haskell
data LocationGraph = LocationGraph
  { lgEdges   :: Set (Location, Location)
  , lgRegions :: Map Location Region
  , lgCoords  :: Map Location (Double, Double)   -- new, optional
  }
```

When `lgCoords` is empty, everything works as before — no compass, no directional information. When populated, the engine can derive bearings between any two connected locations:

```haskell
bearing :: (Double, Double) -> (Double, Double) -> Double
bearing (x1, y1) (x2, y2) = atan2 (x2 - x1) (y2 - y1) * 180 / pi
-- Result in degrees, 0° = north, 90° = east
```

Snap to the nearest cardinal/intercardinal:

```haskell
snapToCardinal :: Double -> String
snapToCardinal deg = ["N","NE","E","SE","S","SW","W","NW"] !! round (deg / 45) `mod` 8
```

This is the only engine-level change in the proposal. Everything else remains scenario-level.

### Compass rendering

The compass sits in the status line area of the left panel, next to the existing location/time display. It's a compact single-line rendering showing which cardinal directions have exits:

```
[ The Ridge — Monday — Autumn — Waning Crescent ]
[  W · N · NE  ]
```

Or, if there's room for a small rose (3 lines, fits within the existing `buildStatusPart` structure):

```
        N
    W · + · E
        S
```

Where only directions with actual exits are shown (others are blank or dim). The `+` center could optionally show wind direction with an arrow character when wind data is available — the scenario provides wind angle, the engine renders the arrow. But even without wind, the compass works: it just shows exits.

**Rendering rules:**
- Directions with an exit: bold
- Directions without an exit: dim dot or blank
- If wind angle is available (via a scenario-provided callback or world tag): a small arrow character at center pointing in wind direction (↑ ↗ → ↘ ↓ ↙ ← ↖)
- If no coordinates on the graph: compass is not rendered at all, status line works exactly as it does today

### Why this belongs in the engine

Coordinates are spatial facts about the world — "the ridge is east of the hollow" is the same kind of truth as "the ridge is adjacent to the hollow." The engine already owns adjacency via `lgEdges`. Coordinates are the same data with geometry attached.

Any scenario that populates `lgCoords` gets the compass for free. A diner scenario with three rooms probably doesn't need it. A hunting scenario spread across a square mile does. The feature is opt-in by data presence, not by flag.

### Files touched

- `src/GameTypes/Types.hs`: add `lgCoords` field to `LocationGraph`, update `emptyLocationGraph`
- `src/Engine/Core/World.hs`: new `exitBearings :: CharId -> GameWorld -> [(Location, String, Double)]` helper — given the player's location, returns adjacent locations with their cardinal label and bearing in degrees
- `src/Terminal/Display.hs`: compass rendering in `buildStatusPart` (or a new `buildCompassPart`), conditional on `lgCoords` being non-empty
- `src/Engine/Author/Scene.hs`: `SceneEdge` rendering could optionally prepend cardinal direction to edge labels (e.g., "N — Head to the north field")
- `app/Scenarios/DeerHunt/Locations.hs`: populate `lgCoords` from the existing coordinate map (section 1 already defines all coordinates)

### Tests

- Bearing math: `bearing (0.5, 0.5) (0.5, 1.0)` = 0° (due north), `bearing (0.5, 0.5) (1.0, 0.5)` = 90° (due east)
- Cardinal snapping: 0° → "N", 47° → "NE", 180° → "S", 315° → "NW"
- Empty coords: compass not rendered, status line unchanged
- Exit list: given a location with 3 adjacent locations, returns 3 bearings
- Rendering: compass output contains only directions that have exits

---

## Implementation order

1. **Compass rose + engine coordinates** (system 5) — Engine-level prerequisite. Adds `lgCoords` to `LocationGraph` and compass rendering to the terminal. Small, self-contained engine change. Must land first because wind (system 1) depends on coordinates being on the graph rather than in a standalone scenario function.

2. **Terrain properties** (system 2) — Smallest scenario change. Pure functions on existing locations, modifier plugged into existing spook math. No new axioms, no new tags. Good foundation because systems 1 and 3 also modify spook chance — having terrain in place first means we can test the stacking.

3. **Wind** (system 1) — New axiom + tags, modifier into spook. Reads coordinates from `lgCoords` instead of a scenario-local function. Independent of terrain but stacks with it. Optionally feeds wind direction into the compass center arrow.

4. **Sitting/stillness** (system 3) — New stat + axiom + toggle refactor. Independent of wind and terrain but all three stack in the spook calculation.

5. **Sign-reading** (system 4) — Most complex. New axiom, new tags, `lookForDeer` rewrite, experience rework. Benefits from having the other three systems in place so the full hunt loop is richer when sign-reading comes online.

Each system is independently shippable and testable. The spook chance becomes:

```
windVec       = (sin(windAngle * pi/180), cos(windAngle * pi/180))
alignment     = dot (normalize (deerCoords - playerCoords)) windVec  -- -1.0 to +1.0
windMult      = clamp 0.1 2.0 (1.0 + alignment * windStrength)      -- continuous
terrainAdd    = terrainNoiseModifier(playerLoc, weather)             -- -0.05 to +0.15
visAdd        = terrainVisibilityModifier(deerLoc)                   -- -0.10 to +0.10
stillnessAdd  = stillnessModifier(stillness)                         -- -0.10 to 0.0

finalSpookChance = max 0.02 $
    baseSpookChance(experience, pace)
  * windMult
  + terrainAdd + visAdd + stillnessAdd
```

The `0.02` floor ensures there's always some risk. The wind modifier is fully continuous — no buckets, no snapping. A 47° angle at 0.63 strength produces a different result than a 48° angle at 0.64 strength.

## Scope

One engine change: adding optional `lgCoords` to `LocationGraph` and compass rendering to the terminal. This is additive — graphs without coordinates behave exactly as before. All other systems (wind, terrain, stillness, sign-reading) remain scenario-level: new `DeerHuntTag` variants, new axioms, modified probability functions, richer narration. The coordinate map from section 1 moves from a standalone scenario function into `lgCoords` on the graph, which both the compass and the wind system read from.
