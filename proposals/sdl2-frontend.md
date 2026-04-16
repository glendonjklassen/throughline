# SDL2 Frontend

A 1:1 port of the terminal UI to an SDL2 window, keeping the same split-panel text layout, typewriter animation, breathing prompt, and glitch effects. The terminal frontend stays as a runtime flag. The SDL2 window is a fixed-size monospace text grid rendered via an embedded TTF font.

## Why

The terminal works. But it's a ceiling. ANSI escape codes can't do:
- Smooth per-character color transitions (gradients on narration text as tension builds)
- Layered rendering (weather particles drifting behind text, fog overlays)
- Pixel-positioned elements (a compass rose that isn't just text)
- Custom glyph rendering (terrain symbols, wind arrows, shot trajectory)
- Controlled animation timing (the typewriter and glitch effects fight with terminal buffering)

This port changes none of that yet. It reproduces exactly what's on screen today, but in a context where all of the above becomes possible later.

## Scope

### What changes
- New module tree: `src/SDL/` with renderer, input handler, font management
- New `RuntimeUI` implementation: `sdlUI :: ScenarioDisplay -> RuntimeUI`
- `app/Main.hs`: `--terminal` flag selects head; SDL2 is default
- `package.yaml` / `stack.yaml`: add `sdl2`, `sdl2-ttf` dependencies
- Embed one TTF font file in `assets/`

### What doesn't change
- `src/Engine/` — untouched
- `src/GameTypes/` — untouched
- `app/Scenarios/` — untouched
- `src/Terminal/` — untouched, still works via `--terminal`
- `RuntimeUI` interface — same 7-field record
- `Env`, `App` monad, `coreLoop` — same
- All tests — same (they use headless mode, not the UI)

## Architecture

```
app/Main.hs
  |
  +-- --terminal flag --> Terminal.Runner.terminalUI  (existing)
  |
  +-- default ----------> SDL.Runner.sdlUI           (new)
                            |
                            +-- SDL.Renderer   (text grid, split panels)
                            +-- SDL.Font       (TTF loading, glyph cache)
                            +-- SDL.Input      (key events -> action selection)
                            +-- SDL.Animation  (typewriter, pulse, glitch)
```

### Module breakdown

**SDL.Runner** (~50 lines)
- `sdlUI :: ScenarioDisplay -> RuntimeUI`
- Mirrors `Terminal.Runner.terminalUI` — constructs the RuntimeUI record
- `uiSetup`: init SDL2, create window (fixed size), load font, build glyph cache
- `uiTeardown`: destroy window, free font, quit SDL2
- `uiGameLoop`: runs the App monad with SDL-based rendering and input

**SDL.Font** (~80 lines)
- Load embedded TTF at a fixed point size
- `data GlyphCache` — pre-rendered texture for each printable ASCII character in each color
- `renderChar :: GlyphCache -> Char -> Color -> (Int, Int) -> IO ()` — blit one glyph
- `renderString :: GlyphCache -> String -> Color -> (Int, Int) -> IO ()` — blit a string
- `cellSize :: GlyphCache -> (Int, Int)` — character cell dimensions in pixels

**SDL.Renderer** (~200 lines)
- `renderWorld` — the SDL equivalent of `Terminal.Render.renderWorld`
- Same structure: build left pane lines, build right pane lines, zip and render
- Uses `Terminal.Display.buildStatusPart`, `buildHistoryLines`, `buildCompassString` — these are pure functions that return `[String]`, not ANSI-dependent
- Strips ANSI codes from strings and interprets color functions as SDL colors
- `renderSplitRow` — draws left cell, separator, right cell as positioned text

**SDL.Input** (~60 lines)
- Polls SDL events in a loop
- Maps SDL key events to the same character set: digits 1-9, 'q', 'd', 'm'
- Returns `Maybe AnyAction` exactly like `Terminal.Render.awaitKey`
- Window close event maps to quit

**SDL.Animation** (~120 lines)
- `typewriteLine` — renders text character by character with per-character delay, checks for key interrupt via SDL event poll
- `breathingPulse` — sine-wave brightness on the prompt line, same 3.2s cycle
- `glitchFrame` — corrupts random cell positions, renders glitched frame for 80ms, then clean frame
- All use SDL's timing (`SDL.delay`, `SDL.ticks`) instead of `threadDelay`

### Color mapping

The terminal uses 7 named colors (grey, green, yellow, red, cyan, bold, dim) plus ANSI 256-color for tension gradients and the breathing pulse. The SDL renderer maps these to RGB values:

The palette is late-autumn prairie — cold sky, dry grass, bare wood. Not fantasy, not neon. The kind of colors you'd see from a truck window at 6:45 AM in November.

**Base tones:**

| Role | Hex | RGB | What it feels like |
|---|---|---|---|
| Background | `#141411` | (20, 20, 17) | Not pure black. Soil dark, slightly warm. |
| Default text (`bold`) | `#D4CCBA` | (212, 204, 186) | Parchment. Warm white, never blue. |
| Dim text | `#5C5849` | (92, 88, 73) | Dried mud. Readable but receding. |
| Grey (UI chrome) | `#8A8475` | (138, 132, 117) | Weathered fence post. |

**Semantic colors:**

| Role | Hex | RGB | When it appears |
|---|---|---|---|
| Narrator (low tension) | `#7D8C6A` | (125, 140, 106) | Sage. Calm narration, early morning. |
| Narrator (mid tension) | `#B5A44E` | (181, 164, 78) | Dry grass in afternoon light. Things are happening. |
| Narrator (high tension) | `#C47A3A` | (196, 122, 58) | Rust. Late sun on metal. Buck fever. |
| Narrator (max tension) | `#5C5849` | (92, 88, 73) | Dims out. Tunnel vision. |
| Dialogue (`cyan`) | `#8FAFA7` | (143, 175, 167) | Lichen on poplar bark. Slightly blue-green. |
| Internal thought (`dim`) | `#6B6558` | (107, 101, 88) | Quieter than grey. Almost subconscious. |
| Warning | `#C4943A` | (196, 148, 58) | Amber. Not screaming, just firm. |
| Error | `#A65D5D` | (166, 93, 93) | Dried blood on snow. Muted, not bright. |

**Tension gradient** (narrator color across 0-10):

| Tension | Hex | Feel |
|---|---|---|
| 0-2 | `#7D8C6A` | Sage. Quiet bush. |
| 3-4 | `#9A9A5A` | Yellow-green. Something's close. |
| 5-6 | `#B5A44E` | Dry grass. Fresh sign. Heartrate up. |
| 7-8 | `#C47A3A` | Rust. Antlers in the scope. |
| 9-10 | `#5C5849` | Dim. Post-shot. Everything narrows. |

**Breathing pulse:**
- Oscillates between `#5C5849` (dim) and `#D4CCBA` (parchment)
- Same sine wave timing (3.2s cycle)

**Separator:**
- `#3D3A33` — barely visible vertical line between panes. Structural, not decorative.

**Glitch characters:**
- Flash in `#C47A3A` (rust) at tension 4-6
- Flash in `#A65D5D` (dried blood) at tension 7+
- Background stays `#141411`

The palette is intentionally desaturated. Nothing glows. The tension gradient works by warming up — sage to grass to rust to nothing — like the light changing through a day in the field.

### Font

**JetBrains Mono Regular** (~200KB, SIL Open Font License — free for any use). Legible at small sizes, clean at grid scale, no ligatures needed.

### Window geometry

Fixed window: **1280 x 800 pixels**.

At 16px font size with JetBrains Mono:
- Cell size: ~10 x 20 pixels
- Grid: 128 columns x 40 rows
- Left pane: ~86 columns (67%)
- Right pane: ~38 columns
- Separator: 4 columns

This matches the current terminal layout on a typical 120-column terminal. The numbers are computed at startup from the actual glyph metrics, not hardcoded.

## Implementation order

1. **Dependencies + window** — Add sdl2/sdl2-ttf to stack, open a black window, render "Hello throughline" in the embedded font. Verify WSLg works. (~30 min agent time)

2. **Font + glyph cache** — Load TTF, build ASCII glyph cache, render colored text at grid positions. (~30 min)

3. **Static render** — Port `renderWorld` to SDL. Build left pane, right pane, separator. Render a frozen frame from a test world. No input, no animation. (~1 hour)

4. **Input loop** — Poll SDL events, map keys to actions, wire into `coreLoop` via `ActionSource`. Game is now playable but no animation. (~30 min)

5. **Typewriter** — Port character-by-character rendering with SDL timing. (~30 min)

6. **Breathing pulse** — Sine-wave brightness on prompt text. (~15 min)

7. **Glitch effect** — Random cell corruption at high tension. (~15 min)

8. **Debug overlay** — Learning mode lines in left pane. (~15 min)

9. **End screen + merge prompts** — Port `uiOnEnd`, `uiPromptMerge`. (~15 min)

10. **Main.hs flag** — `--terminal` selects terminal head, default is SDL2. (~10 min)

Total agent time: ~4-5 hours across a few sessions. Your time: choose a font, tune the color palette, decide if the window size feels right.

## What this enables later (not in scope now)

Once the SDL2 head exists, future proposals can add:
- Weather particle layer behind text (snow falling, wind streaks)
- Smooth color gradients on narration text as tension shifts
- A small ASCII-art compass rose rendered with custom positioning
- Terrain-aware background tints (darker in dense bush, lighter in fields)
- Shot sequence animation (muzzle flash overlay, screen shake)
- A minimap or location indicator (opt-in per scenario)
- Sound via SDL2_mixer (wind, footsteps, bolt action)
- Resolution-independent scaling for different monitors

None of these require engine changes. They're all presentation-layer additions to the SDL renderer.

## Decisions (confirmed)

- **Font**: JetBrains Mono Regular
- **Window title**: "Throughline"
- **Color palette**: Late-autumn prairie (see palette section above)

## Risk

- **WSLg rendering**: SDL2 over WSLg occasionally has frame tearing or input lag. If this is bad, the mitigation is building a native Windows `.exe` later — the SDL2 code is portable, only the build toolchain changes.
- **sdl2-ttf Haskell binding maturity**: The `sdl2-ttf` package is maintained but thin. If it's flaky, fallback is `sdl2-image` with a pre-rendered bitmap font sheet (same visual result, different loading code).
- **Font licensing**: All three suggestions are SIL OFL — free for bundling, modification, and redistribution. No risk.
