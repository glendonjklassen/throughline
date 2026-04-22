# Steam release — technical / polish / usability audit

Scope: what throughline needs to ship on Steam as a paid product. Business
side (storefront page, pricing, marketing, contracts, age rating) is
excluded.

Ordered roughly by "can't ship without this" → "nice to have." Items
marked **MUST** block a release; **SHOULD** matter for reviews and
returns; **NICE** are post-release upgrades.

---

## 1. Packaging and platform builds  **MUST**

Today the project builds via `stack build` on a dev machine. Steam
needs standalone distributable bundles that a user can install from
their library with no toolchain setup.

- **Windows (x86_64)** — `.exe` produced via Stack on a Windows host
  or cross-compilation via MSYS2. SDL2 + SDL2_ttf DLLs bundled next
  to the executable. Installer or zip-to-a-folder acceptable; Steam
  handles extraction.
- **Linux (x86_64)** — a native Linux binary in an `install/` folder
  with SDL2/SDL2_ttf shared libs alongside. Test on Ubuntu LTS and
  SteamOS 3 (Arch-based) specifically; those are the two
  realistic distros. A `run.sh` wrapper that sets `LD_LIBRARY_PATH`
  relative to the binary is standard.
- **macOS (arm64 + x86_64)** — universal `.app` bundle with signed
  binaries and notarized package, or at minimum a signed binary so
  Gatekeeper doesn't quarantine it. macOS is the most labor-intensive
  target; can defer if launch targets Windows/Linux only.
- **Steam Deck verification** — aim for "Playable" or "Verified."
  SteamOS runs the Linux build via the compositor. Key checklist is
  in §2 below.

**Work**: set up CI builds for each platform (GitHub Actions
matrices), produce release artifacts automatically. Haskell builds
are slow on Windows; plan for ~30 min builds. Sign macOS binary
(~$100/yr Apple Developer cert). Windows code signing is optional
but reduces SmartScreen warnings (~$200/yr).

---

## 2. Steam Deck + controller support  **MUST**

The Deck is the single biggest Steam vector for a small indie today.
Deck owners will try your game; if controls don't work you get an
instant refund.

- **Steam Input API** — map controller bindings through Steam's
  input system so players can rebind without code changes. The
  current letter-key scheme (qwerty row for movement, asdfghjkl for
  general) needs a controller equivalent: D-pad for movement options,
  face buttons cycling through general actions, right stick or
  triggers for less-used keys, a dedicated button for journal.
- **On-screen keyboard invocation** — Deck users can't easily type;
  if any action requires text input (currently none, good), route it
  through Steam's OSK.
- **Font legibility at 1280×800** — Deck screen is small; the current
  layout is designed for a larger terminal. Test reading distance of
  ~18" and scale the grid accordingly.
- **Battery-friendly frame pacing** — the SDL loop currently uses
  `SDL.waitEventTimeout 33` which is kind. Confirm no busy-wait
  paths. Consider capping to 30fps when idle.

**Work**: Steam Input SDK integration is a few days. Font/layout
testing on real Deck or Deck emulator needed.

---

## 3. Save/load UI  **MUST**

The event log infrastructure is excellent — sessions persist, merges
work, replay is deterministic. But the *user-visible* save UX doesn't
exist yet. A Steam player expects a save slot picker and confidence
that their progress isn't lost.

- **Save slot model** — pick one of: (a) single save per scenario,
  auto-saved, with no slot UI (simplest; matches the event-log
  shape); (b) named slots the player creates (more work, more
  control). Recommend (a) with a "reset this hunt" option.
- **Main menu state** — currently the launcher doesn't show whether
  a scenario has in-progress state. Add "Continue" vs. "New hunt"
  per scenario.
- **Steam Cloud** — point Steam at the `sessions/` directory. Fully
  automatic; Steam syncs per account. Works as long as file layout
  is stable across platforms.
- **Save corruption recovery** — if a log entry is malformed, the
  loader currently errors out. Add a "your save is corrupted,
  rewind to last known-good snapshot?" path so a bad write doesn't
  lose a season.

**Work**: a few days for the menu-level save picker and recovery
path. Steam Cloud config is trivial once the path is stable.

---

## 4. Settings / options menu  **MUST**

No in-game settings exist. Minimum bar for a Steam release:

- **Display**: fullscreen / windowed toggle, window size / scaling
  factor.
- **Text**: font size (2-3 steps), optional high-contrast palette,
  reveal animation speed slider (some players will want to skip
  the per-cell fades entirely).
- **Input**: key rebinding UI. Not strictly required if Steam Input
  covers remap, but PC players expect in-game rebind too.
- **Audio**: master / music / SFX volume (present even if you ship
  with no audio at first, so the UI doesn't have to change later).

**Work**: 3-5 days. The settings UI is its own full-screen overlay;
can reuse the journal overlay's pattern. Persist to a small JSON
file under the OS's standard config dir.

---

## 5. Audio  **SHOULD**

The game currently ships with zero sound. A text-heavy game can get
away with minimal audio but total silence reads as "unfinished" to
most players.

- **Ambient bed** per scene (wind for DeerHunt, diner hum for Diner,
  etc.) — a single looping track, 30-60 seconds, 48kHz. Licensable
  field recordings from Freesound / Splice cover this cheaply.
- **UI sounds**: menu move, selection confirm, journal open/close,
  first-find sparkle chime. Even six crisp sounds transform feel.
- **Beat-tied cues**: shot fired, deer spotted, kill moment, day
  rollover. Sparse — the prose is the main event.
- **SDL2_mixer** binding is available for Haskell. Integration point:
  a sibling of `SDL.FontContext` that manages loaded samples and
  channels.

**Work**: 2-3 weeks including sound design and integration. The
*absence* of audio is more noticeable than its presence on a quiet
game, so prioritize over flashier features.

---

## 6. Accessibility  **SHOULD**

- **Font size / scaling** (covered under §4). Single most important
  accessibility feature.
- **Colorblind palette** — current sparkle/tint uses warm vs. cool
  distinctions. Add a deuteranopia-safe palette option.
- **Dyslexic-friendly font toggle** — ship OpenDyslexic as an
  alternative. Small effort, notable goodwill.
- **Reveal-animation skip persistent** — some players will want
  animations off by default, not skipped per-turn.
- **Screen reader support** — the game is almost entirely text; an
  NVDA / VoiceOver hook that speaks the narration as it renders
  would be a genuinely novel feature for the form. Deep work but a
  natural fit: pipe each `NarrativeMessage` to the OS screen-reader
  API. Consider for v1.1.

---

## 7. Performance + stability  **MUST**

- **Crash handling** — current behavior on an `error` call is to
  throw to terminal. In a bundled app, wrap the main loop in a
  top-level handler that writes a crash report to `sessions/crashes/`
  and displays a friendly error screen. Send nothing to the
  network.
- **Memory profile** — confirm a long hunt (50+ days) doesn't leak.
  `ghc-prof` once. The event log grows unboundedly; consider a
  snapshot-compaction pass when the log exceeds N entries.
- **Startup time** — SDL context creation is ~1 sec on Linux. Acceptable.
  Benchmark on Deck.
- **Window close mid-animation** — already handled via `pollQuit` in
  key loops. Audit that every blocking `awaitKeySDL` respects quit.

---

## 8. Content depth  **SHOULD**

A Steam price tag implies a certain content floor. DeerHunt currently
supports a season of repeating days with rotating weather, discoveries,
and rare events. Honest self-assessment:

- **One scenario is thin.** Top Buy / Diner / Customer exist but are
  barely-playable prototypes compared to DeerHunt. Either develop
  two more scenarios to DeerHunt's depth, or position the release
  around DeerHunt alone and price accordingly (cheap, short).
- **DeerHunt replay value** — per-buck racks, varied weather, rare
  events, and the find catalog give you 10-20 hours before the
  mystery is exhausted. Enough for a $5-10 game, thin for $15+.
- **Catalog completion tail** — right now the catalog has ~25 entries
  total across trees/animals/sign/finds. For a collection mechanic
  to really pull, target 50+. That's scenario-local content work,
  not engineering.
- **Procgen variety** — the map generator produces meaningful
  variation, which holds up. Verify no obvious seed clustering.

**Work**: content-bound. 4-8 weeks of scenario and catalog writing
to reach "this is worth $15."

---

## 9. Onboarding / first-run  **SHOULD**

- **No tutorial exists.** The player gets dropped into the scenario
  menu with no framing. For Deer Hunt specifically, the spatial HUD
  and the letter-key scheme both need explanation.
- **One-page "how to play" screen** reachable from the menu, plus a
  first-time overlay on scenario start that fades out after a few
  turns. Not a handholding wizard — a coaster.
- **Key hints in the HUD** for the first 3-5 turns. Already partially
  there (`1 journal / 2 past / 3 catalog` footer in the overlay);
  extend to the main HUD's action list.

---

## 10. Localization  **NICE**

English-only is fine for launch. The text is the product though, so
non-English is a significant content project. If serious about it:

- Externalize all user-visible strings from scenario modules into a
  lookup table keyed by some id. Currently prose is inlined
  throughout `Narration.hs`, `Actions.hs`, `Axioms.hs` etc. This is
  a real refactor.
- `gettext`-style catalog per language. Translation by humans, not
  MT — the voice is the game.
- Deck has large French/German/Portuguese audiences, but none of
  those feel critical for a narrative game where the prose carries
  everything.

---

## 11. Polish details  **SHOULD**

- **Window icon** (`.ico` on Windows, `.icns` on macOS). Set
  `SDL.setWindowIcon` on init.
- **Splash screen** with title treatment and version. 1-2 seconds.
- **Credits screen**, even if it's just you and the font. Accessible
  from the main menu.
- **Version info** visible in the menu (`v1.0.0`, build hash
  optional). Helps with bug reports.
- **Quit confirmation** if mid-hunt. One keystroke quit is fine for
  dev; production should ask.
- **Cursor behavior** — game is keyboard-first; decide whether the
  mouse is hidden over the window or remains visible.
- **No debug keys in release** — currently F3 cycles debug overlays.
  Keep for dev builds; compile out or hide in release.

---

## 12. Steam-specific integrations  **NICE**

- **Achievements** — 15-30 is typical. First-find of each
  discovery category, first kill, first buck with 10-point rack,
  first week afield, first rusty-car find. All map cleanly to
  existing tags.
- **Rich Presence** — "On day 7 in the south bush." Tiny integration;
  shows in friends list.
- **Screenshot hotkey** — Steam handles this automatically, but
  verify the overlay doesn't fight SDL's.
- **Steam Workshop / UGC** — way too early. Player-made scenarios
  would be amazing, but the scenario-serialization work (your
  proposal `shared-universe.md`) has to land first.

---

## 13. Known architectural risks for a public release

- **JSON event log is not forward-compatible**. Add a version field
  to log entries now. When schema evolves mid-season, players who
  updated mid-hunt shouldn't lose their log. Write a migration
  pipeline.
- **UUID dependency on `/dev/urandom`** — verify Windows build
  behaves. `Data.UUID.V4.nextRandom` should be fine but exercise it.
- **`error` calls scattered through engine code** (guard clauses in
  `Effects.hs`, unreachable pattern branches, etc.). For a release,
  convert the user-reachable ones to graceful failures.
- **No input validation on loaded logs**. A corrupted or malicious
  `events.jsonl` could crash or misbehave. For local save files
  this is low-stakes; for shared-universe merges from foreign logs,
  it's a real concern. Scope to local-only for v1.
- **No autosave cadence**. Today every action writes to the log
  (effectively autosave), which is great. Just document it so
  players know closing the window doesn't lose their hunt.

---

## Prioritized release roadmap

**Ship-blocking (MUST):** cross-platform packaging + installer (§1),
Steam Deck and controller mapping (§2), save/load UX (§3), settings
menu (§4), crash handling + log versioning (§7, §13).

**Strongly recommended before launch (SHOULD):** basic audio (§5),
accessibility minimum — font scaling + colorblind palette (§6),
onboarding (§9), polish items (§11), content depth audit (§8).

**Post-launch (NICE):** screen reader integration (§6), achievements
(§12), localization (§10), Workshop/UGC.

Realistic minimum timeline from where the code is today to a Steam
launch: **3-4 months focused work** if you're solo, plus content and
audio time tracked separately.
