-- | Audio backend interface.
--
-- The real SDL2_mixer integration is deferred — a Haskell binding
-- exists ('sdl2-mixer'), but it's a chunk of integration work and
-- needs sound assets, neither of which the engine iteration needs
-- right now.  What we DO need, to unblock everything that depends on
-- audio (UI click sounds, ambient beds per scene, beat-tied cues
-- like a fired shot), is the *interface* — a single place the
-- runner can tell the audio layer "this happened," and a silent
-- default that no-ops so existing builds keep working.
--
-- When the real backend lands, it constructs an 'AudioBackend'
-- record with real implementations and hands it to whoever owns the
-- audio handle.  Every emission site already exists.
module SDL.AudioBackend
  ( AudioEvent(..)
  , AudioBackend(..)
  , silentBackend
  , playEvent
  ) where

-- | A discrete thing the audio layer might want to react to.
-- Deliberately low-level: scenarios emit 'NarrativeMessage' values
-- today, and the runner maps those to 'AudioEvent' before dispatch.
-- Keeping the event set small makes it obvious what a scenario can
-- actually trigger.
data AudioEvent
  = -- | The player selected an option from the HUD.
    UiSelect
    -- | A menu move or tab switch — quieter than a confirm.
  | UiMove
    -- | A journal open or close.  Distinct so the sound designer can
    -- choose whether it reuses 'UiMove' or gets its own crinkle.
  | UiJournal
    -- | First-time reveal of a catalog entry.  Intended to be the
    -- one "delight" cue the player hears on discovery.
  | Discovery
    -- | A beat-tied gameplay event.  The 'String' lets scenarios
    -- pick their own cue keys ("shot", "spot", "nightfall") without
    -- the audio module needing to know which scenarios exist.  The
    -- backend maps unknown keys to silence.
  | Gameplay !String
  deriving (Show, Eq)

-- | All the functions the runner needs to drive audio.  Holding this
-- as a record (instead of a typeclass) keeps the backend swap a
-- runtime decision — handy for tests and for the silent default.
data AudioBackend = AudioBackend
  { abPlay    :: AudioEvent -> IO ()
  , abStopAll :: IO ()
    -- | Update master / music / SFX gains live.  The runner calls
    -- this whenever settings change so the player doesn't have to
    -- restart to hear the new volumes.
  , abSetGains :: Double -> Double -> Double -> IO ()
    -- | Clean up any open sound devices and channels.  Paired with
    -- backend construction; called from the launcher on exit.
  , abShutdown :: IO ()
  }

-- | The default backend: a well-behaved no-op.  Every shipped
-- executable gets audio hooks without needing to link SDL2_mixer —
-- when the real backend lands, only the launcher wiring changes.
silentBackend :: AudioBackend
silentBackend = AudioBackend
  { abPlay     = \_        -> pure ()
  , abStopAll  = pure ()
  , abSetGains = \_ _ _    -> pure ()
  , abShutdown = pure ()
  }

-- | Convenience wrapper so callers don't have to reach through a
-- record field every time they want to fire an event.
playEvent :: AudioBackend -> AudioEvent -> IO ()
playEvent = abPlay
