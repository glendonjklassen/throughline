-- | Per-identity competencies — durable, monotone, boolean facts
-- about a character that travel across scenarios.
--
-- A competency is a named capacity earned through demonstrated
-- practice in any scenario.  Once a player has it, they have it
-- forever (no decay), and any future scenario can read it to gate or
-- tint behavior — the world describes itself differently to a
-- competent character.
--
-- The engine owns the vocabulary; scenarios decide what counts as
-- earning evidence in their context, and what reading the
-- competency means in their context.  Earning is end-of-scenario
-- (the per-bundle 'entryOnEnd' hook calls 'grantCompetency'); reading
-- happens at scenario construction (the factory takes 'Progress'
-- and branches).
--
-- __Per-entry contract.__  Each constructor below ships a Haddock
-- contract with three sections:
--
--   * @GRANT@ — the in-world signal that should mark the competency
--     as earned.  Scenarios diverge on the specific mechanic, but
--     should converge on the spirit.
--   * @READ@ — the kind of beat or option that should consult the
--     competency.
--   * @NOT@ — the adjacent capacities that this one explicitly does
--     /not/ cover, to keep the vocabulary disjoint.
--
-- A scenario author who grants or reads a competency for something
-- outside its contract is wrong against a written spec, not against
-- a vibe.  Disputes go to this module's docstring.
--
-- __Forward link.__  These competencies are the natural unit for
-- Steam achievements: each one is a discrete, monotone, player-facing
-- accomplishment that already maps to a per-identity boolean in the
-- 'Engine.Sync.Progress' file.  When the Steam build wires
-- achievements, the unlock event is a successful 'grantCompetency'
-- call.
module Engine.Competency
  ( Competency (..)
  , allCompetencies
  , competencyName
  ) where

import qualified Data.Aeson as Aeson
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Text  (Text)
import qualified Data.Text  as T

-- | The full vocabulary of competencies.  Closed enum on purpose —
-- adding a new entry is an engine change, which is the friction
-- that keeps the list disciplined.  Promote a new competency only
-- when at least two scenarios independently want it.
data Competency
  = -- | Sustained patience in a charged-but-uneventful moment.
    --
    -- @GRANT@: the player chose inaction during a window the world
    -- made tempting to act in (≥ N consecutive sit-still ticks
    -- with no movement, no shot, no draw).
    --
    -- @READ@: options or beats that require comfort with stillness
    -- in tense or socially-charged silence (a long deer sit, an
    -- awkward counter silence, a stakeout).
    --
    -- @NOT@: physical immobility for stealth ('NightVision' /
    -- low-light territory); patience with another person's slowness
    -- ('LiveBodyReading' territory).
    WaitingWithoutAct

  | -- | Extracting meaning from traces left by animals.
    --
    -- @GRANT@: the player discovered a meaningful number of distinct
    -- sign tags during a hunt (tracks, scrapes, scat, beds, sheds).
    --
    -- @READ@: scenarios where reading non-human traces matters —
    -- another hunt, a trapline, recognising a poacher's evidence.
    --
    -- @NOT@: weather cues ('WeatherPrediction' territory); human
    -- traces ('LiveBodyReading' territory, which is live-only
    -- anyway).
    AnimalSignReading

  | -- | Anticipating weather change from current ambient cues
    -- (wind shift, sky colour, animal behaviour).
    --
    -- @GRANT@: player correctly anticipated a weather transition
    -- (e.g. moved to cover before a flurry) often enough to indicate
    -- it wasn't luck.
    --
    -- @READ@: outdoor scenarios where weather affects outcomes —
    -- ice-fishing, sailing, farming, a future trapline.
    --
    -- @NOT@: noticing the current weather ('AnimalSignReading' is
    -- closer); seasonal/calendar knowledge.
    WeatherPrediction

  | -- | Interpreting a live human's posture, micro-expression, or
    -- gait in real time.
    --
    -- @GRANT@: scenarios where the player correctly identified
    -- another character's emotional or intent state from
    -- non-verbal cues alone (e.g. spotting the lying coworker in
    -- TopBuy).
    --
    -- @READ@: social scenarios with deception, hidden mood, or
    -- approach-avoidance dynamics.
    --
    -- @NOT@: reading written text or tone in dialogue (separate
    -- capacity, not yet promoted); reading a corpse or an absent
    -- person ('AnimalSignReading' applies to non-human traces only,
    -- and "post-mortem human reading" doesn't have a competency
    -- yet).
    LiveBodyReading

  | -- | Fine motor control under acute stress.
    --
    -- @GRANT@: the player executed a precise, irreversible action
    -- in a high-stakes moment (the shot, the suture, the pour
    -- under hostile attention) and didn't fumble.
    --
    -- @READ@: any scenario with a single-attempt high-stakes
    -- precision action.
    --
    -- @NOT@: low-stress fine motor (just dexterity, not earned
    -- through pressure); endurance of repetition (different
    -- capacity, not yet promoted).
    StressMotorControl

  | -- | Operating effectively in low light.
    --
    -- @GRANT@: the player accomplished meaningful action during
    -- dawn, dusk, or full dark phases of a scenario without
    -- artificial light.
    --
    -- @READ@: night-set or pre-dawn scenarios — late shifts, early
    -- starts, urban-after-hours.
    --
    -- @NOT@: stealth (separate; not yet promoted); stillness
    -- ('WaitingWithoutAct' is closer).
    NightVision
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance ToJSON Competency where
  toJSON = Aeson.toJSON . competencyName

instance FromJSON Competency where
  parseJSON = Aeson.withText "Competency" $ \t ->
    case lookup t [ (competencyName c, c) | c <- allCompetencies ] of
      Just c  -> pure c
      Nothing -> fail ("Unknown Competency: " <> show t)

-- | Every competency in the vocabulary.  Use over @[minBound..maxBound]@
-- when iterating so call sites read intentionally.
allCompetencies :: [Competency]
allCompetencies = [minBound .. maxBound]

-- | Stable on-disk name for a competency.  Matches the constructor
-- name today; deliberately separated through this function so
-- renaming a constructor in code doesn't silently break older
-- progress files — change the constructor and add a fall-through
-- here to translate the old name on read.
competencyName :: Competency -> Text
competencyName WaitingWithoutAct  = T.pack "WaitingWithoutAct"
competencyName AnimalSignReading  = T.pack "AnimalSignReading"
competencyName WeatherPrediction  = T.pack "WeatherPrediction"
competencyName LiveBodyReading    = T.pack "LiveBodyReading"
competencyName StressMotorControl = T.pack "StressMotorControl"
competencyName NightVision        = T.pack "NightVision"
