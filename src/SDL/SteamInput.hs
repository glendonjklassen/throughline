{- HLINT ignore "Use head" -}
-- | Bridge between Steam Input's named digital actions and the
-- internal keyboard characters the SDL input handler dispatches on.
--
-- Why a decoupled layer: the actual Steam Input SDK is C and needs
-- FFI, a Steam App ID, and the Steam runtime to test.  None of that
-- is present in a dev build.  But the *mapping* — "when Steam says
-- move_north, act as if the player pressed W" — is pure, testable,
-- and the only piece that scenarios or the runner ever see.  So we
-- build the mapping now and let the FFI layer drop in later without
-- the engine needing to change.
--
-- The steam-input action names match the default 'Action Set' a
-- bundle would publish through the Steam Partner backend.  Add or
-- rename actions here when the published set changes; runtime code
-- always treats the mapping as the source of truth.
module SDL.SteamInput
  ( SteamAction
  , steamActionMap
  , steamActionToChar
  , allSteamActions
  ) where

import qualified Data.Map.Strict as Map

import           SDL.InputHandler (debugKeyChar, generalOptionKeys,
                                   movementOptionKeys, quitKeyChar)

-- | The name Steam uses for a digital action.  Kept as 'String' to
-- match what the Steam Input API returns; there's no upside to a
-- more structured type when the values come from an external
-- configuration.
type SteamAction = String

-- | Map from published Steam action names to the internal character
-- the input handler dispatches on.  A controller press that binds to
-- "move_forward" should act as the first movement letter (top-left
-- of the qwerty row).
--
-- Movement actions ('move_north' through 'move_clockwise') map onto
-- the positional movement letters.  General actions like 'journal'
-- and 'confirm' map onto fixed control keys so they don't collide
-- with the general-action pool, which is dynamic per scenario.
steamActionMap :: Map.Map SteamAction Char
steamActionMap = Map.fromList
  [ -- 8 principal directions + a 9th "straight" slot mirror the
    -- movement letters on the keyboard.  The qwerty row is the
    -- "compass" in our HUD convention.
    ("move_nw",       movementOptionKeys !! 0)   -- q
  , ("move_n",        movementOptionKeys !! 1)   -- w
  , ("move_ne",       movementOptionKeys !! 2)   -- e
  , ("move_e",        movementOptionKeys !! 3)   -- r
  , ("move_se",       movementOptionKeys !! 4)   -- t
  , ("move_s",        movementOptionKeys !! 5)   -- y
  , ("move_sw",       movementOptionKeys !! 6)   -- u
  , ("move_w",        movementOptionKeys !! 7)   -- i
  , ("move_forward",  movementOptionKeys !! 8)   -- o
  , ("move_alt",      movementOptionKeys !! 9)   -- p

    -- General actions — the face-button pool on a controller.  Keys
    -- match the home-row letters the keyboard uses.
  , ("action_primary",   generalOptionKeys !! 0) -- a
  , ("action_secondary", generalOptionKeys !! 1) -- s
  , ("action_tertiary",  generalOptionKeys !! 2) -- d
  , ("action_quaternary",generalOptionKeys !! 3) -- f
  , ("action_5",         generalOptionKeys !! 4) -- g
  , ("action_6",         generalOptionKeys !! 5) -- h
  , ("action_7",         generalOptionKeys !! 6) -- j
  , ("action_8",         generalOptionKeys !! 7) -- k
  , ("action_9",         generalOptionKeys !! 8) -- l

    -- Fixed controls.  "journal" is a dedicated button on the
    -- controller (default binding: Select) so the player never has
    -- to scroll to it.
  , ("journal",     '1')
  , ("quit",        quitKeyChar)
  , ("debug_cycle", debugKeyChar)   -- dev builds only; release
                                     -- should omit this from the
                                     -- published Action Set.
  ]

-- | Resolve a Steam action name to its internal character, or
-- 'Nothing' if the action isn't bound.  Unbound actions are dropped
-- silently — the player can rebind them through Steam's own UI and
-- the runtime will pick up whatever they pick next time.
steamActionToChar :: SteamAction -> Maybe Char
steamActionToChar name = Map.lookup name steamActionMap

-- | Every Steam action the bundle publishes.  Used by the bundle's
-- Steam Input manifest generator (when one exists) to keep the
-- shipped Action Set in sync with what the code accepts.
allSteamActions :: [SteamAction]
allSteamActions = Map.keys steamActionMap
