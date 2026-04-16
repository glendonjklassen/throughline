-- | SDL2 input handling. Maps key events to the same character set
-- as the terminal UI: digits 1-9, 'q', 'd', 'm'.
module SDL.InputHandler
  ( awaitKeySDL
  , awaitAnyKeySDL
  , pollQuit
  ) where

import qualified SDL
import qualified SDL.Input.Keyboard.Codes as KC

-- | Block until a relevant keypress, returning the character.
-- Returns Nothing on window close.
awaitKeySDL :: IO (Maybe Char)
awaitKeySDL = do
  event <- SDL.waitEvent
  case SDL.eventPayload event of
    SDL.QuitEvent -> pure Nothing
    SDL.KeyboardEvent kd
      | SDL.keyboardEventKeyMotion kd == SDL.Pressed ->
          case keycodeToChar (SDL.keysymKeycode (SDL.keyboardEventKeysym kd)) of
            Just c  -> pure (Just c)
            Nothing -> awaitKeySDL  -- ignore unmapped keys
    _ -> awaitKeySDL

-- | Block until any keypress or window close. Returns True on keypress, False on quit.
awaitAnyKeySDL :: IO Bool
awaitAnyKeySDL = do
  event <- SDL.waitEvent
  case SDL.eventPayload event of
    SDL.QuitEvent -> pure False
    SDL.KeyboardEvent kd
      | SDL.keyboardEventKeyMotion kd == SDL.Pressed -> pure True
    _ -> awaitAnyKeySDL

-- | Map SDL keycodes to the characters our game loop expects.
keycodeToChar :: SDL.Keycode -> Maybe Char
keycodeToChar kc
  | kc == KC.Keycode1 = Just '1'
  | kc == KC.Keycode2 = Just '2'
  | kc == KC.Keycode3 = Just '3'
  | kc == KC.Keycode4 = Just '4'
  | kc == KC.Keycode5 = Just '5'
  | kc == KC.Keycode6 = Just '6'
  | kc == KC.Keycode7 = Just '7'
  | kc == KC.Keycode8 = Just '8'
  | kc == KC.Keycode9 = Just '9'
  | kc == KC.KeycodeQ = Just 'q'
  | kc == KC.KeycodeD = Just 'd'
  | kc == KC.KeycodeM = Just 'm'
  | otherwise         = Nothing

-- | Check if a quit event is pending (non-blocking).
pollQuit :: IO Bool
pollQuit = any isQuit <$> SDL.pollEvents
  where
    isQuit e = case SDL.eventPayload e of
      SDL.QuitEvent -> True
      SDL.KeyboardEvent kd ->
        SDL.keyboardEventKeyMotion kd == SDL.Pressed
        && SDL.keysymKeycode (SDL.keyboardEventKeysym kd) == KC.KeycodeQ
      _ -> False
