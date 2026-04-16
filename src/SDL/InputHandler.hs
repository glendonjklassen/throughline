-- | SDL2 input handling. Maps key events to the same character set
-- as the terminal UI: digits 1-9, 'q', 'd', 'm'.
module SDL.InputHandler
  ( awaitKeySDL
  , awaitAnyKeySDL
  , pollQuit
  , pollAnyKey
  , waitOrKey
  , safeIndex
  ) where

import qualified SDL
import qualified SDL.Input.Keyboard.Codes as KC
import           Text.Read (readMaybe)

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

-- | Check if any key was pressed (non-blocking). Returns True if a keypress
-- was consumed, False otherwise. Does not consume quit events.
pollAnyKey :: IO Bool
pollAnyKey = any isKey <$> SDL.pollEvents
  where
    isKey e = case SDL.eventPayload e of
      SDL.KeyboardEvent kd ->
        SDL.keyboardEventKeyMotion kd == SDL.Pressed
      _ -> False

-- | Wait up to @ms@ milliseconds for a keypress.  Returns True if a key
-- was pressed, False if the timeout expired.  Only consumes one event at a
-- time so window-management events are not silently drained.
waitOrKey :: Int -> IO Bool
waitOrKey ms = do
  mEvent <- SDL.waitEventTimeout (fromIntegral ms)
  case mEvent of
    Nothing -> pure False  -- timeout, no event
    Just event -> case SDL.eventPayload event of
      SDL.KeyboardEvent kd
        | SDL.keyboardEventKeyMotion kd == SDL.Pressed -> pure True
      _ -> pure False  -- non-key event, leave the rest in the queue

-- | Map a single character to a 1-based index into the list.
-- Returns Nothing for non-digit input, '0', or indices beyond the list length.
safeIndex :: Char -> [x] -> Maybe x
safeIndex c as =
  case readMaybe [c] of
    Just i | i >= 1, i <= length as -> Just (as !! (i - 1))
    _                               -> Nothing
