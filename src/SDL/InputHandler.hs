-- | SDL2 input handling.  Maps key events to characters, and provides
-- the option-key scheme used to label and pick player actions in-game.
module SDL.InputHandler
  ( awaitKeySDL
  , awaitAnyKeySDL
  , InputEvent(..)
  , awaitInputSDL
  , pollQuit
  , pollAnyKey
  , waitOrKey
  , RevealInput(..)
  , waitOrRevealInput
  , optionKeys
  , optionKeyFor
  , safeOptionIndex
  , movementOptionKeys
  , generalOptionKeys
  , poolKeyFor
  , safeOptionIndexIn
  , quitKeyChar
  , debugKeyChar
  ) where

import           Control.Applicative ((<|>))
import           Data.List           (elemIndex)
import           Foreign.C.Types     (CInt)
import qualified SDL
import qualified SDL.Input.Keyboard.Codes as KC

-- | Block until a relevant keypress, returning the character.
-- Returns Nothing on window close.  Clicks and touches are ignored
-- by this variant; use 'awaitInputSDL' when either is acceptable.
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

-- | Either a mapped key press or a pointer event (mouse click or
-- finger tap).  Pointer coordinates are in *pixels*, measured from
-- the top-left of the window — the menu / runner code already knows
-- each clickable region in pixels, so a single hit-test function
-- fits both input paths.
data InputEvent
  = KeyPress Char
  | ClickAt  !Int !Int
  deriving (Show, Eq)

-- | Block until any usable input — a mapped key, a mouse button-down,
-- or a finger-down — or a window close.  Unmapped keys and motion
-- events are silently skipped; only discrete \"I picked something\"
-- events resolve the wait.  Clicks and touches return pixel
-- coordinates relative to the window's top-left.
--
-- The 'SDL.Window' argument is used to convert SDL's normalized touch
-- coordinates into pixel coordinates that match the renderer's click
-- targets.  Mouse coords are already in window pixels, so the window
-- isn't consulted on that path.
awaitInputSDL :: SDL.Window -> IO (Maybe InputEvent)
awaitInputSDL window = do
  event <- SDL.waitEvent
  case SDL.eventPayload event of
    SDL.QuitEvent -> pure Nothing
    SDL.KeyboardEvent kd
      | SDL.keyboardEventKeyMotion kd == SDL.Pressed ->
          case keycodeToChar (SDL.keysymKeycode (SDL.keyboardEventKeysym kd)) of
            Just c  -> pure (Just (KeyPress c))
            Nothing -> awaitInputSDL window
    SDL.MouseButtonEvent mb
      | SDL.mouseButtonEventMotion mb == SDL.Pressed -> do
          let SDL.P (SDL.V2 x y) = SDL.mouseButtonEventPos mb
          pure (Just (ClickAt (fromIntegral x) (fromIntegral y)))
    SDL.TouchFingerEvent tf
      | SDL.touchFingerEventMotion tf == SDL.Pressed -> do
          -- SDL reports touches in normalized [0, 1] coords.  Scale
          -- against the *actual* window size so the mapping matches
          -- the click rects the renderer built for this frame —
          -- otherwise a Surface / tablet at a non-Deck viewport lands
          -- taps in the wrong place.
          let SDL.P (SDL.V2 nx ny) = SDL.touchFingerEventPos tf
          SDL.V2 w h <- SDL.get (SDL.windowSize window)
          pure (Just (ClickAt (round (nx * fromIntegral (w :: CInt)))
                              (round (ny * fromIntegral (h :: CInt)))))
    _ -> awaitInputSDL window

-- | Block until any keypress or window close. Returns True on keypress, False on quit.
awaitAnyKeySDL :: IO Bool
awaitAnyKeySDL = do
  event <- SDL.waitEvent
  case SDL.eventPayload event of
    SDL.QuitEvent -> pure False
    SDL.KeyboardEvent kd
      | SDL.keyboardEventKeyMotion kd == SDL.Pressed -> pure True
    _ -> awaitAnyKeySDL

-- | Map SDL keycodes to the characters our game loop expects.  All 26
-- letter keys and digits 1-9 are mapped through; other keys are
-- ignored.  The keysym is the physical key, so shift state doesn't
-- flip case — we always return lowercase.
keycodeToChar :: SDL.Keycode -> Maybe Char
keycodeToChar kc = lookup kc letterCodes <|> lookup kc digitCodes
  where
    letterCodes =
      [ (KC.KeycodeA, 'a'), (KC.KeycodeB, 'b'), (KC.KeycodeC, 'c')
      , (KC.KeycodeD, 'd'), (KC.KeycodeE, 'e'), (KC.KeycodeF, 'f')
      , (KC.KeycodeG, 'g'), (KC.KeycodeH, 'h'), (KC.KeycodeI, 'i')
      , (KC.KeycodeJ, 'j'), (KC.KeycodeK, 'k'), (KC.KeycodeL, 'l')
      , (KC.KeycodeM, 'm'), (KC.KeycodeN, 'n'), (KC.KeycodeO, 'o')
      , (KC.KeycodeP, 'p'), (KC.KeycodeQ, 'q'), (KC.KeycodeR, 'r')
      , (KC.KeycodeS, 's'), (KC.KeycodeT, 't'), (KC.KeycodeU, 'u')
      , (KC.KeycodeV, 'v'), (KC.KeycodeW, 'w'), (KC.KeycodeX, 'x')
      , (KC.KeycodeY, 'y'), (KC.KeycodeZ, 'z')
      ]
    digitCodes =
      [ (KC.Keycode1, '1'), (KC.Keycode2, '2'), (KC.Keycode3, '3')
      , (KC.Keycode4, '4'), (KC.Keycode5, '5'), (KC.Keycode6, '6')
      , (KC.Keycode7, '7'), (KC.Keycode8, '8'), (KC.Keycode9, '9')
      , (KC.KeycodeEscape, quitKeyChar)
      , (KC.KeycodeF3,     debugKeyChar)
      , (KC.KeycodeReturn,  '\n')
      , (KC.KeycodeReturn2, '\n')
      , (KC.KeycodeKPEnter, '\n')
      ]

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

-- | Wait up to @ms@ milliseconds for any skip-worthy input — a key
-- press, a mouse button-down, or a finger-down.  Returns True if one
-- was consumed, False on timeout.  Used by the typewriter to treat
-- either a keypress or a tap as "finish the text already."  Only
-- consumes one event so window-management events aren't silently
-- drained.
waitOrKey :: Int -> IO Bool
waitOrKey ms = do
  mEvent <- SDL.waitEventTimeout (fromIntegral ms)
  case mEvent of
    Nothing -> pure False
    Just event -> case SDL.eventPayload event of
      SDL.KeyboardEvent kd
        | SDL.keyboardEventKeyMotion kd == SDL.Pressed -> pure True
      SDL.MouseButtonEvent mb
        | SDL.mouseButtonEventMotion mb == SDL.Pressed -> pure True
      SDL.TouchFingerEvent tf
        | SDL.touchFingerEventMotion tf == SDL.Pressed -> pure True
      _ -> pure False

-- | Input observed during the HUD reveal animation.  Keys dispatch
-- through as skip+select in one gesture; pointer events skip without
-- a selection (the player needs to tap again on the fully-revealed
-- HUD).  Keeps tap UX aligned with the typewriter — first tap
-- finishes the animation, second tap selects.
data RevealInput
  = RevealTimeout     -- ^ nothing happened within the window
  | RevealSkip        -- ^ pointer event: end animation, no selection
  | RevealKey !Char   -- ^ mapped key: end animation and dispatch this char
  deriving (Show, Eq)

-- | Wait up to @ms@ milliseconds for skip-worthy input during the HUD
-- reveal animation.  Keys produce 'RevealKey' (skip + select), pointer
-- events produce 'RevealSkip' (skip only), unmapped keys and ignored
-- events map to 'RevealTimeout' so the animation keeps running.
waitOrRevealInput :: Int -> IO RevealInput
waitOrRevealInput ms = do
  mEvent <- SDL.waitEventTimeout (fromIntegral ms)
  case mEvent of
    Nothing    -> pure RevealTimeout
    Just event -> case SDL.eventPayload event of
      SDL.KeyboardEvent kd
        | SDL.keyboardEventKeyMotion kd == SDL.Pressed ->
            case keycodeToChar (SDL.keysymKeycode (SDL.keyboardEventKeysym kd)) of
              Just c  -> pure (RevealKey c)
              Nothing -> pure RevealTimeout
      SDL.MouseButtonEvent mb
        | SDL.mouseButtonEventMotion mb == SDL.Pressed -> pure RevealSkip
      SDL.TouchFingerEvent tf
        | SDL.touchFingerEventMotion tf == SDL.Pressed -> pure RevealSkip
      _ -> pure RevealTimeout

-- ---------------------------------------------------------------------------
-- Option-key scheme
-- ---------------------------------------------------------------------------
--
-- Action lists can exceed nine options, so numeric selection doesn't
-- stretch far enough.  We use letters instead, walking the alphabet
-- in order and skipping keys reserved for global UI ('q' quit, 'd'
-- debug).  Option 1 → 'a', option 2 → 'b', option 3 → 'c', option 4 →
-- 'e' (skipping 'd'), and so on.

-- | Characters reserved for global commands and unavailable as option keys.
reservedOptionKeys :: String
reservedOptionKeys = "qd"

-- | Letter keys available for option selection, in presentation order.
-- 24 keys — plenty for any realistic action list.
optionKeys :: String
optionKeys = filter (`notElem` reservedOptionKeys) ['a' .. 'z']

-- | The character that labels option @n@ (1-based).  Falls back to the
-- decimal representation of @n@ if the option index exceeds the
-- available letter pool (shouldn't happen in practice).
optionKeyFor :: Int -> Char
optionKeyFor n
  | n >= 1 && n <= length optionKeys = optionKeys !! (n - 1)
  | otherwise = case show n of
      (c:_) -> c
      []    -> '?'

-- | Pick an element by its option-key character.  'a' → index 0, 'b' → 1,
-- etc.  Returns @Nothing@ for unmapped keys or out-of-range picks.
safeOptionIndex :: Char -> [x] -> Maybe x
safeOptionIndex = safeOptionIndexIn optionKeys

-- ---------------------------------------------------------------------------
-- Positional pools
-- ---------------------------------------------------------------------------
--
-- The runtime splits action options across two keyboard rows so the
-- player's fingers fall naturally: movement on the top letter row,
-- general (non-movement) actions on the home row.  Reserved roles
-- live on non-letter keys — Escape quits, F3 cycles debug — so every
-- letter is free for a scenario option.

-- | Top-row keys, left to right.  Carry movement (spatial HUD) options.
movementOptionKeys :: String
movementOptionKeys = "qwertyuiop"

-- | Home-row keys, left to right.  Carry non-movement actions.
generalOptionKeys :: String
generalOptionKeys = "asdfghjkl"

-- | Placeholder character used internally for the Escape keycode.
-- Chosen to be outside ASCII and outside any letter/digit pool so
-- option-key lookups never confuse it with a real selection.
quitKeyChar :: Char
quitKeyChar = '\x1B'  -- ASCII ESC

-- | Placeholder for F3 (debug cycle), same rationale as 'quitKeyChar'.
debugKeyChar :: Char
debugKeyChar = '\x7F'  -- ASCII DEL, not emitted by any text key

-- | Option-key character for the @n@'th (1-based) entry of the pool.
-- Falls back to the decimal representation of @n@ only if @n@
-- overflows the pool (practically unreachable for the HUD's cell
-- counts, but kept safe).
poolKeyFor :: String -> Int -> Char
poolKeyFor pool n
  | n >= 1 && n <= length pool = pool !! (n - 1)
  | otherwise = case show n of
      (c:_) -> c
      []    -> '?'

-- | Pick an element from @xs@ by its position in @pool@.
safeOptionIndexIn :: String -> Char -> [x] -> Maybe x
safeOptionIndexIn pool c xs =
  case elemIndex c pool of
    Just i | i < length xs -> Just (xs !! i)
    _                      -> Nothing
