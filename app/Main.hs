module Main where

import           Engine
import           GameTypes           (CharId, Scenario)
import           SDL.Layout          (ScenarioDisplay, defaultDisplay)
import           SDL.Runner          (sdlUI)
import           SDL.Renderer        (SDLContext(..), initSDL, freeSDL, clearSDL, presentSDL)
import           SDL.FontContext     (renderText)
import           SDL.Palette         (defaultText, dimText, greyText)
import           SDL.InputHandler    (awaitKeySDL)

import           Scenarios.DeerHunt  (deerHunt, deerHuntDisplay)
import           Scenarios.TopBuy    (topBuy, topBuyDisplay)
import           Scenarios.Diner     (diner, dinerDisplay)
import           Scenarios.DinerMaya (dinerMaya, dinerMayaDisplay)
import           Scenarios.Customer  (customer)

-- ---------------------------------------------------------------------------
-- Scenario registry
-- ---------------------------------------------------------------------------

scenarioList :: [(String, String, ScenarioDisplay, Int -> CharId -> Scenario)]
scenarioList =
  [ ("Deer Hunt",         "Mid-November. Southern Manitoba. One square mile. One buck.",
     deerHuntDisplay, deerHunt)
  , ("Top Buy",           "A retail ethics dilemma. Your coworker is stealing.",
     topBuyDisplay, topBuy)
  , ("Late Night Diner",  "2 AM. Can't sleep. A diner, a server, a stranger.",
     dinerDisplay,  diner)
  , ("Diner: Maya",       "The same night. Behind the counter.",
     dinerMayaDisplay, dinerMaya)
  , ("Customer",          "Walking through a store. (Prototype)",
     defaultDisplay, customer)
  ]

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = sdlMain

-- ---------------------------------------------------------------------------
-- SDL mode
-- ---------------------------------------------------------------------------

sdlMain :: IO ()
sdlMain = do
  ctx <- initSDL "assets/JetBrainsMono-Regular.ttf"
  choice <- sdlMenu ctx
  freeSDL ctx
  case choice of
    Nothing -> pure ()  -- quit
    Just idx -> do
      let (_, _, disp, mkScenario) = scenarioList !! idx
      runScenario (sdlUI disp) mkScenario

-- | Render the scenario menu in the SDL window and await a choice.
sdlMenu :: SDLContext -> IO (Maybe Int)
sdlMenu ctx = do
  clearSDL ctx
  let fc = sdlFont ctx
  renderText fc "throughline" defaultText (3, 2)
  renderText fc "A narrative engine." dimText (3, 3)
  renderText fc "" dimText (3, 4)
  mapM_ (\(n, (label, desc, _, _)) -> do
    let row = fromIntegral (4 + n * 2)
    renderText fc (show n <> ". " <> label) defaultText (4, row)
    renderText fc ("   " <> desc) dimText (4, row + 1)
    ) (zip [1 :: Int ..] scenarioList)
  let quitRow = fromIntegral (4 + length scenarioList * 2 + 2)
  renderText fc "q) Quit" greyText (4, quitRow)
  presentSDL ctx
  awaitChoice
  where
    awaitChoice = do
      mc <- awaitKeySDL
      case mc of
        Nothing  -> pure Nothing
        Just 'q' -> pure Nothing
        Just c   ->
          let n = fromEnum c - fromEnum '0'
          in if n >= 1 && n <= length scenarioList
               then pure (Just (n - 1))
               else awaitChoice
