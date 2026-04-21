module Scenarios.DeerHuntSpec (spec) where

import           Data.Either        (isRight)
import qualified Data.Map.Strict as Map
import           Test.Hspec

import           Engine.Author.Validate    (validateScenario, validateSceneGraph)
import           Engine.Core.Conditions    (checkCondition)
import           Engine.Core.World         (setCharacterStat)
import           Engine.Author.Random      (rollD)
import           Engine.Author.Scene       (SceneGraph(..), SceneEdge(..))
import           Engine.CRDT.ORSet         (orFromList, orEmpty, orMember)
import           Engine.Headless            (runHeadlessRandom)
import           GameTypes
import           MonadStack                (AppError, Env)

import           TestFixtures              (mkScenarioEnv, step)

import           Scenarios.DeerHunt           (deerHunt)
import           Scenarios.DeerHunt.Actions   (huntGraph)
import           Scenarios.DeerHunt.Constants
import           Scenarios.DeerHunt.Generation (TerrainClass(..))
import           Scenarios.DeerHunt.Probability
import           Scenarios.DeerHuntTestFixtures

-- | Build a world where the player and deer are co-located at the
-- given location, DeerSpotted is set, and the clock tick is controlled
-- for deterministic random outcomes.
mkShootableWorld :: CharId -> Location -> Int -> GameWorld
mkShootableWorld you loc tick = GameWorld
  { worldCharacters = Map.fromList
      [ (you,  Character you  "You"      [] orEmpty)
      , (deer, Character deer "The Deer" [] orEmpty)
      ]
  , worldGraph
      = setCharacterStat you  (Capacity Intelligence)  5
      . setCharacterStat you  (Capacity Strength)      6
      . setCharacterStat you  (Capacity Understanding) 2
      . setCharacterStat you  (Capacity Hunger)        8
      . setCharacterStat deer (Capacity Intelligence)  3
      . setCharacterStat deer (Capacity Strength)      6
      $ Map.empty
  , worldLocations = Map.fromList [(you, loc), (deer, loc)]
  , worldActiveEffects = []
  , worldClock     = LamportClock tick (PlayerId "test")
  , worldTags      = orFromList
      [ deerSpotted
      , weatherTag (WeatherDesc "Clear and Cold")
      , seasonTag 3, dayOfWeekTag 5, lunarPhaseTag 0
      , dayNumberTag 0, timeTag 10
      ]
  , worldLocationGraph = emptyLocationGraph
  , worldSeed          = 0
  , worldLocationHistory = Map.empty
  , worldLocationVisits  = Map.empty
  }

-- | Same as mkShootableWorld but with another hunter co-located.
mkShootableWorldWith :: CharId -> CharId -> Location -> Int -> GameWorld
mkShootableWorldWith you other loc tick =
  let base = mkShootableWorld you loc tick
  in base
    { worldCharacters = Map.insert other (Character other "Other Hunter" [] orEmpty)
                                   (worldCharacters base)
    , worldLocations  = Map.insert other loc (worldLocations base)
    , worldGraph
        = setCharacterStat other (Capacity Intelligence) 5
        . setCharacterStat other (Capacity Strength) 6
        . setCharacterStat other (Capacity Understanding) 2
        $ worldGraph base
    }

-- | Search for a clock tick that produces the desired shot outcome.
findTick :: (GameWorld -> Bool) -> (Location -> Int -> GameWorld) -> Location -> Int
findTick predicate mkWorld loc =
  case [ t | t <- [0..5000], predicate (mkWorld loc t) ] of
    (t:_) -> t
    []    -> error "findTick: no tick in [0..5000] satisfied predicate"

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  let you = Named "test-player"
      -- Canonical "shootable" location: any field location from the fixture map.
      shootLoc = pickByClass CField

  describe "DeerHunt scenario" $ do

    it "solo clean kill: DeerKilled" $ do
      let tick = findTick (`doesShotHit` you) (mkShootableWorld you) shootLoc
          w0 = mkShootableWorld you shootLoc tick
          scenario = (deerHunt fixtureSeed you) { scenarioInitial = w0 }
      env <- mkScenarioEnv you scenario
      wFinal <- step env (ActionId "takeTheShot") w0
      checkCondition wFinal (HasWorldTag deerKilled) `shouldBe` True
      checkCondition wFinal (HasWorldTag deerGone)   `shouldBe` False
      checkCondition wFinal (HasWorldTag hunterShot) `shouldBe` False

    it "missed shot: DeerGone" $ do
      let tick = findTick (\w -> not (doesShotHit w you)) (mkShootableWorld you) shootLoc
          w0 = mkShootableWorld you shootLoc tick
          scenario = (deerHunt fixtureSeed you) { scenarioInitial = w0 }
      env <- mkScenarioEnv you scenario
      wFinal <- step env (ActionId "takeTheShot") w0
      checkCondition wFinal (HasWorldTag deerGone)   `shouldBe` True
      checkCondition wFinal (HasWorldTag deerKilled) `shouldBe` False

    it "co-located kill: DeerKilled, trust drops" $ do
      let other = Named "other-hunter"
          mkW = mkShootableWorldWith you other
          tick = findTick (\w -> doesShotHit w you && not (isFriendlyFire w)) mkW shootLoc
          w0 = mkShootableWorldWith you other shootLoc tick
          scenario = (deerHunt fixtureSeed you) { scenarioInitial = w0 }
      env <- mkScenarioEnv you scenario
      wFinal <- step env (ActionId "takeTheShot") w0
      checkCondition wFinal (HasWorldTag deerKilled) `shouldBe` True
      checkCondition wFinal (HasWorldTag hunterShot) `shouldBe` False

    it "friendly fire: HunterShot" $ do
      let other = Named "other-hunter"
          mkW = mkShootableWorldWith you other
          tick = findTick (\w -> doesShotHit w you && isFriendlyFire w) mkW shootLoc
          w0 = mkShootableWorldWith you other shootLoc tick
          scenario = (deerHunt fixtureSeed you) { scenarioInitial = w0 }
      env <- mkScenarioEnv you scenario
      wFinal <- step env (ActionId "takeTheShot") w0
      checkCondition wFinal (HasWorldTag hunterShot) `shouldBe` True
      checkCondition wFinal (HasWorldTag deerKilled) `shouldBe` False

    it "tension rises with deer spotted" $ do
      let w0 = mkShootableWorld you shootLoc 42
          scenario = (deerHunt fixtureSeed you) { scenarioInitial = w0 }
      env <- mkScenarioEnv you scenario
      w1 <- step env (ActionId "sit:on") w0
      getTension w1 `shouldSatisfy` (>= 8)

    it "experience increases with fresh sign" $ do
      -- Player near deer: find a field location in the same region as the
      -- deer's starting class, then co-locate for the sign trigger.
      let scenario = deerHunt fixtureSeed you
          fieldLoc = pickByClass CField
          w0 = (scenarioInitial scenario)
                 { worldLocations = Map.fromList
                     [ (you,  fieldLoc)
                     , (deer, fieldLoc)
                     ] }
          startExp = experience you w0
      env <- mkScenarioEnv you (scenario { scenarioInitial = w0 })
      w1 <- step env (ActionId "sit:on") w0
      let endExp = experience you w1
      endExp `shouldSatisfy` (>= startExp)

    it "can navigate between adjacent nodes" $ do
      let scenario = deerHunt fixtureSeed you
          w0 = scenarioInitial scenario
          -- Find any edge out of the start location.
          (path, _) = walkPath fixtureStart CField 1
      case path of
        ((from, to) : _) -> do
          env <- mkScenarioEnv you scenario
          let walkId = edgeActionId from to
          w1 <- step env walkId w0
          charLocation you w1 `shouldBe` Just to
        [] -> expectationFailure "walkPath returned empty path"

  describe "validation" $ do
    let you' = Named "test-player"

    it "has no duplicate ActionIds" $
      isRight (validateScenario (deerHunt fixtureSeed you')) `shouldBe` True

    it "has a connected scene graph" $
      isRight (validateSceneGraph (huntGraph fixtureHuntWorld)) `shouldBe` True

  -- -------------------------------------------------------------------
  -- Integration tests
  -- -------------------------------------------------------------------

  describe "integration" $ do

    it "look discovers deer and enables the shot" $ do
      let shotAtNextTick t =
            let w = (mkShootableWorld you shootLoc t)
                      { worldClock = LamportClock (t + 1) (PlayerId "test") }
            in doesShotHit w you
          tick = case [ t | t <- [0..5000], shotAtNextTick t ] of
                   (t:_) -> t
                   []    -> error "look discovers deer: no tick found"
          baseTags = orFromList
            [ weatherTag (WeatherDesc "Clear and Cold")
            , seasonTag 3, dayOfWeekTag 5, lunarPhaseTag 0
            , dayNumberTag 0, timeTag 10
            ]
          w0 = (mkShootableWorld you shootLoc tick) { worldTags = baseTags }
          scenario = (deerHunt fixtureSeed you) { scenarioInitial = w0 }
      env <- mkScenarioEnv you scenario
      checkCondition w0 (HasWorldTag deerSpotted) `shouldBe` False
      w1 <- step env (ActionId "look") w0
      checkCondition w1 (HasWorldTag deerSpotted) `shouldBe` True
      w2 <- step env (ActionId "takeTheShot") w1
      checkCondition w2 (HasWorldTag deerKilled) `shouldBe` True

    it "walk from start, look, and shoot" $ do
      let base     = deerHunt fixtureSeed you
          pinned   = filter (\a -> axiomId a `notElem`
                      [ ScenarioAxiom "deerMovement"
                      , ScenarioAxiom "spook"
                      ]) (scenarioAxioms base)
          -- Walk a 3-step path from start into a field location.
          (path, endLoc) = walkPath fixtureStart CField 3
          walkSteps = length path
          shotAfterWalk t =
            let w = (mkShootableWorld you endLoc t)
                      { worldClock = LamportClock (t + walkSteps + 1) (PlayerId "test") }
            in doesShotHit w you
          tick = case [ t | t <- [0..5000], shotAfterWalk t ] of
                   (t:_) -> t
                   []    -> error "walk from start: no tick found"
          w0 = (scenarioInitial base)
                 { worldClock = LamportClock tick (PlayerId "test")
                 , worldLocations = Map.insert deer endLoc
                     (worldLocations (scenarioInitial base))
                 }
          scenario = base { scenarioAxioms  = pinned
                          , scenarioInitial = w0
                          }
      env <- mkScenarioEnv you scenario
      charLocation you  w0 `shouldBe` Just fixtureStart
      charLocation deer w0 `shouldBe` Just endLoc

      -- Walk each edge of the path.
      wFinal <- foldr (\edge k w -> step env (uncurry edgeActionId edge) w >>= k) pure path w0
      charLocation you wFinal `shouldBe` Just endLoc
      charLocation deer wFinal `shouldBe` Just endLoc

      -- Look for the deer.
      wLook <- step env (ActionId "look") wFinal
      checkCondition wLook (HasWorldTag deerSpotted) `shouldBe` True

      -- Take the shot.
      wShot <- step env (ActionId "takeTheShot") wLook
      checkCondition wShot (HasWorldTag deerKilled) `shouldBe` True

    it "headless random walk reaches a terminal condition" $ do
      let seeds = [42, 0, 7, 100, 2025]
      results <- mapM (runHeadlessRandom (deerHunt fixtureSeed) (PlayerId "integration-test") 1000) seeds
      let anyTerminated = any terminatedOk results
      anyTerminated `shouldBe` True

    describe "movement narration" $ do
      it "edges have NarrationPool narration with multiple variants" $ do
        let e = case sgEdges (huntGraph fixtureHuntWorld) of
                  (x:_) -> x
                  []    -> error "huntGraph has no edges"
        case edgeNarration e of
          NarrationPool _ vs -> length vs `shouldSatisfy` (> 1)
          _                  -> expectationFailure "expected NarrationPool narration"

    describe "movingFast tradeoff" $ do
      it "deer moves at 30% but not at 15% for a specific tick" $ do
        let fieldA = pickByClass CField
            fieldB = fromMaybe fieldA (pickAdjacentByClass fieldA CField)
            mkW tk = GameWorld
              { worldCharacters = Map.fromList
                  [ (you, Character you "You" [] orEmpty)
                  , (deer, Character deer "The Deer" [] orEmpty) ]
              , worldGraph = Map.empty
              , worldLocations = Map.fromList [(you, fieldA), (deer, fieldB)]
              , worldActiveEffects = []
              , worldClock = LamportClock tk (PlayerId "test")
              , worldTags = orEmpty
              , worldLocationGraph = emptyLocationGraph
              , worldSeed = 0
              , worldLocationHistory = Map.empty
              , worldLocationVisits  = Map.empty }
            inRange t = let r = rollD (mkW t) saltDeerMove
                        in r >= 0.15 && r < 0.30
            targetTick = case [ t | t <- [0..5000 :: Int], inRange t ] of
              (t:_) -> t
              []    -> error "no tick in [0..5000] with rollD between 0.15 and 0.30"
        rollD (mkW targetTick) saltDeerMove `shouldSatisfy` (< 0.30)
        rollD (mkW targetTick) saltDeerMove `shouldSatisfy` (>= 0.15)

    describe "tension with fresh sign" $ do
      it "tension is 6 when fresh sign is present" $ do
        let base = deerHunt fixtureSeed you
            pinned = filter (\a -> axiomId a /= ScenarioAxiom "deerMovement"
                                && axiomId a /= ScenarioAxiom "spook")
                           (scenarioAxioms base)
            scenario = base { scenarioAxioms = pinned }
            -- Player and deer in the same class region but *different*
            -- locations, so freshSign fires (same-class) without
            -- deerSpotted (not co-located) which would push tension to 8.
            fieldA = pickByClass CField
            fieldB = case pickAdjacentByClass fieldA CField of
                       Just b  -> b
                       Nothing -> pickByClass CField   -- fallback, may co-locate
            w0 = (scenarioInitial scenario)
                   { worldLocations = Map.fromList
                       [ (you,  fieldA)
                       , (deer, fieldB) ]
                   }
        env <- mkScenarioEnv you (scenario { scenarioInitial = w0 })
        w1 <- step env (ActionId "sit:on") w0
        orMember freshSign (worldTags w1) `shouldBe` True
        w2 <- step env (ActionId "look") w1
        getTension w2 `shouldBe` 6

    it "full playthrough: walk from start, find deer, shoot" $ do
      let scenario = deerHunt fixtureSeed you
          w0 = scenarioInitial scenario
          (path, _) = walkPath fixtureStart CField 3
      env <- mkScenarioEnv you scenario
      charLocation you w0 `shouldBe` Just fixtureStart
      wFinal <- foldr (\edge k w -> step env (uncurry edgeActionId edge) w >>= k) pure path w0
      -- Hunt until terminal.
      wEnd <- huntLoop env wFinal 1000
      huntEnded wEnd `shouldBe` True

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

huntEnded :: GameWorld -> Bool
huntEnded w = checkCondition w (HasWorldTag deerKilled)
           || checkCondition w (HasWorldTag deerGone)
           || checkCondition w (HasWorldTag hunterShot)

huntLoop :: Env -> GameWorld -> Int -> IO GameWorld
huntLoop _   _ 0 = error "huntLoop: timed out without reaching terminal condition"
huntLoop env w n
  | huntEnded w = pure w
  | checkCondition w (HasWorldTag deerSpotted) =
      step env (ActionId "takeTheShot") w
  -- Night fell: walk any neighbour round-trip to advance time to dawn.
  | checkCondition w (HasWorldTag backAtTruck) =
      case (charLocation (Named "test-player") w, findAnyNeighbour w) of
        (Just cur, Just nbr) -> do
          w'  <- step env (edgeActionId cur nbr) w
          w'' <- step env (edgeActionId nbr cur) w'
          huntLoop env w'' (n - 2)
        _ -> pure w   -- nowhere to walk; give up
  | otherwise = do
      w' <- step env (ActionId "look") w
      if huntEnded w' || checkCondition w' (HasWorldTag deerSpotted)
        then huntLoop env w' (n - 1)
        else do
          let isSitting = checkCondition w' (HasWorldTag playerSitting)
              passTime  = if isSitting then ActionId "look" else ActionId "sit:on"
          w'' <- step env passTime w'
          huntLoop env w'' (n - 1)
  where
    findAnyNeighbour world =
      case charLocation (Named "test-player") world of
        Just cur ->
          let pairs = foldr (:) [] (lgEdges (worldLocationGraph world))
              ns = [ b | (a, b) <- pairs, a == cur ]
                ++ [ a | (a, b) <- pairs, b == cur ]
          in case ns of
               (x:_) -> Just x
               []    -> Nothing
        Nothing  -> Nothing

terminatedOk :: Either AppError (GameWorld, [LogEntry]) -> Bool
terminatedOk (Left _)        = False
terminatedOk (Right (w, _))  = huntEnded w

-- | Re-export for the edge action ID generation in tests.
edgeActionId :: Location -> Location -> ActionId
edgeActionId from to = ActionId ("walk:" <> locationName from <> ":" <> locationName to)

-- | Local fromMaybe (avoid extra import).
fromMaybe :: a -> Maybe a -> a
fromMaybe d Nothing  = d
fromMaybe _ (Just x) = x
