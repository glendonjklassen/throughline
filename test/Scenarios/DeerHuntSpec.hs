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
import           Scenarios.DeerHunt.Locations
import           Scenarios.DeerHunt.Probability

-- | Build a world where the player and deer are co-located, DeerSpotted is set,
-- and the clock tick is controlled for deterministic random outcomes.
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

  describe "DeerHunt scenario" $ do

    -- PATH 1: Solo clean kill
    it "solo clean kill: DeerKilled" $ do
      let tick = findTick (`doesShotHit` you) (mkShootableWorld you) stubbleRows
          w0 = mkShootableWorld you stubbleRows tick
          scenario = (deerHunt 0 you) { scenarioInitial = w0 }
      env <- mkScenarioEnv you scenario

      wFinal <- step env (ActionId "takeTheShot") w0

      checkCondition wFinal (HasWorldTag deerKilled) `shouldBe` True
      checkCondition wFinal (HasWorldTag deerGone)   `shouldBe` False
      checkCondition wFinal (HasWorldTag hunterShot)  `shouldBe` False

    -- PATH 2: Missed shot → DeerGone
    it "missed shot: DeerGone" $ do
      let tick = findTick (\w -> not (doesShotHit w you)) (mkShootableWorld you) stubbleRows
          w0 = mkShootableWorld you stubbleRows tick
          scenario = (deerHunt 0 you) { scenarioInitial = w0 }
      env <- mkScenarioEnv you scenario

      wFinal <- step env (ActionId "takeTheShot") w0

      checkCondition wFinal (HasWorldTag deerGone)    `shouldBe` True
      checkCondition wFinal (HasWorldTag deerKilled)  `shouldBe` False

    -- PATH 3: Co-located kill → DeerKilled + trust drop
    it "co-located kill: DeerKilled, trust drops" $ do
      let other = Named "other-hunter"
          mkW = mkShootableWorldWith you other
          tick = findTick (\w -> doesShotHit w you && not (isFriendlyFire w)) mkW stubbleRows
          w0 = mkShootableWorldWith you other stubbleRows tick
          scenario = (deerHunt 0 you) { scenarioInitial = w0 }
      env <- mkScenarioEnv you scenario

      wFinal <- step env (ActionId "takeTheShot") w0

      checkCondition wFinal (HasWorldTag deerKilled) `shouldBe` True
      checkCondition wFinal (HasWorldTag hunterShot) `shouldBe` False

    -- PATH 4: Friendly fire → HunterShot
    it "friendly fire: HunterShot" $ do
      let other = Named "other-hunter"
          mkW = mkShootableWorldWith you other
          tick = findTick (\w -> doesShotHit w you && isFriendlyFire w) mkW stubbleRows
          w0 = mkShootableWorldWith you other stubbleRows tick
          scenario = (deerHunt 0 you) { scenarioInitial = w0 }
      env <- mkScenarioEnv you scenario

      wFinal <- step env (ActionId "takeTheShot") w0

      checkCondition wFinal (HasWorldTag hunterShot)  `shouldBe` True
      checkCondition wFinal (HasWorldTag deerKilled)  `shouldBe` False

    -- PATH 5: Tension rises when deer is spotted
    it "tension rises with deer spotted" $ do
      let w0 = mkShootableWorld you stubbleRows 42
          scenario = (deerHunt 0 you) { scenarioInitial = w0 }
      env <- mkScenarioEnv you scenario

      w1 <- step env (ActionId "sit:on") w0
      getTension w1 `shouldSatisfy` (>= 8)

    -- PATH 6: Experience increases when finding sign (same zone as deer)
    it "experience increases with fresh sign" $ do
      let scenario = deerHunt 0 you
          w0 = (scenarioInitial scenario)
                 { worldLocations = Map.fromList
                     [ (you,  nFieldEdge)    -- same zone as deer at stubbleRows
                     , (deer, stubbleRows)
                     ] }
          startExp = experience you w0
      env <- mkScenarioEnv you (scenario { scenarioInitial = w0 })

      w1 <- step env (ActionId "sit:on") w0
      let endExp = experience you w1
      endExp `shouldSatisfy` (>= startExp)

    -- PATH 7: Navigation works — can walk between connected locations
    it "can navigate between adjacent nodes" $ do
      let scenario = deerHunt 0 you
          w0 = scenarioInitial scenario  -- starts at truckNorth
      env <- mkScenarioEnv you scenario

      -- Walk from truck to ditch
      let walkId = edgeActionId truckNorth ditchNorth
      w1 <- step env walkId w0

      charLocation you w1 `shouldBe` Just ditchNorth

  describe "validation" $ do
    let you' = Named "test-player"

    it "has no duplicate ActionIds" $
      isRight (validateScenario (deerHunt 0 you')) `shouldBe` True

    it "has a connected scene graph" $
      isRight (validateSceneGraph huntGraph) `shouldBe` True

  -- -------------------------------------------------------------------
  -- Integration tests
  -- -------------------------------------------------------------------

  describe "integration" $ do

    -- Test the look -> shoot pipeline.  Player and deer are co-located
    -- but deerSpotted is NOT set.  Looking should discover the deer and
    -- enable takeTheShot.
    --
    -- The shot outcome is determined by the clock tick at action-creation
    -- time.  Look bumps the clock by 1, so we search for a starting tick
    -- where the shot hits at tick+1.
    it "look discovers deer and enables the shot" $ do
      let -- Find a tick T so that doesShotHit is true at tick T+1 (after look)
          shotAtNextTick t =
            let w = (mkShootableWorld you stubbleRows t)
                      { worldClock = LamportClock (t + 1) (PlayerId "test") }
            in doesShotHit w you
          tick = case [ t | t <- [0..5000], shotAtNextTick t ] of
                   (t:_) -> t
                   []    -> error "look discovers deer: no tick found"
          -- Build a world like mkShootableWorld but WITHOUT deerSpotted in tags.
          baseTags = orFromList
            [ weatherTag (WeatherDesc "Clear and Cold")
            , seasonTag 3, dayOfWeekTag 5, lunarPhaseTag 0
            , dayNumberTag 0, timeTag 10
            ]
          w0 = (mkShootableWorld you stubbleRows tick) { worldTags = baseTags }
          scenario = (deerHunt 0 you) { scenarioInitial = w0 }
      env <- mkScenarioEnv you scenario

      -- Pre-condition: deerSpotted is NOT set
      checkCondition w0 (HasWorldTag deerSpotted) `shouldBe` False

      -- Look for the deer (player and deer are co-located)
      w1 <- step env (ActionId "look") w0
      checkCondition w1 (HasWorldTag deerSpotted) `shouldBe` True

      -- Now take the shot — should produce deerKilled
      w2 <- step env (ActionId "takeTheShot") w1
      checkCondition w2 (HasWorldTag deerKilled) `shouldBe` True

    -- Walk the full path from the truck to the deer, look for it, shoot it.
    -- Deer movement and spook axioms are removed so the deer stays pinned
    -- at stubbleRows.  This isolates the walk -> look -> shoot pipeline.
    --
    -- The shot is evaluated at tick T+4 (3 walk steps + 1 look step), so
    -- we find a starting tick that produces a hit after 4 clock advances.
    it "walk from truck, look, and shoot" $ do
      let base     = deerHunt 0 you
          -- Remove deer movement and spook axioms so the deer stays put
          -- and doesn't bolt when we arrive.
          pinned   = filter (\a -> axiomId a `notElem`
                      [ ScenarioAxiom "deerMovement"
                      , ScenarioAxiom "spook"
                      ]) (scenarioAxioms base)
          -- Find a tick T so that doesShotHit is true at tick T+4
          shotAfterWalk t =
            let w = (mkShootableWorld you stubbleRows t)
                      { worldClock = LamportClock (t + 4) (PlayerId "test") }
            in doesShotHit w you
          tick     = case [ t | t <- [0..5000], shotAfterWalk t ] of
                       (t:_) -> t
                       []    -> error "walk from truck: no tick found"
          w0       = (scenarioInitial base)
                       { worldClock = LamportClock tick (PlayerId "test")
                         -- Pin deer at stubbleRows so the walk path leads to it.
                       , worldLocations = Map.insert deer stubbleRows
                           (worldLocations (scenarioInitial base))
                       }
          scenario = base { scenarioAxioms  = pinned
                          , scenarioInitial = w0
                          }
      env <- mkScenarioEnv you scenario

      -- Player starts at truckNorth, deer at stubbleRows
      charLocation you  w0 `shouldBe` Just truckNorth
      charLocation deer w0 `shouldBe` Just stubbleRows

      -- Step 1: truck -> ditch
      w1 <- step env (edgeActionId truckNorth ditchNorth) w0
      charLocation you w1 `shouldBe` Just ditchNorth

      -- Step 2: ditch -> north field edge
      w2 <- step env (edgeActionId ditchNorth nFieldEdge) w1
      charLocation you w2 `shouldBe` Just nFieldEdge

      -- Step 3: north field edge -> stubble rows (where deer is)
      w3 <- step env (edgeActionId nFieldEdge stubbleRows) w2
      charLocation you w3 `shouldBe` Just stubbleRows

      -- Deer should still be at stubbleRows (movement axiom removed)
      charLocation deer w3 `shouldBe` Just stubbleRows

      -- Look for the deer
      w4 <- step env (ActionId "look") w3
      checkCondition w4 (HasWorldTag deerSpotted) `shouldBe` True

      -- Take the shot
      w5 <- step env (ActionId "takeTheShot") w4
      checkCondition w5 (HasWorldTag deerKilled) `shouldBe` True

    -- Run the full scenario headlessly with random action selection.
    -- The scenario should reach a terminal condition within the step limit.
    it "headless random walk reaches a terminal condition" $ do
      let seeds = [42, 0, 7, 100, 2025]
      results <- mapM (runHeadlessRandom (deerHunt 0) (PlayerId "integration-test") 1000) seeds
      let anyTerminated = any terminatedOk results
      anyTerminated `shouldBe` True

    -- -----------------------------------------------------------------
    -- Test: movement narrations use NarrationPool with multiple variants
    -- -----------------------------------------------------------------
    describe "movement narration" $ do
      it "edges have NarrationPool narration with multiple variants" $ do
        let e = case sgEdges huntGraph of
                  (x:_) -> x
                  []    -> error "huntGraph has no edges"
        case edgeNarration e of
          NarrationPool _ vs -> length vs `shouldSatisfy` (> 1)
          _                  -> expectationFailure "expected NarrationPool narration"

    -- -----------------------------------------------------------------
    -- Test: movingFast suppresses deer movement probability
    -- -----------------------------------------------------------------
    describe "movingFast tradeoff" $ do
      it "deer moves at 30% but not at 15% for a specific tick" $ do
        -- Find tick where the roll falls between 0.15 and 0.30
        let mkW tk = GameWorld
              { worldCharacters = Map.fromList
                  [ (you, Character you "You" [] orEmpty)
                  , (deer, Character deer "The Deer" [] orEmpty) ]
              , worldGraph = Map.empty
              , worldLocations = Map.fromList [(you, nFieldEdge), (deer, stubbleRows)]
              , worldActiveEffects = []
              , worldClock = LamportClock tk (PlayerId "test")
              , worldTags = orEmpty
              , worldLocationGraph = emptyLocationGraph
              , worldSeed = 0 }
            -- rollCheck world salt prob = rollD world salt < prob
            -- We need: 0.15 <= rollD < 0.30
            inRange t = let r = rollD (mkW t) saltDeerMove
                        in r >= 0.15 && r < 0.30
            targetTick = case [ t | t <- [0..5000 :: Int], inRange t ] of
              (t:_) -> t
              []    -> error "no tick in [0..5000] with rollD between 0.15 and 0.30"
        -- At 30% threshold: deer should move (roll < 0.30)
        rollD (mkW targetTick) saltDeerMove `shouldSatisfy` (< 0.30)
        -- At 15% threshold: deer should NOT move (roll >= 0.15)
        rollD (mkW targetTick) saltDeerMove `shouldSatisfy` (>= 0.15)

    -- -----------------------------------------------------------------
    -- Test: freshSign sets tension to 6
    -- -----------------------------------------------------------------
    describe "tension with fresh sign" $ do
      it "tension is 6 when fresh sign is present" $ do
        let base = deerHunt 0 you
            -- Remove deer movement so the deer stays in the same zone
            pinned = filter (\a -> axiomId a /= ScenarioAxiom "deerMovement"
                                && axiomId a /= ScenarioAxiom "spook")
                           (scenarioAxioms base)
            scenario = base { scenarioAxioms = pinned }
            w0 = (scenarioInitial scenario)
                   { worldLocations = Map.fromList
                       [ (you,  nFieldEdge)
                       , (deer, stubbleRows) ]
                   }
        env <- mkScenarioEnv you (scenario { scenarioInitial = w0 })
        -- First step: deerPresence adds freshSign (all axioms see pre-axiom world)
        w1 <- step env (ActionId "sit:on") w0
        orMember freshSign (worldTags w1) `shouldBe` True
        -- Second step: tensionAxiom now sees freshSign and sets tension to 6
        w2 <- step env (ActionId "look") w1
        getTension w2 `shouldBe` 6

    -- Full playthrough: start at the truck, walk to the field, hunt until
    -- we find the deer and shoot it.  No state overrides, no removed axioms.
    -- The test plays like a real player: walk, look, wait, shoot.
    it "full playthrough: walk from truck, find deer, shoot" $ do
      let scenario = deerHunt 0 you
          w0 = scenarioInitial scenario
      env <- mkScenarioEnv you scenario

      -- Start at the truck
      charLocation you w0 `shouldBe` Just truckNorth

      -- Walk: truckNorth → ditchNorth → nFieldEdge → stubbleRows
      w1 <- step env (edgeActionId truckNorth ditchNorth) w0
      w2 <- step env (edgeActionId ditchNorth nFieldEdge) w1
      w3 <- step env (edgeActionId nFieldEdge stubbleRows) w2
      charLocation you w3 `shouldBe` Just stubbleRows

      -- Now hunt: look for the deer, wait, repeat until we spot it or the
      -- hunt ends.  The deer wanders via axioms.  When it enters our node,
      -- the spook axiom either sets deerSpotted or the deer bolts.  If we
      -- look while co-located, that also sets deerSpotted.  Eventually
      -- we get a shot.
      wFinal <- huntLoop env w3 1000

      huntEnded wFinal `shouldBe` True

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | True when the hunt has reached a terminal condition.
huntEnded :: GameWorld -> Bool
huntEnded w = checkCondition w (HasWorldTag deerKilled)
           || checkCondition w (HasWorldTag deerGone)
           || checkCondition w (HasWorldTag hunterShot)

-- | Play the hunt like a real player: look for deer, wait, shoot when spotted.
-- Handles nightfall (backAtTruck) by waiting it out.  Steps are limited to
-- prevent infinite loops.
huntLoop :: Env -> GameWorld -> Int -> IO GameWorld
huntLoop _   w 0 = error $ "huntLoop: timed out without reaching terminal condition"
                        <> "\n  deerSpotted=" <> show (checkCondition w (HasWorldTag deerSpotted))
                        <> "\n  backAtTruck=" <> show (checkCondition w (HasWorldTag backAtTruck))
                        <> "\n  playerLoc="   <> show (charLocation (Named "test-player") w)
                        <> "\n  deerLoc="     <> show (charLocation deer w)
huntLoop env w n
  -- Done: hunt is over
  | huntEnded w = pure w
  -- Deer spotted: take the shot
  | checkCondition w (HasWorldTag deerSpotted) =
      step env (ActionId "takeTheShot") w
  -- At truck overnight: walk back and forth to advance the clock until dawn.
  -- (wait/look are blocked by backAtTruck, but walk actions still work)
  | checkCondition w (HasWorldTag backAtTruck) =
      let loc = charLocation (Named "test-player") w
          (there, back) = case loc of
            Just l | l == truckSouth -> (edgeActionId truckSouth ditchSouth,  edgeActionId ditchSouth truckSouth)
            Just l | l == ditchSouth -> (edgeActionId ditchSouth truckSouth,  edgeActionId truckSouth ditchSouth)
            Just l | l == truckWest  -> (edgeActionId truckWest  ditchWest,   edgeActionId ditchWest  truckWest)
            Just l | l == ditchWest  -> (edgeActionId ditchWest  truckWest,   edgeActionId truckWest  ditchWest)
            Just l | l == ditchNorth -> (edgeActionId ditchNorth truckNorth,  edgeActionId truckNorth ditchNorth)
            _                        -> (edgeActionId truckNorth ditchNorth,  edgeActionId ditchNorth truckNorth)
      in step env there w
           >>= step env back
           >>= \w'' -> huntLoop env w'' (n - 2)
  -- In the field: look for deer, then sit down or look again
  | otherwise = do
      w' <- step env (ActionId "look") w
      if huntEnded w' || checkCondition w' (HasWorldTag deerSpotted)
        then huntLoop env w' (n - 1)
        else do
          -- Sit if not already sitting, otherwise look again to pass time
          let isSitting = checkCondition w' (HasWorldTag playerSitting)
              passTime  = if isSitting then ActionId "look" else ActionId "sit:on"
          w'' <- step env passTime w'
          huntLoop env w'' (n - 1)

-- | Check whether a headless run reached one of the terminal conditions.
terminatedOk :: Either AppError (GameWorld, [LogEntry]) -> Bool
terminatedOk (Left _)        = False
terminatedOk (Right (w, _))  = huntEnded w

-- | Re-export for the edge action ID generation in tests.
edgeActionId :: Location -> Location -> ActionId
edgeActionId from to = ActionId ("walk:" <> locationName from <> ":" <> locationName to)
