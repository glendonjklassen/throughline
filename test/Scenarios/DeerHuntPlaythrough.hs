-- | Narrated DeerHunt playthrough — run with: stack test --ta '-m "DeerHunt playthrough"'
module Scenarios.DeerHuntPlaythrough (spec) where

import           Data.List              (isInfixOf)
import qualified Data.Map.Strict        as Map
import           Test.Hspec

import           Engine.Author.Scene    (edgeActionId)
import           Engine.Core.Conditions (checkCondition)
import           Engine.Core.NarrativeMessage
import           Engine.CRDT.ORSet      (orFromList, orEmpty)
import           Engine.Core.World      (setCharacterStat)
import           Engine.Core.Effects    (mergeWorlds)
import qualified Engine.Headless
import           Engine.Headless        (TurnRecord(..), runHeadlessPlaythrough)
import           Engine.Sync.Causality  (buildMergeDiff, runMergeAxioms)
import           Engine.Core.Axioms.Merge (systemMergeAxioms)
import           Engine.Sync.EventLog   (mergeLogs, replayFrom)
import           GameTypes
import           MonadStack              (AppError)
import           Scenarios.DeerHunt     (deerHunt)
import           Scenarios.DeerHunt.Axioms (hunterArrivalMergeAxiom)
import           Scenarios.DeerHunt.Constants
import           Scenarios.DeerHunt.Generation (TerrainClass(..))
import           Scenarios.DeerHunt.Probability (doesShotHit, isFriendlyFire)
import           Scenarios.DeerHuntTestFixtures

-- ---------------------------------------------------------------------------
-- Formatting
-- ---------------------------------------------------------------------------

formatMsg :: NarrativeEntry -> String
formatMsg ne = case neMessage ne of
  MsgNarrate text        -> "    > " ++ text
  MsgEffect  text        -> "      " ++ text
  MsgSay _ name _ _ text -> "    " ++ name ++ ": " ++ text
  MsgThink _ text        -> "    ~ " ++ text
  MsgDialogue lines'     -> unlines
    [ "    " ++ name ++ ": " ++ text | (_, name, _, _, text) <- lines' ]

formatTurn :: Int -> TurnRecord -> String
formatTurn n tr = unlines $
  [ ""
  , "  Turn " ++ show n ++ ":"
  , "    Available:"
  ] ++
  [ "      " ++ show i ++ ". " ++ a | (i, a) <- zip [(1::Int)..] (turnAvailable tr) ] ++
  [ "    Chosen: " ++ turnChosen tr
  , ""
  ] ++
  map formatMsg (turnMessages tr)

formatPlaythrough :: [TurnRecord] -> String
formatPlaythrough = concatMap (uncurry formatTurn) . zip [1..]

-- ---------------------------------------------------------------------------
-- Player identities
-- ---------------------------------------------------------------------------

pidA, pidB :: PlayerId
pidA = PlayerId "hunter-alpha"
pidB = PlayerId "hunter-bravo"

youA, youB :: CharacterId
youA = Named (take 12 "hunter-alpha")
youB = Named (take 12 "hunter-bravo")

-- ---------------------------------------------------------------------------
-- Walk scripts — derived from the canonical fixture map.
-- ---------------------------------------------------------------------------

fieldWalk :: ([ActionId], Location)
fieldWalk =
  let (path, endLoc) = walkPath fixtureStart CField 3
  in (map (uncurry edgeActionId) path, endLoc)

walkScript :: [ActionId]
walkScript = fst fieldWalk

fieldEnd :: Location
fieldEnd = snd fieldWalk

-- ---------------------------------------------------------------------------
-- Pinned deer (remove deer movement and spook so the deer stays put)
-- ---------------------------------------------------------------------------

pinnedDeerHunt :: CharacterId -> Scenario
pinnedDeerHunt you =
  let base = deerHunt fixtureSeed you
      pinned = filter (\a -> axiomId a `notElem`
                  [ ScenarioAxiom "deerMovement"
                  , ScenarioAxiom "spook"
                  , ScenarioAxiom "dayRollover"
                  ]) (scenarioAxioms base)
      w0 = scenarioInitial base
      w0pinned = w0 { worldLocations = Map.insert deer fieldEnd (worldLocations w0) }
  in base { scenarioAxioms = pinned, scenarioInitial = w0pinned }

-- | Find a clock tick where the shot hits.
findHitTick :: CharacterId -> Int -> Int
findHitTick you offset =
  case [ t | t <- [0..5000]
           , let w = GameWorld
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
                   , worldLocations = Map.fromList [(you, fieldEnd), (deer, fieldEnd)]
                   , worldActiveEffects = []
                   , worldClock = LamportClock (t + offset) (PlayerId "test")
                   , worldTags = orFromList
                       [ deerSpotted
                       , weatherTag (WeatherDesc "Clear and Cold")
                       , seasonTag 3, dayOfWeekTag 5, lunarPhaseTag 0
                       , dayNumberTag 0, timeTag 10
                       ]
                   , worldLocationGraph = emptyLocationGraph
                   , worldSeed = 0
                   , worldLocationHistory = Map.empty
                   , worldLocationVisits  = Map.empty
                   , worldJournal         = []
                   , worldDayNumber       = 1
                   }
           , doesShotHit w you
           , not (isFriendlyFire w)
           ] of
    (t:_) -> t
    []    -> error "findHitTick: no tick in [0..5000] produced a hit without friendly fire"

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "DeerHunt playthrough (narrated)" $ do

  it "solo hunt — walk, spot, shoot" $ do
    let tick = findHitTick youA 4
        script = walkScript ++ [ActionId "look", ActionId "takeTheShot"]
        hunt you =
          let base = pinnedDeerHunt you
              w0   = scenarioInitial base
          in base { scenarioInitial = w0 { worldClock = LamportClock tick (PlayerId "test") } }

    Right (world, turns) <- runHeadlessPlaythrough hunt pidA script

    putStrLn "\n\n=== SOLO DEER HUNT ==="
    putStrLn $ formatPlaythrough turns

    checkCondition world (HasWorldTag deerKilled) `shouldBe` True

  it "two-hunter merge — stranger arrival at same location" $ do
    Right (worldA, turnsA) <- runHeadlessPlaythrough pinnedDeerHunt pidA walkScript
    Right (_worldB, turnsB) <- runHeadlessPlaythrough pinnedDeerHunt pidB walkScript

    putStrLn "\n\n=== HUNTER A (solo) ==="
    putStrLn $ formatPlaythrough turnsA

    putStrLn "=== HUNTER B (solo) ==="
    putStrLn $ formatPlaythrough turnsB

    let lastTurnA = last turnsA
    lastTurnA `shouldSatisfy` (notElem "Wave to the other hunter." . turnAvailable)

    let scenA = pinnedDeerHunt youA
        scenB = pinnedDeerHunt youB

    Right (_, logA) <- runHeadlessScript' pinnedDeerHunt pidA walkScript
    Right (_, logB) <- runHeadlessScript' pinnedDeerHunt pidB walkScript

    let base  = mergeWorlds (scenarioInitial scenA) (scenarioInitial scenB)
        (_, divergent) = mergeLogs logA logB

    Right mergedWorld <- replayFrom scenA base divergent

    let md = buildMergeDiff pidA logA divergent worldA mergedWorld
        allMergeAx = systemMergeAxioms ++ [hunterArrivalMergeAxiom youA]
        mergeEffects = runMergeAxioms allMergeAx mergedWorld md
        mergeNarration = concatMap showEffect mergeEffects
        showEffect (Effect (Narrate text) _ _ _) = ["    > " ++ text]
        showEffect _ = []

    putStrLn "=== MERGE (from Hunter A's perspective) ===\n"
    mapM_ putStrLn mergeNarration

    let actionsAfter = map anyActionLabel
          $ filter (checkCondition mergedWorld . anyActionCondition)
          $ scenarioActions scenA

    putStrLn "\n    Available after merge:"
    mapM_ (\a -> putStrLn $ "      - " ++ a) actionsAfter
    putStrLn ""

    actionsAfter `shouldSatisfy` elem "Wave to the other hunter."

    let hitBase = findHitTick youA 2
        shootWorld = mergedWorld { worldClock = LamportClock hitBase pidA }
        postMergeHunt who = (pinnedDeerHunt who) { scenarioInitial = shootWorld }

    Right (_, waveTurnsB) <- runHeadlessPlaythrough postMergeHunt pidB [ActionId "wave"]

    putStrLn "=== Hunter B waves ==="
    putStrLn $ formatPlaythrough waveTurnsB

    Right (finalWorld, shootTurns) <- runHeadlessPlaythrough postMergeHunt pidA
      [ActionId "wave", ActionId "look", ActionId "takeTheShot"]

    putStrLn "=== Hunter A waves, spots, and shoots ==="
    putStrLn $ formatPlaythrough shootTurns

    let hasHunterNarrate (Effect (Narrate msg) _ _ _) = "another hunter" `isInfixOf` msg
        hasHunterNarrate _                            = False
    any hasHunterNarrate mergeEffects `shouldBe` True
    checkCondition finalWorld (HasWorldTag deerKilled) `shouldBe` True

-- | Thin wrapper: we need logs for the merge but runHeadlessPlaythrough
-- doesn't return them. Reuse the existing narrated runner for that.
runHeadlessScript'
  :: (CharacterId -> Scenario) -> PlayerId -> [ActionId]
  -> IO (Either AppError (GameWorld, [LogEntry]))
runHeadlessScript' mkScenario pid script = do
  result <- Engine.Headless.runHeadlessNarrated mkScenario pid script
  case result of
    Left err              -> pure (Left err)
    Right (w, entries, _) -> pure (Right (w, entries))
