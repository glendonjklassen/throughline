-- =============================================================================
-- Scenarios.CoLocationSpec
--
-- Tests that verify cross-scenario co-location interactions after merging
-- independent play sessions. The tests pair the TopBuy scenario (a retail
-- store employee on the sales floor) with the Customer scenario (a shopper
-- navigating the store).
--
-- The greetCustomer action requires both the TopBuy player and the customer
-- to be at salesFloor simultaneously. After merging their independent
-- sessions, whether greetCustomer is available depends on the customer's
-- final position.
--
-- Both merge paths are tested:
--   1. Snapshot merge: CRDT union of two final GameWorlds (mergeWorlds)
--   2. Log merge: merge event logs (mergeLogs), then replay from a shared
--      base world (replayFrom)
-- Both paths must agree on the resulting world state and action availability.
-- =============================================================================
module Scenarios.CoLocationSpec (spec) where

import           Test.Hspec

import           Engine.Author.Scene       (edgeActionId)
import           Engine.Core.Conditions    (checkCondition)
import           Engine.Core.Effects       (mergeWorlds)
import           Engine.Headless           (runHeadlessScript)
import           Engine.Sync.Causality     (buildMergeDiff)
import           Engine.Sync.EventLog      (mergeLogs, replayFrom)
import           GameTypes
import           Scenarios.Customer        (customer)
import           Scenarios.TopBuy          (topBuy)
import           Scenarios.TopBuy.Locations
import           Scenarios.TopBuy.SalesFloorScene (greetCustomer)

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do

  -- =========================================================================
  -- Co-location interactions
  --
  -- These tests pair two DIFFERENT scenarios: TopBuy (retail store employee
  -- on the sales floor) and Customer (a shopper navigating the store).
  -- The greetCustomer action requires both the TopBuy player and the
  -- customer to be at salesFloor simultaneously. After merging their
  -- independent sessions, whether greetCustomer is available depends on
  -- the customer's final position.
  -- =========================================================================

  describe "co-location interactions" $ do

    -- Player identities for the two scenarios.
    let tbPid   = PlayerId "topbuy-player"   -- TopBuy player's session ID
        custPid = PlayerId "customer-a"      -- Customer player's session ID
        tbYou   = Named (take 12 "topbuy-player")  -- TopBuy player's CharId
        custYou = Named (take 12 "customer-a")      -- Customer's CharId

        -- Snapshot merge path: run both scenarios independently, then CRDT-
        -- merge their final worlds. No replay involved — just a structural
        -- union of the two GameWorlds. This is the fast path.
        snapshotMerge tbScript custScript = do
          Right (tbWorld,   _) <- runHeadlessScript (topBuy 0)   tbPid tbScript
          Right (custWorld, _) <- runHeadlessScript (customer 0) custPid custScript
          pure (mergeWorlds tbWorld custWorld)

        -- Log merge path: merge the initial worlds of both scenarios as the
        -- base world (the divergence point — both players started here, then
        -- went their separate ways). Then merge their event logs and replay
        -- all divergent entries in Lamport order against that base.
        -- This mirrors the offerMerge flow in the sync protocol.
        logMerge tbScript custScript = do
          let scen     = topBuy 0 tbYou
              custScen = customer 0 custYou
              -- Base world: CRDT-merge of both scenarios' initial worlds.
              -- This is where the timelines diverged — both players started
              -- from this combined initial state, then acted independently.
              base     = mergeWorlds (scenarioInitial scen) (scenarioInitial custScen)
          Right (_, tbLog)   <- runHeadlessScript (topBuy 0)   tbPid tbScript
          Right (_, custLog) <- runHeadlessScript (customer 0) custPid custScript
          -- Merge the two logs in Lamport order. All entries are divergent
          -- because neither player synced with the other before acting.
          let (_, divergent) = mergeLogs tbLog custLog
          -- Replay the merged log from the base world.
          replayFrom scen base divergent

        -- canGreet evaluates greetCustomer's condition against a world.
        -- greetCustomer requires both the TopBuy player (tbYou) and the
        -- customer (custYou) to be at salesFloor. Returns True if the
        -- condition is satisfied, False otherwise.
        canGreet w = checkCondition w (actionCondition (greetCustomer tbYou custYou))

    -- -----------------------------------------------------------------------
    -- Customer left before merge
    --
    -- The customer walked: parking -> entrance -> salesFloor -> entrance ->
    -- parking. They visited the sales floor but left before the merge. The
    -- TopBuy player just waited (stayed on salesFloor). After merge,
    -- greetCustomer should be UNAVAILABLE because the customer is back in
    -- parking — they're no longer co-located.
    -- -----------------------------------------------------------------------

    describe "customer left before merge" $ do

      it "snapshot merge: greetCustomer unavailable" $ do
        w <- snapshotMerge
          -- TopBuy player waits (stays on salesFloor).
          [ActionId "wait"]
          -- Customer walks to salesFloor, then walks all the way back to parking.
          [edgeActionId parkingLot entrance, edgeActionId entrance salesFloor,
           edgeActionId salesFloor entrance, edgeActionId entrance parkingLot]
        -- Customer is in parking, not salesFloor. greetCustomer requires co-location.
        canGreet w `shouldBe` False

      it "log merge: greetCustomer unavailable" $ do
        result <- logMerge
          [ActionId "wait"]
          [edgeActionId parkingLot entrance, edgeActionId entrance salesFloor,
           edgeActionId salesFloor entrance, edgeActionId entrance parkingLot]
        case result of
          Left err -> expectationFailure ("merge failed: " <> show err)
          -- Same result via log merge: customer is gone, greeting impossible.
          Right w  -> canGreet w `shouldBe` False

      it "snapshot and log merge agree" $ do
        -- Both merge paths must reach the same conclusion about action
        -- availability. If they disagree, the merge semantics are broken.
        let tbScript   = [ActionId "wait"]
            custScript = [edgeActionId parkingLot entrance, edgeActionId entrance salesFloor,
                          edgeActionId salesFloor entrance, edgeActionId entrance parkingLot]
        sw     <- snapshotMerge tbScript custScript
        result <- logMerge      tbScript custScript
        case result of
          Left err -> expectationFailure ("merge failed: " <> show err)
          Right lw -> canGreet sw `shouldBe` canGreet lw

      -- Provenance assertion: since no sync occurred between the two players
      -- before their actions, ALL location deltas in the MergeDiff should be
      -- Unaware. Neither player knew about the other's state when they acted.
      -- This is correct — the customer walked around the store without any
      -- knowledge of the TopBuy player, and vice versa. Unaware is the
      -- expected baseline for unsynchronized merges.
      it "location deltas have Unaware provenance (no prior sync)" $ do
        -- Customer walks to salesFloor and back to parking.
        let custScript = [edgeActionId parkingLot entrance, edgeActionId entrance salesFloor,
                          edgeActionId salesFloor entrance, edgeActionId entrance parkingLot]
            scen     = topBuy 0 tbYou
            custScen = customer 0 custYou
            -- Base world: the combined starting state of both scenarios.
            base     = mergeWorlds (scenarioInitial scen) (scenarioInitial custScen)
        -- Run both players independently.
        Right (_, tbLog)   <- runHeadlessScript (topBuy 0)   tbPid [ActionId "wait"]
        Right (_, custLog) <- runHeadlessScript (customer 0) custPid custScript
        -- Merge logs and replay to get the final merged world.
        let (_, divergent) = mergeLogs tbLog custLog
        result <- replayFrom scen base divergent
        case result of
          Left err -> expectationFailure ("merge failed: " <> show err)
          Right mergedWorld ->
            -- buildMergeDiff computes what changed and annotates each delta
            -- with provenance. tbLog is "our" log, custLog is "foreign."
            -- Since the customer's frontier is empty (no prior sync), all
            -- their deltas are tagged Unaware.
            let md = buildMergeDiff tbPid tbLog custLog base mergedWorld
            in all (\d -> mdProvenance d == Unaware) (mergeLocations md)
                 `shouldBe` True

    -- -----------------------------------------------------------------------
    -- Customer still on floor at merge time
    --
    -- The customer walked: parking -> entrance -> salesFloor and stopped.
    -- The TopBuy player waited (stayed on salesFloor). After merge,
    -- greetCustomer should be AVAILABLE because both characters are on
    -- the sales floor simultaneously.
    -- -----------------------------------------------------------------------

    describe "customer still on floor at merge time" $ do

      it "snapshot merge: greetCustomer available" $ do
        w <- snapshotMerge
          -- TopBuy player waits (stays on salesFloor).
          [ActionId "wait"]
          -- Customer walks to salesFloor and stops there.
          [edgeActionId parkingLot entrance, edgeActionId entrance salesFloor]
        -- Both are on salesFloor. greetCustomer's co-location condition is met.
        canGreet w `shouldBe` True

      it "log merge: greetCustomer available" $ do
        result <- logMerge
          [ActionId "wait"]
          [edgeActionId parkingLot entrance, edgeActionId entrance salesFloor]
        case result of
          Left err -> expectationFailure ("merge failed: " <> show err)
          -- Same result via log merge: both on salesFloor, greeting possible.
          Right w  -> canGreet w `shouldBe` True

      it "snapshot and log merge agree" $ do
        -- Both merge paths must agree: greetCustomer is available via both.
        let tbScript   = [ActionId "wait"]
            custScript = [edgeActionId parkingLot entrance, edgeActionId entrance salesFloor]
        sw     <- snapshotMerge tbScript custScript
        result <- logMerge      tbScript custScript
        case result of
          Left err -> expectationFailure ("merge failed: " <> show err)
          Right lw -> canGreet sw `shouldBe` canGreet lw

      -- Provenance assertion: even though the customer IS on the sales floor
      -- (co-located with the TopBuy player), provenance is still Unaware.
      -- Provenance reflects what the foreign player KNEW when they acted, not
      -- where they ended up. The customer walked to the sales floor without
      -- knowing anything about the TopBuy player's state — no frontier was
      -- exchanged before acting. In an unsynchronized merge, all foreign
      -- deltas are Unaware regardless of the spatial outcome.
      it "location deltas have Unaware provenance (no prior sync, even when customer is present)" $ do
        -- Customer walks to salesFloor and stays.
        let custScript = [edgeActionId parkingLot entrance, edgeActionId entrance salesFloor]
            scen     = topBuy 0 tbYou
            custScen = customer 0 custYou
            -- Base world: combined starting state of both scenarios.
            base     = mergeWorlds (scenarioInitial scen) (scenarioInitial custScen)
        Right (_, tbLog)   <- runHeadlessScript (topBuy 0)   tbPid [ActionId "wait"]
        Right (_, custLog) <- runHeadlessScript (customer 0) custPid custScript
        let (_, divergent) = mergeLogs tbLog custLog
        result <- replayFrom scen base divergent
        case result of
          Left err -> expectationFailure ("merge failed: " <> show err)
          Right mergedWorld ->
            -- Even though the customer is right there on salesFloor, the
            -- MergeDiff still says Unaware — they didn't KNOW about the
            -- TopBuy player when they walked there. Provenance is about
            -- information, not coincidence.
            let md = buildMergeDiff tbPid tbLog custLog base mergedWorld
            in all (\d -> mdProvenance d == Unaware) (mergeLocations md)
                 `shouldBe` True
