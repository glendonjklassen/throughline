module Main where

import           Test.Hspec

import qualified Engine.Core.AxiomsSpec
import qualified Engine.ChainSpec
import qualified Engine.Core.ConditionsSpec
import qualified Engine.Author.CommonAxiomsSpec
import qualified Engine.Author.DSLSpec
import qualified Engine.Author.SceneSpec
import qualified Engine.Core.EffectsSpec
import qualified Engine.Sync.EventLogSpec
import qualified Engine.Sync.IdentitySpec
import qualified Engine.Author.NarrativeSpec
import qualified Engine.CRDT.ORSetSpec
import qualified Engine.CRDT.PNCounterSpec
import qualified Engine.Sync.CausalitySpec
import qualified Engine.Sync.ConvergenceSpec
import qualified Engine.CRDT.ORSetPropSpec
import qualified Engine.CRDT.PNCounterPropSpec
import qualified Engine.JSONRoundTripSpec
import qualified SDL.TextSpec
import qualified Engine.DiffPropSpec
import qualified Engine.Core.WorldSpec
import qualified Scenarios.CustomerSyncSpec
import qualified Scenarios.CoLocationSpec
import qualified Engine.Core.SystemAxiomsSpec
import qualified Scenarios.DinerSpec
import qualified Scenarios.DinerSyncSpec
import qualified Scenarios.DinerInterleaveSpec
import qualified Scenarios.DeerHuntSpec
import qualified Scenarios.DeerHuntPlaythrough
import qualified Scenarios.DeerHuntSyncSpec
import qualified Scenarios.TopBuySpec

main :: IO ()
main = hspec $ do
  Engine.Core.ConditionsSpec.spec
  Engine.Core.EffectsSpec.spec
  Engine.Author.CommonAxiomsSpec.spec
  Engine.Author.DSLSpec.spec
  Engine.Author.SceneSpec.spec
  Engine.Core.AxiomsSpec.spec
  Engine.Core.SystemAxiomsSpec.spec
  Engine.Author.NarrativeSpec.spec
  Engine.ChainSpec.spec
  Engine.Sync.EventLogSpec.spec
  Engine.Sync.IdentitySpec.spec
  Engine.CRDT.ORSetSpec.spec
  Engine.CRDT.PNCounterSpec.spec
  Engine.Sync.CausalitySpec.spec
  Engine.Sync.ConvergenceSpec.spec
  Engine.CRDT.ORSetPropSpec.spec
  Engine.CRDT.PNCounterPropSpec.spec
  Engine.JSONRoundTripSpec.spec
  SDL.TextSpec.spec
  Engine.DiffPropSpec.spec
  Engine.Core.WorldSpec.spec
  Scenarios.DeerHuntSpec.spec
  Scenarios.DeerHuntPlaythrough.spec
  Scenarios.DeerHuntSyncSpec.spec
  Scenarios.TopBuySpec.spec
  Scenarios.DinerSpec.spec
  Scenarios.DinerSyncSpec.spec
  Scenarios.DinerInterleaveSpec.spec
  Scenarios.CustomerSyncSpec.spec
  Scenarios.CoLocationSpec.spec
