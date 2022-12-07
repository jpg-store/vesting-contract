{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Main (main) where

import Spec.Vesting (mkVesting, makeAndUnlockVesting)
import Canonical.Vesting (Input(Input))
import Control.Monad (void)
import qualified Canonical.Vesting as Vesting
import Data.Default (def)
import Test.Plutip.Contract (
  initAda,
  withCollateral,
  withContract, assertExecutionWith
 )
import qualified Ledger.Ada as Ada
import qualified Plutus.Contract as Contract
import qualified Ledger
import Test.Tasty
import Test.Plutip.LocalCluster (withConfiguredCluster)
import Test.Plutip.Predicate
import BotPlutusInterface.Types (LogContext (ContractLog), LogLevel (Error), LogType (AnyLog))
import Test.Plutip.Options (TraceOption(ShowTraceButOnlyContext))

tests :: TestTree
tests =
  let config = def
  in withConfiguredCluster
      config
      "Vesting Contract Tests"
      [
        let wallet0 = 200_000_000
            wallet1 = 100_000_000
            fundsToVest = Ada.lovelaceValueOf 100_000_000
        in
         assertExecutionWith
          [ShowTraceButOnlyContext ContractLog $ Error [AnyLog]]
          "Make successful vesting"
          (withCollateral $ initAda [wallet0] <> initAda [wallet1])
          (do
            withContract $ \[_w1pkh] -> do
                ppkh <- Contract.ownFirstPaymentPubKeyHash
                (_, startTime) <- Contract.currentNodeClientTimeRange

                let vestingDatum = Input [Ledger.unPaymentPubKeyHash ppkh] [Vesting.Portion startTime fundsToVest]

                void $ mkVesting vestingDatum

                makeAndUnlockVesting vestingDatum

          )
          [shouldSucceed]

      ]

main :: IO()
main = defaultMain tests
