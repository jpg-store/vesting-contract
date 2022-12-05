{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import BotPlutusInterface.Types (LogContext (ContractLog), LogLevel (Error), LogType (AnyLog))
import qualified Canonical.Vesting as Vesting
import Data.Default (def)
import qualified Ledger
import qualified Ledger.Ada as Ada
import qualified Plutus.Contract as Contract
import Spec.Vesting (mkVesting)
import Test.Plutip.Contract (
  assertExecutionWith,
  initAda,
  withCollateral,
  withContract,
 )
import Test.Plutip.LocalCluster (withConfiguredCluster)
import Test.Plutip.Options (TraceOption (ShowTraceButOnlyContext))
import Test.Plutip.Predicate
import Test.Tasty

tests :: TestTree
tests =
  let config = def
   in withConfiguredCluster
        config
        "Vesting Contract Tests"
        [ let wallet0 = 200_000_000
              wallet1 = 100_000_000
              fundsToVest = Ada.lovelaceValueOf 100_000_000
           in assertExecutionWith
                [ShowTraceButOnlyContext ContractLog $ Error [AnyLog]]
                "Make successful vesting"
                (withCollateral $ initAda [wallet0] <> initAda [wallet1])
                ( do
                    withContract $ \[w2pkh] -> do
                      currentTime <- Contract.currentTime
                      mkVesting
                        ( Vesting.Input
                            [Ledger.unPaymentPubKeyHash w2pkh]
                            [Vesting.Portion currentTime fundsToVest]
                        )
                )
                [shouldSucceed]
        ]

main :: IO ()
main = defaultMain tests
