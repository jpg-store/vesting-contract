{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main (main) where

import Control.Monad (void, when)
import Spec.Vesting (mkVesting, makeAndUnlockVesting)
import Spec.Utils (getUtxos)
import Canonical.Vesting (Input(Input))
import qualified Canonical.Vesting as Vesting
import Data.Default (def)
import Test.Plutip.Contract (
  initAda,
  withCollateral,
  withContract,
  assertExecutionWith, withContractAs
 )
import qualified Data.Map as Map
import qualified Ledger.Ada as Ada
import qualified Plutus.Contract as Contract
import qualified Ledger
import Test.Tasty
import Test.Plutip.LocalCluster (withConfiguredCluster)
import Test.Plutip.Predicate
import BotPlutusInterface.Types (LogContext (ContractLog), LogLevel (Error), LogType (AnyLog))
import Test.Plutip.Options (TraceOption(ShowTraceButOnlyContext))
import Test.Plutip.Internal.Types (ExecutionResult(..))

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
          )
          [shouldSucceed],

         assertExecutionWith
          [ShowTraceButOnlyContext ContractLog $ Error [AnyLog]]
          "Get all vesting utxos"
          (withCollateral $ initAda [1000])
          (do
            withContract $ \_ -> do
                utxos <- Map.toList <$> getUtxos Vesting.vestingAddr
                when (null utxos) $ error "Expected 1 or more utxos at vesting contract address"
          )
          [shouldSucceed],

        let wallet0 = 200_000_000
            wallet1 = 100_000_000
            fundsToVest = Ada.lovelaceValueOf 100_000_000
        in
         assertExecutionWith
          [ShowTraceButOnlyContext ContractLog $ Error [AnyLog]]
          "Unlock vesting successfully"
          (withCollateral $ initAda [wallet0] <> initAda [wallet1])
          (do

            execRes <- withContract $ \[w1pkh] -> do
                (_, startTime) <- Contract.currentNodeClientTimeRange

                let vestingDatum = Input [Ledger.unPaymentPubKeyHash w1pkh] [Vesting.Portion startTime fundsToVest]

                void $ mkVesting vestingDatum
                return vestingDatum

            let (Right (vestingDatum, _)) = outcome execRes

            withContractAs 1 $ \[_w0pkh] -> 
              makeAndUnlockVesting vestingDatum
          )
          [shouldSucceed]

      ]

main :: IO()
main = defaultMain tests
