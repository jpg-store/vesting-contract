module Main (main) where

import Spec.Vesting (simpleLockVesting, checkVestingUtxoExists, mkAndUnlockVesting)
import Data.Default (def)
import Test.Tasty
import Test.Plutip.LocalCluster (withConfiguredCluster)

tests :: TestTree
tests =
  let config = def
  in withConfiguredCluster
      config
      "Vesting Contract Tests"
      [ simpleLockVesting
      , checkVestingUtxoExists
      , mkAndUnlockVesting
         -- assertExecutionWith
         --  [ShowTraceButOnlyContext ContractLog $ Error [AnyLog]]
         --  "Get all vesting utxos"
         --  (withCollateral $ initAda [1000])
         --  (do
         --    withContract $ \_ -> do
         --        utxos <- Map.toList <$> getUtxos Vesting.vestingAddr
         --        when (null utxos) $ error "Expected 1 or more utxos at vesting contract address"
         --  )
         --  [shouldSucceed]

        -- let wallet0 = 200_000_000
        --     wallet1 = 100_000_000
        --     fundsToVest = Ada.lovelaceValueOf 100_000_000
        -- in
        --  assertExecutionWith
        --   [ShowTraceButOnlyContext ContractLog $ Error [AnyLog]]
        --   "Unlock vesting successfully"
        --   (withCollateral $ initAda [wallet0] <> initAda [wallet1])
        --   (do
        --
        --     execRes <- withContract $ \[w1pkh] -> do
        --         (_, startTime) <- Contract.currentNodeClientTimeRange
        --
        --         let vestingDatum = Input [Ledger.unPaymentPubKeyHash w1pkh] [Vesting.Portion startTime fundsToVest]
        --
        --         void $ mkVesting vestingDatum
        --         return vestingDatum
        --
        --     let (Right (vestingDatum, _)) = outcome execRes
        --
        --     withContractAs 1 $ \[_w0pkh] ->
        --       makeAndUnlockVesting vestingDatum
        --   )
        --   [shouldSucceed]

      ]

main :: IO()
main = defaultMain tests
