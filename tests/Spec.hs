module Main (main) where

import Spec.Vesting
import Data.Default (def)
import Test.Tasty
import Test.Plutip.LocalCluster (withConfiguredCluster)

tests :: TestTree
tests =
  let config = def
  in withConfiguredCluster
      config
      "Vesting Contract Tests"
      [
      simpleLockVesting
      , checkVestingUtxoExists
      , mkAndUnlockVesting
      , nativeTokenVesting
      -- , unboundedDatum
      ]

main :: IO()
main = defaultMain tests
