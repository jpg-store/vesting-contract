{-# LANGUAGE NumericUnderscores #-}
module Main (main) where

import AuditPSM
import Plutus.Model
import Test.Tasty
import Spec.Vesting (unlockVesting)
-- import Canonical.Vesting (Input(..), Portion(..))

initialSetup :: String -> Run a -> TestTree
initialSetup = testNoErrors (adaValue 1_000_000_000) defaultBabbage

main :: IO ()
main = emptyNewBeneficiariesInWithdrawalsTests >>= defaultMain
