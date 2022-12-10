{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Spec.Vesting
  ( simpleLockVesting
  , checkVestingUtxoExists
  , mkAndUnlockVesting
  , nativeTokenVesting
  , unboundedDatum
  , multiUserVesting
  )
  where

import Control.Monad (void)
import Control.Monad.Reader (lift)
import Canonical.Vesting (Portion(..), Action (..) )
import Ledger.Ada qualified as Ada
import Data.Map          qualified as Map
import Plutus.Contract qualified as Contract
import Spec.Utils
import Spec.Setup
import Test.Plutip.Predicate
import Test.Plutip.Internal.Types (ExecutionResult(..))
import Ledger (Value, unPaymentPubKeyHash)
import Spec.Mint qualified as Mint
import Ledger.Value qualified as Value


simpleLockVesting :: VestingTest
simpleLockVesting = execVestingTest "Lock vesting contract successfully"
                   ( do
                      let vestingTxFee :: Value
                          vestingTxFee = Ada.lovelaceValueOf (- 156100)
                      Andrea `shouldHave` (Ada.adaValueOf 9900 <> vestingTxFee)
                   )
                   ( withUser Andrea $ do
                      (_, startTime) <- lift Contract.currentNodeClientTimeRange
                      mkVesting [Vlad, Borja] [Portion startTime (Ada.adaValueOf 100)]
                   ) [shouldSucceed]

checkVestingUtxoExists :: VestingTest
checkVestingUtxoExists = execVestingTest "Check if vesting utxo exists"
                        ( do
                           let vestingTxFee :: Value
                               vestingTxFee = Ada.lovelaceValueOf (- 153100)
                           Vlad `shouldHave` (Ada.adaValueOf 9900 <> vestingTxFee)
                        )
                       ( do
                          execRes <- withUser Vlad $ do
                               (_, startTime) <- lift Contract.currentNodeClientTimeRange
                               r <- mkVesting [Borja] [Portion startTime (Ada.adaValueOf 100)]
                               void $ lift $ Contract.waitNSlots 10
                               return r

                          case outcome execRes of
                            (Right ((vestingDatum, _), _)) -> withUser Borja $ findVestingUtxo vestingDatum
                            (Left _) -> error "Vesting utxo not found"

                       )
                       [shouldSucceed]

mkAndUnlockVesting :: VestingTest
mkAndUnlockVesting = execVestingTest "make and unlock vesting successfully"
                   ( do
                      let vestingTxFee :: Value
                          vestingTxFee = Ada.lovelaceValueOf (- 153100)
                      Andrea `shouldHave` (Ada.adaValueOf 9900 <> vestingTxFee) -- Andrea vests 100 Ada
                      Borja `shouldHave` (Ada.adaValueOf 10_100  <> Ada.lovelaceValueOf (-1779479)) -- Borja takes the vested ada
                   )
                   ( do
                      execRes <- withUser Andrea $ do
                        (_, startTime) <- lift Contract.currentNodeClientTimeRange
                        mkVesting [Borja] [Portion startTime (Ada.adaValueOf 100)]

                      let (Right ((vestingDatum, _), _)) = outcome execRes

                      execRes2 <- withUser Borja $ findVestingUtxo vestingDatum

                      let (Right (Just oref, _)) = outcome execRes2

                      execRes3 <- usersPPkhs

                      let (Right (ppkhs,_)) = outcome execRes3
                          action = Disburse $ case Map.lookup Borja ppkhs of
                                      Nothing -> error "Not Possible"
                                      (Just ppkh) -> [unPaymentPubKeyHash ppkh]

                      withUser Borja $ unlockVesting oref [Borja] vestingDatum action
                   ) [shouldSucceed]

nativeTokenVesting :: VestingTest
nativeTokenVesting = execVestingTest "make and unlock vesting of native tokens successfully"
                     (do
                        let vestingTxFee :: Value
                            vestingTxFee = Ada.lovelaceValueOf (-388813 + (-162700))
                        Oskar `shouldHave` (Ada.adaValueOf 9_900 <> vestingTxFee)
                        Vlad `shouldHave` (Ada.adaValueOf 10_100 <> Value.assetClassValue Mint.vestingAC 10_000 <> Ada.lovelaceValueOf (-1793033)) -- Vlad Takes vested native token + Ada
                     )
                     (
                       do
                         void $ withUser Oskar (mintNativeTokens Mint.mintingPolicy Mint.mintingPolicyHash "VestingToken")

                         execRes <- withUser Oskar $ do
                            (_, startTime) <- lift Contract.currentNodeClientTimeRange
                            mkVesting [Vlad] [Portion startTime (Ada.adaValueOf 100 <> Value.assetClassValue Mint.vestingAC 10_000)]

                         let (Right ((vestingDatum, _), _)) = outcome execRes

                         execRes2 <- withUser Vlad $ findVestingUtxo vestingDatum

                         let (Right (Just oref, _)) = outcome execRes2

                         execRes3 <- usersPPkhs

                         let (Right (ppkhs,_)) = outcome execRes3
                             action = Disburse $ case Map.lookup Vlad ppkhs of
                                         Nothing -> error "Not Possible"
                                         (Just ppkh) -> [unPaymentPubKeyHash ppkh]

                         withUser Vlad $ unlockVesting oref [Vlad] vestingDatum action
                     )
                     [shouldSucceed]

multiUserVesting :: VestingTest
multiUserVesting = execVestingTest "Make vesting to multiple pubKeyHash(es)"
                    (do
                        Drazen `shouldHave` (Ada.adaValueOf 9_000 <> Value.assetClassValue Mint.vestingAC 9_000 <> Ada.lovelaceValueOf (-565413))
                    )
                    (do
                         void $ withUser Drazen (mintNativeTokens Mint.mintingPolicy Mint.mintingPolicyHash Mint.vestingToken)

                         execRes <- withUser Drazen $ do
                            (_, startTime) <- lift Contract.currentNodeClientTimeRange
                            mkVesting [Vlad, Oskar, Magnus, Ellen] [Portion startTime (Ada.adaValueOf 1000 <> Value.assetClassValue Mint.vestingAC 1000)]

                         let ((vestingDatum, _), _) = case outcome execRes of
                                                        Left msg -> error (show msg)
                                                        Right res -> res


                         execRes2 <- withUser Vlad $ findVestingUtxo vestingDatum

                         let (Just oref, _) = case outcome execRes2 of
                                                Left msg -> error (show msg)
                                                Right res -> res

                         execRes3 <- usersPPkhs

                         let (Right (ppkhs,_)) = outcome execRes3
                             action = Disburse $ case Map.lookup Vlad ppkhs of
                                         Nothing -> error "Not Possible"
                                         (Just ppkh) -> [unPaymentPubKeyHash ppkh]

                         withUser Vlad $ unlockVesting oref [Vlad, Oskar, Magnus] vestingDatum action
                    )
                    [shouldSucceed]

unboundedDatum :: VestingTest
unboundedDatum = execVestingTest "Test for an upperbound of the input datum"
                     (do
                        let txFee :: Value
                            txFee = Ada.lovelaceValueOf (-388813 + (-1500800)) -- Fee for Minting policy and submitting tx for vesting.
                        Oskar `shouldHave` (Ada.adaValueOf 9_000 <> txFee <> Value.assetClassValue Mint.vestingAC 9_000)
                     )
                     (
                       do

                         -- Oskar mints native tokens
                         void $ withUser Oskar (mintNativeTokens Mint.mintingPolicy Mint.mintingPolicyHash Mint.vestingToken)

                         execRes <- withUser Oskar $ do
                            (_, startTime) <- lift Contract.currentNodeClientTimeRange
                            mkVesting [Vlad] (replicate 200 $ Portion startTime (Ada.adaValueOf 5 <> Value.assetClassValue Mint.vestingAC 5))

                         let ((vestingDatum, _), _) = case outcome execRes of
                                                        Left msg -> error (show msg)
                                                        Right res -> res


                         execRes2 <- withUser Vlad $ findVestingUtxo vestingDatum

                         let (Just oref, _) = case outcome execRes2 of
                                                Left msg -> error (show msg)
                                                Right res -> res

                         execRes3 <- usersPPkhs

                         let (Right (ppkhs,_)) = outcome execRes3
                             action = Disburse $ case Map.lookup Vlad ppkhs of
                                         Nothing -> error "Not Possible"
                                         (Just ppkh) -> [unPaymentPubKeyHash ppkh]

                         withUser Vlad $ unlockVesting oref [Vlad] vestingDatum action
                     )
                     [shouldFail]
