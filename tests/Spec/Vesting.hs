module Spec.Vesting
  ( simpleLockVesting
  )
  where

import Control.Monad.Reader (lift)
import Canonical.Vesting (Portion(..) )
import Ledger.Ada qualified as Ada
import Plutus.Contract qualified as Contract
import Spec.Utils
import Spec.Setup
import Test.Plutip.Predicate


simpleLockVesting :: Return
simpleLockVesting = execVestingTest "Lock vesting contract successfully"
                   ( do
                      Andrea `shouldHave` (Ada.adaValueOf 9900 <> vestingTxFee)
                   )
                   ( withUser Andrea $ do
                      (_, startTime) <- lift Contract.currentNodeClientTimeRange
                      mkVesting [Vlad, Borja] [Portion startTime (Ada.adaValueOf 100)]
                   ) [shouldSucceed]

-- checkVestingUtxoExists :: Return
-- checkVestingUtxoExists = execVestingTest "Check if vesting utxo exists"
--                        ( withUser Vlad $ do
--                            (_, startTime) <- lift Contract.currentNodeClientTimeRange
--                            mkVesting [Vlad, Borja] [Portion startTime (Ada.lovelaceValueOf 100_000_000)]
--                        )

-- mkVesting :: Vesting.Input -> Contract () EmptySchema Text TxId
-- mkVesting vestingDatum = do
--
--   let
--       txSK :: Constraints.TxConstraints i o
--       txSK = Constraints.mustPayToOtherScriptWithInlineDatum
--                vestingValHash
--                (toDatum vestingDatum)
--                (totalValueInInput vestingDatum)
--
--   txId <- submitBpiTxConstraintsWith @Void mempty txSK []
--   awaitTxConfirmed $ Ledger.getCardanoTxId txId
--   void $ Contract.waitNSlots 5
--   return (Ledger.getCardanoTxId txId)
--
-- makeAndUnlockVesting :: Vesting.Input -> Contract () EmptySchema Text TxId
-- makeAndUnlockVesting vestingDatum = do
--
--   void $ Contract.waitNSlots 10
--
--   ppkh <- Contract.ownFirstPaymentPubKeyHash
--
--   utxos <- Map.toList <$> Contract.utxosAt vestingAddr
--   oref' <- getVestingUtxo vestingDatum
--   (_, startTime) <- Contract.currentNodeClientTimeRange
--
--   let (Just oref) = oref'
--       txSk = Constraints.mustSpendScriptOutput
--                 oref (toRedeemer $ Vesting.Disburse $ Vesting.beneficiaries vestingDatum)
--            <> Constraints.mustPayToPubKey ppkh (totalValueInInput vestingDatum)
--            <> Constraints.mustPayToOtherScriptWithDatumInTx
--                vestingValHash
--                (toDatum vestingDatum)
--                mempty
--
--       lkps = Constraints.unspentOutputs (Map.fromList utxos)
--            <> Constraints.otherScript (Versioned vestingValidator PlutusV2)
--
--   txId <- submitBpiTxConstraintsWith @Vesting lkps txSk (mustValidateInFixed (Ledger.from startTime))
--
--   awaitTxConfirmed $ Ledger.getCardanoTxId txId
--   pure (Ledger.getCardanoTxId txId)
