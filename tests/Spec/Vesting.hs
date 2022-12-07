{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Spec.Vesting
  ( mkVesting
  , makeAndUnlockVesting
  )
  where

import Data.Void (Void)
import Data.Text (Text)
import Data.Map qualified as Map
import Control.Monad (void)
import Ledger (TxId, Language(PlutusV2), Versioned(Versioned))
import qualified Ledger
import Plutus.PAB.Effects.Contract.Builtin (EmptySchema)
import Plutus.Contract qualified as Contract
import Canonical.Vesting qualified as Vesting
import Canonical.Vesting (Vesting ,vestingValHash, vestingAddr, vestingValidator)
import Ledger.Constraints qualified as Constraints
import Plutus.Contract (Contract, awaitTxConfirmed)
import Spec.Utils (toDatum, toRedeemer, totalValueInInput)
import BotPlutusInterface.Constraints (submitBpiTxConstraintsWith, mustValidateInFixed)

mkVesting :: Vesting.Input -> Contract () EmptySchema Text TxId
mkVesting vestingDatum = do

  let
      txSK :: Constraints.TxConstraints i o
      txSK = Constraints.mustPayToOtherScriptWithInlineDatum
               vestingValHash
               (toDatum vestingDatum)
               (totalValueInInput vestingDatum)

  txId <- submitBpiTxConstraintsWith @Void mempty txSK []
  awaitTxConfirmed $ Ledger.getCardanoTxId txId
  return (Ledger.getCardanoTxId txId)

makeAndUnlockVesting :: Vesting.Input -> Contract () EmptySchema Text TxId
makeAndUnlockVesting vestingDatum = do

  void $ Contract.waitNSlots 10

  ppkh <- Contract.ownFirstPaymentPubKeyHash
  utxos <- Map.toList <$> Contract.utxosAt vestingAddr
  (_, startTime) <- Contract.currentNodeClientTimeRange

  let [(oref,_)] = utxos
      txSk = Constraints.mustSpendScriptOutput
                oref (toRedeemer $ Vesting.Disburse $ Vesting.beneficiaries vestingDatum)
           <> Constraints.mustPayToPubKey ppkh (totalValueInInput vestingDatum)
           <> Constraints.mustPayToOtherScriptWithDatumInTx
               vestingValHash
               (toDatum vestingDatum)
               mempty

      lkps = Constraints.unspentOutputs (Map.fromList utxos)
           <> Constraints.otherScript (Versioned vestingValidator PlutusV2)


  txId <- submitBpiTxConstraintsWith @Vesting lkps txSk (mustValidateInFixed (Ledger.from startTime))

  awaitTxConfirmed $ Ledger.getCardanoTxId txId
  pure (Ledger.getCardanoTxId txId)
