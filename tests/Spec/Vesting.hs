{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Spec.Vesting
  ( mkVesting
  , makeAndUnlockVesting
  )
  where

import Data.Void (Void)
import Data.Text (Text)
import Data.Map (Map)
import Data.Map qualified as Map
import Control.Monad (void)
import Ledger.Tx (TxOutRef)
import Ledger (TxId, Language(PlutusV2), Versioned(Versioned), Datum (getDatum))
import qualified Ledger
import Plutus.PAB.Effects.Contract.Builtin (EmptySchema)
import Plutus.Contract qualified as Contract
import Canonical.Vesting qualified as Vesting
import Canonical.Vesting (Vesting ,vestingValHash, vestingAddr, vestingValidator)
import Ledger.Constraints qualified as Constraints
import Plutus.Contract (Contract, awaitTxConfirmed)
import Spec.Utils (toDatum, toRedeemer, totalValueInInput, getValidatorDatum)
import PlutusTx (fromBuiltinData)
import BotPlutusInterface.Constraints (submitBpiTxConstraintsWith, mustValidateInFixed)

getVestingUtxo :: Vesting.Input -> Contract () EmptySchema Text (Maybe TxOutRef)
getVestingUtxo vestingDatum = do
  utxos <-  traverse getValidatorDatum =<< Contract.utxosAt vestingAddr

  let refAndDatums :: Map TxOutRef (Maybe Vesting.Input)
      refAndDatums = fmap (>>= (fromBuiltinData . getDatum))  utxos


  case Map.toList (Map.filter (== Just vestingDatum) refAndDatums) of
    [(oref,_)] -> return (Just oref)
    _         -> return Nothing


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
  void $ Contract.waitNSlots 5
  return (Ledger.getCardanoTxId txId)

makeAndUnlockVesting :: Vesting.Input -> Contract () EmptySchema Text TxId
makeAndUnlockVesting vestingDatum = do

  void $ Contract.waitNSlots 10

  ppkh <- Contract.ownFirstPaymentPubKeyHash

  utxos <- Map.toList <$> Contract.utxosAt vestingAddr
  oref' <- getVestingUtxo vestingDatum
  (_, startTime) <- Contract.currentNodeClientTimeRange

  let (Just oref) = oref'
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
