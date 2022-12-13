{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Spec.Utils
  ( toDatum
  , toRedeemer
  , totalValueInInput
  , findVestingUtxo
  , mkVesting
  , getVestingUtxos
  , unlockVesting
  , usersPPkhs
  )
  where

import Control.Monad.Reader ( asks, MonadTrans(lift) )
import Data.Map (Map)
import Data.Map          qualified as Map
import Data.Text (Text)
import Data.Void (Void)
import Canonical.Vesting (Input(..), Schedule, Action (..), Vesting)
import Canonical.Vesting qualified as Vesting
import Ledger
    ( TxOutRef,
      TxId,
      Value,
      ChainIndexTxOut(ScriptChainIndexTxOut),
      Datum(..),
      Redeemer(Redeemer), Versioned (..), Language (PlutusV2), unPaymentPubKeyHash
    )
import Ledger qualified
import Ledger.Constraints qualified as Constraints
import Plutus.Contract (Contract, EmptySchema)
import Plutus.Contract   qualified as Contract
import PlutusTx (ToData, toBuiltinData, fromBuiltinData)
import Spec.Setup
import BotPlutusInterface.Constraints (submitBpiTxConstraintsWith, mustValidateInFixed)
import Control.Monad (void)

mkVesting :: [User] -> Schedule -> AuditM (Input,TxId)
mkVesting users schedule = do
  ppkhs <- asks (fmap Ledger.unPaymentPubKeyHash . fromUsers (`elem` users))

  let vestingDatum = Input ppkhs schedule

      txSK = Constraints.mustPayToOtherScriptWithInlineDatum
               Vesting.vestingValHash
               (toDatum vestingDatum)
               (totalValueInInput vestingDatum)

  txId <- lift $ submitBpiTxConstraintsWith @Void mempty txSK []
  lift $ Contract.awaitTxConfirmed $ Ledger.getCardanoTxId txId
  return (vestingDatum, Ledger.getCardanoTxId txId)

unlockVesting :: Input -> Action -> TxOutRef -> AuditM (Input,TxId)
unlockVesting input action oref = do
  void $ lift $ Contract.waitNSlots 10

  utxos <- getVestingUtxos
  ppkh <- lift Contract.ownFirstPaymentPubKeyHash
  (_, startTime) <- lift Contract.currentNodeClientTimeRange

  let
     
      (Disburse newBenficiaries) = action
      newInput = input{beneficiaries=newBenficiaries}
      txSk = Constraints.mustSpendScriptOutput
                oref (toRedeemer action)
           <> Constraints.mustPayToPubKey ppkh (totalValueInInput newInput)
           <> Constraints.mustPayToOtherScriptWithDatumInTx
               Vesting.vestingValHash
               (toDatum newInput)
               mempty

      lkps = Constraints.unspentOutputs utxos
           <> Constraints.otherScript (Versioned Vesting.vestingValidator PlutusV2)

  txId <- lift $ submitBpiTxConstraintsWith @Vesting lkps txSk (mustValidateInFixed (Ledger.from startTime))

  lift $ Contract.awaitTxConfirmed $ Ledger.getCardanoTxId txId
  pure (newInput, Ledger.getCardanoTxId txId)



findVestingUtxo :: Vesting.Input -> AuditM (Maybe TxOutRef)
findVestingUtxo vestingDatum = do
  utxos <-  lift $ traverse getValidatorDatum =<< Contract.utxosAt Vesting.vestingAddr

  let refAndDatums :: Map TxOutRef (Maybe Vesting.Input)
      refAndDatums = fmap (>>= (fromBuiltinData . getDatum)) utxos

  case Map.toList (Map.filter (== Just vestingDatum) refAndDatums) of
    ((oref,_):_) -> return (Just oref)
    _         -> return Nothing

  where

    getValidatorDatum :: ChainIndexTxOut -> Contract () EmptySchema Text (Maybe Ledger.Datum)
    getValidatorDatum ScriptChainIndexTxOut{..} = do
      Contract.datumFromHash (fst _ciTxOutScriptDatum)
    getValidatorDatum _                         = return Nothing

getVestingUtxos :: AuditM (Map TxOutRef ChainIndexTxOut)
getVestingUtxos = lift $ Contract.utxosAt Vesting.vestingAddr

totalValueInInput :: Input -> Value
totalValueInInput = mconcat . fmap Vesting.amount . Vesting.schedule

toDatum :: forall a. ToData a => a -> Datum
toDatum = Datum . toBuiltinData

toRedeemer :: forall a. ToData a => a -> Redeemer
toRedeemer = Redeemer . toBuiltinData

-- usersPPkhs :: AuditM (Users 'PKH)
usersPPkhs = withUser Andrea  (asks getUsers :: AuditM (Map User Ledger.PaymentPubKeyHash))
