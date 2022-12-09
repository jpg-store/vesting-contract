module Spec.Utils
  ( toDatum
  , toRedeemer
  , totalValueInInput
  , findVestingUtxo
  , mkVesting
  , getVestingUtxos
  , vestingTxFee
  )
  where

import Control.Monad.Reader ( asks, MonadTrans(lift) )
import Data.Map (Map)
import Data.Map          qualified as Map
import Data.Text (Text)
import Data.Void (Void)
import Canonical.Vesting (Input(Input), Schedule, vestingValHash, vestingAddr)
import Canonical.Vesting qualified as Vesting
import Ledger
    ( TxOutRef,
      TxId,
      Value,
      ChainIndexTxOut(ScriptChainIndexTxOut),
      Datum(..),
      Redeemer(Redeemer)
    )
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Plutus.Contract (Contract, EmptySchema)
import Plutus.Contract   qualified as Contract
import PlutusTx (ToData, toBuiltinData, fromBuiltinData)
import Spec.Setup ( AuditM, User, fromUsers )
import BotPlutusInterface.Constraints (submitBpiTxConstraintsWith)

mkVesting :: [User] -> Schedule -> AuditM TxId
mkVesting users schedule = do
  ppkhs <- asks (fmap Ledger.unPaymentPubKeyHash . fromUsers (`elem` users))

  let vestingDatum = Input ppkhs schedule

      txSK = Constraints.mustPayToOtherScriptWithInlineDatum
               vestingValHash
               (toDatum vestingDatum)
               (totalValueInInput vestingDatum)

  txId <- lift $ submitBpiTxConstraintsWith @Void mempty txSK []
  lift $ Contract.awaitTxConfirmed $ Ledger.getCardanoTxId txId
  return (Ledger.getCardanoTxId txId)


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
getVestingUtxos = lift $ Contract.utxosAt vestingAddr

totalValueInInput :: Input -> Value
totalValueInInput = mconcat . fmap Vesting.amount . Vesting.schedule

toDatum :: forall a. ToData a => a -> Datum
toDatum = Datum . toBuiltinData

toRedeemer :: forall a. ToData a => a -> Redeemer
toRedeemer = Redeemer . toBuiltinData

vestingTxFee :: Value
vestingTxFee = Ada.lovelaceValueOf (- 156100)
