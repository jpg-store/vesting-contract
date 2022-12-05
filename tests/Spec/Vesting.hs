module Spec.Vesting (
  mkVesting,
  toDatum,
  toRedeemer,
  getUtxos,
) where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)

import qualified Canonical.Vesting as Vesting
import Data.Map (Map)
import Ledger (
  ChainIndexTxOut,
  Datum (Datum),
  Redeemer (Redeemer),
  TxId,
  TxOutRef,
  ValidatorHash,
  Value,
  getCardanoTxId,
 )
import qualified Ledger.Constraints as Constraints
import qualified Ledger.Scripts as Scripts
import Plutus.Contract (Contract, awaitTxConfirmed, submitTx)
import qualified Plutus.Contract as Contract
import Plutus.PAB.Effects.Contract.Builtin (EmptySchema)
import PlutusTx (ToData, toBuiltinData)

mkVesting :: Vesting.Input -> Contract () EmptySchema Text TxId
mkVesting vestingDatum = do
  let txSK :: Constraints.TxConstraints i o
      txSK =
        Constraints.mustPayToOtherScript
          vestingValHash
          (toDatum vestingDatum)
          (mconcat $ Vesting.amount <$> Vesting.schedule vestingDatum)

  tx <- submitTx txSK
  awaitTxConfirmed $ getCardanoTxId tx
  return (getCardanoTxId tx)

toDatum :: forall a. ToData a => a -> Datum
toDatum = Datum . toBuiltinData

toRedeemer :: forall a. ToData a => a -> Redeemer
toRedeemer = Redeemer . toBuiltinData

vestingValHash :: ValidatorHash
vestingValHash = Scripts.validatorHash Vesting.validator

getUtxos :: Contract [Value] EmptySchema Text (Map TxOutRef ChainIndexTxOut)
getUtxos = do
  addr <- NonEmpty.head <$> Contract.ownAddresses
  Contract.utxosAt addr
