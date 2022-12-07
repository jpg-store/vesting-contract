{-# LANGUAGE ImportQualifiedPost #-}
module Spec.Utils
  ( toDatum
  , toRedeemer
  , getUtxos
  , totalValueInInput
  , getValidatorDatum 
  )
  where

import Data.Map (Map)
import Data.Text (Text)
import Canonical.Vesting (Input)
import qualified Canonical.Vesting as Vesting
import Ledger
import Plutus.Contract (Contract, EmptySchema)
import Plutus.Contract qualified as Contract
import PlutusTx (toBuiltinData, ToData)

toDatum :: forall a. ToData a => a -> Datum
toDatum = Datum . toBuiltinData

toRedeemer :: forall a. ToData a => a -> Redeemer
toRedeemer = Redeemer . toBuiltinData

getUtxos :: Address -> Contract [Value] EmptySchema Text (Map TxOutRef ChainIndexTxOut)
getUtxos addr = do
  Contract.utxosAt addr

totalValueInInput :: Input -> Value
totalValueInInput = mconcat . fmap Vesting.amount . Vesting.schedule


getValidatorDatum :: ChainIndexTxOut -> Contract () EmptySchema Text (Maybe Ledger.Datum)
getValidatorDatum ScriptChainIndexTxOut{..} = do
  Contract.datumFromHash (fst _ciTxOutScriptDatum)
getValidatorDatum _                         = return Nothing
