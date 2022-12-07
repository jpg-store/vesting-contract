{-# LANGUAGE ImportQualifiedPost #-}
module Spec.Utils
  ( toDatum
  , toRedeemer
  , getUtxos
  , totalValueInInput
  )
  where

import Data.Map (Map)
import Data.Text (Text)
import Canonical.Vesting (Input)
import qualified Canonical.Vesting as Vesting
import Data.List.NonEmpty qualified as NonEmpty
import Ledger
import Plutus.Contract (Contract, EmptySchema)
import Plutus.Contract qualified as Contract
import PlutusTx (toBuiltinData, ToData)

toDatum :: forall a. ToData a => a -> Datum
toDatum = Datum . toBuiltinData

toRedeemer :: forall a. ToData a => a -> Redeemer
toRedeemer = Redeemer . toBuiltinData

getUtxos :: Contract [Value] EmptySchema Text (Map TxOutRef ChainIndexTxOut)
getUtxos = do
  addr <- NonEmpty.head <$> Contract.ownAddresses
  Contract.utxosAt addr

totalValueInInput :: Input -> Value
totalValueInInput = mconcat . fmap Vesting.amount . Vesting.schedule
