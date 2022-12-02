{-# LANGUAGE NumericUnderscores #-}
module Spec.Vesting (makeVesting, makeSuccessfulVesting, unlockVesting) where

import Plutus.V1.Ledger.Api

import Plutus.Model hiding (validatorHash)
import Plutus.Model.V2 (mkTypedValidator)
import Plutus.Model.Mock.Stat (Stat)
import Canonical.Vesting (Input(..), Action(..), Portion(..), untypedValidator)
import Control.Monad (void)

type Vesting = TypedValidator Input Action

-- vestingValHash :: ValidatorHash
-- vestingValHash = validatorHash validator

vestingScript :: Vesting
vestingScript = mkTypedValidator untypedValidator 

makeVesting :: Input -> Run (Either FailReason Stat)
makeVesting input = do
  admin <- getMainUser
  let tx :: Tx
      tx = payToScript vestingScript (InlineDatum input) totalValue

      totalValue :: Value
      totalValue = mconcat $ amount <$> schedule input

  sp <- spend admin totalValue

  signTx admin (tx <> userSpend sp) >>= sendTx

makeSuccessfulVesting :: Run ()
makeSuccessfulVesting = do
  now   <- currentTime
  user1 <- newUser (adaValue 100)
  void $ makeVesting (Input [user1] [Portion (now + 100 * oneSecond) (adaValue 100)])

unlockVesting :: Run ()
unlockVesting = do
  now   <- currentTime
  user1 <- newUser (adaValue 100)

  let input = Input [user1] [Portion (now + 100 * oneSecond) (adaValue 100)]

  void $ makeVesting input

  waitUntil (now + 200 * oneSecond)

  [(ref, _)] <- utxoAt vestingScript

  let tx' :: Tx
      tx' = mconcat [ spendScript vestingScript ref (Disburse [user1]) input
                    , payToKey user1 (adaValue 99)
                    , payToScript vestingScript (InlineDatum input) (adaValue 1)
                   ]
  tx <- validateIn (from (now + 200 * oneSecond)) tx'
  void $ signTx user1 tx >>= sendTx

oneSecond :: POSIXTime
oneSecond = POSIXTime 1_000
