{-# LANGUAGE
     NumericUnderscores,
     LambdaCase,
     RecordWildCards,
     TemplateHaskell,
     RankNTypes
#-}

module AuditPSM where

import Plutus.Model
import Plutus.Model.Validator.V1
import Canonical.Vesting
import Plutus.V2.Ledger.Api (PubKeyHash)
import Control.Monad
import Plutus.Model.Pretty
import Test.Tasty
import Control.Lens
    ( (<&>), view, makeLenses, ReifiedLens(Lens), ReifiedLens', (&), Fold, folding, (^..), (^?) )
import Control.Monad.Reader
import qualified Data.Time.Clock.POSIX as Time
import Plutus.V1.Ledger.Time (POSIXTime)
import Plutus.V1.Ledger.Interval (from)

import Utils
import Data.List (foldl')
import Plutus.V1.Ledger.Time (POSIXTimeRange)
import Plutus.V1.Ledger.Value (Value)
import Plutus.Model.Mock.Stat (Stat)

type Amount = Integer

logTx :: Tx -> Run ()
logTx = logError . ppTransaction

lock :: Mode Vesting
            -> KnownUser
            -> Input
            -> AuditM ()
lock mode user' input = withUser user' $ \usr -> lift $ do
  let tx' :: Tx
      tx' = payToScript vestingScript (mode input) totalValue

      totalValue = mconcat $ amount <$> schedule input

  sp <- spend usr totalValue

  let tx = tx' <> userSpend sp

  logTx tx

  submitTx usr tx

lock' = lock HashDatum

multisignTx :: [PubKeyHash] -> Tx -> Run Tx
multisignTx usrs tx = foldM (flip signTx) tx usrs

payToKeys :: [PubKeyHash] -> Amount -> Tx
payToKeys usrs amt = foldl' (\acc u -> payToKey u (adaValue amt) <> acc) mempty usrs

type ToUser = Amount
type ToScript = Amount
type Signers = [KnownUser]

unlock :: Mode Vesting -> KnownUser -> Signers  -> Input -> ToUser -> ToScript -> POSIXTimeRange -> AuditM ()
unlock mode usr' signers' input toUser toScript iv =  do
  redeemer <- disburse [usr']
  signers  <- users signers'
  beneficiary <- user usr'
  lift $ do
    [(ref,_)] <- utxoAt vestingScript
    let tx' =  mconcat [ spendScript vestingScript ref redeemer input
                       , payToScript vestingScript (mode input) (adaValue toScript)
                       , payToKey beneficiary (adaValue toUser) ]
    tx <- multisignTx signers =<< validateIn iv tx'
    logTx tx
    void $ sendTx tx


unlock' :: KnownUser
        -> Signers
        -> Input
        -> ToUser
        -> ToScript
        -> POSIXTimeRange
        -> AuditM ()
unlock' = unlock HashDatum

simpleContract :: AuditM ()
simpleContract = do
  now <- lift currentTime

  let deadline = now + seconds 100
      amount = 100

  input <- mkInput [drazen,chase] [adaPortion deadline amount]

  lock' andrea input

  lift $ waitUntil (deadline + seconds 200)

  unlock' drazen [drazen,chase] input 99 1 (from $ deadline + seconds 200)







{-
unlockVesting :: Run ()
unlockVesting = do
  now   <- currentTime
  user1 <- newUser (adaValue 100)

  let input = Input [user1] [Portion (now + 100 * seconds 1) (adaValue 100)]

  makeVestingInline input

  waitUntil (now + 200 * seconds 1)

  [(ref, _)] <- utxoAt vestingScript

  let tx' :: Tx
      tx' = mconcat [ spendScript vestingScript ref (Disburse [user1]) input
                   , payToKey user1 (adaValue 99)
                   , payToScript vestingScript (InlineDatum input) (adaValue 1)
                   ]
  tx <- validateIn (from (now + 200 * seconds 1)) tx'
  void $ signTx user1 tx >>= sendTx



makeSuccessfulVesting :: Run ()
makeSuccessfulVesting = do
  now   <- currentTime
  user1 <- newUser (adaValue 100)
  makeVestingInline (Input [user1] [Portion (now + 100 * seconds 1) (adaValue 100)])

superSimpleTest :: Run ()
superSimpleTest = do
  Users{..} <- identifyUsers
  now <- currentTime
  let amount = adaValue 100
  spending <- spend (user andrea) amount
  let input = Input [drazen] [Portion (now + weeks 1) amount]
      lockFunds = payToScript vestingScript (HashDatum input) amount
                  <> userSpend spending
  logBalanceSheet
  logError $ ppTransaction lockFunds
  submitTx andrea lockFunds
  logBalanceSheet
  wait (weeks 1 + seconds 10)
  [box] <- boxAt vestingScript
  let unlockFunds = spendBox vestingScript (Disburse [drazen])  box
                    <> payToKey drazen (adaValue 99)
                    <> payToScript vestingScript (HashDatum input) (adaValue 1)
  logInfo $ ppTransaction lockFunds
  submitTx drazen unlockFunds
  logBalanceSheet
-}
