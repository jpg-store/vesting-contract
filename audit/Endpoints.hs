module Endpoints where

import Utils
import Canonical.Vesting
import Plutus.Model.Contract
import Plutus.Model (logInfo)
import Control.Monad.Trans
import Plutus.V1.Ledger.Time (POSIXTimeRange, POSIXTime)
import Plutus.Model.Ada (adaValue)
import Config
import Plutus.V1.Ledger.Interval (from)

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
  logInfo "Locking"
  logInfo ("datum: " <> show (schedule input))
  --logTx tx

  submitTx usr tx

lock' :: KnownUser -> Input -> AuditM ()
lock' = lock InlineDatum

runDeposit :: POSIXTime -> DepositConfig -> AuditM (POSIXTime, Input')
runDeposit now DepositConfig{..} = do
  let perPortion = inputAmount `div` portions

      sched = mkSchedule now deadlineOffset perPortion portions []

  let input' = Input' beneficiaries sched
  input <- toInput input'

  lock' benefactor input

  pure (now, input')


unlock :: Mode Vesting -> KnownUser -> Signers  -> [KnownUser] -> Input -> ToUser -> ToScript -> POSIXTimeRange -> AuditM ()
unlock mode usr' signers' newBeneficiaries input toUser toScript iv =  do
  signers           <- users signers'
  redeemer          <- disburse newBeneficiaries
  beneficiary       <- user usr'
  newBeneficiaries' <- users newBeneficiaries
  let outDatum = updateInput newBeneficiaries' input
  lift $ do logInfo "Unlocking"
            logInfo ("old datum:" <> show (schedule input))
            logInfo ("new datum:" <> show (schedule outDatum))
            logInfo $ "paying user: " <> show toUser
            logInfo $ "paying script:" <> show toScript
            logInfo $ "time: " <> show iv

  lift $ do
    [(ref,_)] <- utxoAt vestingScript
    let tx' =  mconcat [ spendScript vestingScript ref redeemer input
                       , payToScript vestingScript (mode outDatum) (adaValue toScript)
                       , payToKey beneficiary (adaValue toUser) ]
    tx <- multisignTx ((beneficiary:signers) <> newBeneficiaries') =<< validateIn iv tx'
    --logTx tx
    submitTx beneficiary tx

unlock' :: KnownUser
        -> Signers
        -> [KnownUser]
        -> Input
        -> ToUser
        -> ToScript
        -> POSIXTimeRange
        -> AuditM ()
unlock' = unlock InlineDatum

runWithdraw ::  WithdrawConfig -> AuditM ()
runWithdraw  WithdrawConfig{..} = do
  lift $ waitUntil (validStart + seconds 10)
  input <- toInput oldInput
  unlock'
    beneficiary
    signers
    newBeneficiaries
    input
    toBeneficiary
    toScript
    (from $ validStart + seconds 10)
