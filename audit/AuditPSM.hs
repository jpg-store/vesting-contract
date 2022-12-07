{-# LANGUAGE
     NumericUnderscores,
     LambdaCase,
     RecordWildCards,
     TemplateHaskell,
     RankNTypes
#-}

module AuditPSM where

import Plutus.Model
import Canonical.Vesting
import Plutus.V2.Ledger.Api (PubKeyHash)
import Control.Monad.State
import Control.Monad
import Control.Monad.Reader
import Plutus.V1.Ledger.Time (POSIXTime, POSIXTimeRange)
import Plutus.V1.Ledger.Interval (from)
import Utils
import Data.List (foldl')

import Test.QuickCheck
import Test.Tasty

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

  --logTx tx

  submitTx usr tx

lock' :: KnownUser -> Input -> AuditM ()
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
    --logTx tx
    submitTx beneficiary tx


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

  input <- mkInput [drazen,chase,vlad] [adaPortion deadline amount]

  lock' andrea input

  lift $ waitUntil (deadline + seconds 200)

  unlock' drazen [drazen,chase] input 99 1 (from $ deadline + seconds 200)

data TestConfig = TestConfig {
  benefactor      :: !KnownUser,   -- The user who pays initial funds to the script
  inputAmount     :: !Integer,     -- The amount paid by the benefactor to the script
  beneficiaries   :: ![KnownUser], -- The users authorized to withdraw from the script
  deadlineOffset  :: !POSIXTime,    -- The difference between "now" and the deadline for each portion
  withdrawCfgs    :: ![WithdrawConfig]
}

data WithdrawConfig = WithdrawConfig {
  beneficiary    :: !KnownUser,   -- The user who withdraws the funds from the script
  withdrawOffset :: !POSIXTime,   -- The difference between "now" and the time of the withdrawal transaction
  signatories    :: ![KnownUser], -- The users who sign the withdrawal transaction
  toBeneficiary  :: !Integer,     -- The ADA value to be paid to the beneficary in the withdrawal transaction
  toScript       :: !Integer      -- The ADA value to be paid back to the script in the withdrawal transaction
}

mkDeposit :: TestConfig -> AuditM (POSIXTime, Input)
mkDeposit TestConfig{..} = do
  now <- lift currentTime

  let deadline = now + deadlineOffset

  input <- mkInput beneficiaries [adaPortion deadline inputAmount]

  lock' benefactor input

  pure (now, input)

mkWithdraw :: POSIXTime -> Input -> WithdrawConfig -> AuditM ()
mkWithdraw now input WithdrawConfig{..} = do
  lift $ wait withdrawOffset

  unlock' beneficiary signatories input toBeneficiary toScript (from $ now + withdrawOffset)

mkTest :: TestConfig -> AuditM ()
mkTest cfg@TestConfig{..} = do
  (now, input) <- mkDeposit cfg

  mapM_ (mkWithdraw now input) withdrawCfgs

simpleHappyPath :: TestConfig
simpleHappyPath = TestConfig {
  benefactor     = andrea,
  inputAmount    = 100,
  beneficiaries  = [drazen,chase,vlad],
  deadlineOffset = seconds 100,
  withdrawCfgs   = [simpleWithdrawCfg]
} where
    simpleWithdrawCfg = WithdrawConfig {
      beneficiary    = drazen,
      withdrawOffset = seconds 200,
      signatories    = [drazen,chase],
      toBeneficiary  = 100,
      toScript       = 0
    }

{- Should suceed properties:

Deposit:
   - Valid input amount (< 10_000)
   - At least one beneficiary
   - Positive deadline offset
   - valid withdrawcfgs

Withdrawal:
   - beneficiary `elem` beneficiaries
   - signatures == majority of beneficiaries
   - withdrawOffset is positive & >= currentTime (time of withdrawal)
   - toBeneficiary <= remaining value locked at script
-}

knownUsers :: [KnownUser]
knownUsers = [andrea,borja,chase,drazen,ellen,george,las,magnus,oskar,vlad]

genHappyDepositCfg :: Gen (KnownUser,Amount,[KnownUser],POSIXTime)
genHappyDepositCfg = do
  benefactor'     <- elements knownUsers
  inputAmt'       <- chooseInteger (1,10_000)
  beneficiaries'  <- sublistOf knownUsers `suchThat` (not . null)
  deadlineOffset' <- seconds <$> arbitrary @Integer `suchThat` (>= 10)
  pure (benefactor', inputAmt', beneficiaries', deadlineOffset')

genHappyWithdrawCfg :: [KnownUser] ->  StateT (POSIXTime,Integer) Gen (Maybe WithdrawConfig)
genHappyWithdrawCfg beneficiaries' = do
  (now,balance) <- get
  if balance <= 0
    then pure Nothing
    else do
      beneficiary' <- lift $ elements beneficiaries'

      withdrawOffset' <- lift $ seconds <$> arbitrary @Integer `suchThat` (>= 10)

      signatories'    <- lift $ sublistOf beneficiaries'
                            `suchThat` (\signers ->
                               length signers > (length beneficiaries' `div` 2))

      toBeneficiary' <- lift $ chooseInteger (1,balance)

      let toScript = balance - toBeneficiary'

      modify' $ const (withdrawOffset', toScript)

      pure . Just $ WithdrawConfig beneficiary' withdrawOffset' signatories' toBeneficiary' toScript

genHappyCfg :: Gen TestConfig
genHappyCfg = do
  (bfctr,inAmt,bfs,dOff) <- genHappyDepositCfg
  wdrCfgs <- evalStateT (genWithdrawConfigs bfs) (dOff,inAmt)
  pure $ TestConfig {
          benefactor     = bfctr,
          inputAmount    = inAmt,
          beneficiaries  = bfs,
          deadlineOffset = dOff,
          withdrawCfgs   = wdrCfgs
      }
 where
   genWithdrawConfigs :: [KnownUser] ->  StateT (POSIXTime,Integer) Gen [WithdrawConfig]
   genWithdrawConfigs bfs = genHappyWithdrawCfg bfs >>= \case
     Nothing -> pure []
     Just cfg -> (cfg:) <$> genWithdrawConfigs bfs


sampleHappyTests :: IO TestTree
sampleHappyTests = do
  configs <- sample' genHappyCfg
  trees <-  flip evalStateT 1 $ traverse go configs
  pure $ testGroup "happy" trees
 where
   go :: TestConfig -> StateT Int IO TestTree
   go cfg = do
     count <- get
     modify' (+1)
     pure $ testNoErrorsTrace  (adaValue 100_000) defaultConfig ("happy #" <> show count) (runAuditM $ mkTest cfg)


runHappyTests :: IO ()
runHappyTests = sampleHappyTests >>= defaultMain
