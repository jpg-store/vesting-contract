{-# LANGUAGE
     NumericUnderscores,
     LambdaCase,
     RecordWildCards,
     TemplateHaskell,
     RankNTypes
#-}
{-# LANGUAGE StandaloneDeriving #-}

module AuditPSM where

import Plutus.Model
import Canonical.Vesting
import Plutus.V2.Ledger.Api (PubKeyHash)
import Control.Monad.State
import Control.Monad

import Plutus.V1.Ledger.Interval (from)
import Utils
import Data.List (foldl')
import GHC.Natural

import Test.QuickCheck

import Text.Pretty.Simple

import qualified Data.Text.Lazy  as T

import Config
import Plutus.V1.Ledger.Time

type Amount = Integer

deriving instance Show Portion

logTx :: Tx -> Run ()
logTx = logInfo . T.unpack . pShow

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

multisignTx :: [PubKeyHash] -> Tx -> Run Tx
multisignTx usrs tx = foldM (flip signTx) tx usrs

payToKeys :: [PubKeyHash] -> Amount -> Tx
payToKeys usrs amt = foldl' (\acc u -> payToKey u (adaValue amt) <> acc) mempty usrs

type ToUser = Amount
type ToScript = Amount
type Signers = [KnownUser]

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



mkSchedule :: POSIXTime -> POSIXTime -> Integer -> Integer -> Schedule' -> Schedule'
mkSchedule _ _ _ 0 acc = reverse acc
mkSchedule now offset perPort ports  acc
        =  mkSchedule (now + offset) offset perPort (ports - 1) (Portion' (now + offset)  perPort : acc)

mkDeposit :: POSIXTime -> DepositConfig -> AuditM (POSIXTime, Input')
mkDeposit now DepositConfig{..} = do
  let perPortion = inputAmount `div` portions

      sched = mkSchedule now deadlineOffset perPortion portions []

  let input' = Input' beneficiaries sched
  input <- toInput input'

  lock' benefactor input

  pure (now, input')

mkWithdraw ::  WithdrawConfig -> AuditM ()
mkWithdraw  WithdrawConfig{..} = do
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


simpleHappyPath :: TestConfig
simpleHappyPath = testConfig depositCfg [withdrawCfg]
  where
    depositCfg = DepositConfig {
      benefactor     = andrea,
      inputAmount    = 100,
      beneficiaries  = [drazen,chase,vlad],
      deadlineOffset = seconds 100,
      portions       = 1
    }
    withdrawCfg = WithdrawConfig {
      beneficiary    = drazen,
      signers    = [drazen,chase],
      newBeneficiaries = [drazen],
      toBeneficiary  = 100,
      toScript       = 0,
      validStart  = seconds 200,
      oldInput = Input' [drazen,chase,vlad] [Portion' (seconds 100) 100]
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
   - output datum == input datum {beneficiaries == newKeys} -- newKeys = arg to disburse
   - locked amount == unvested amount -- *

-}

knownUsers :: [KnownUser]
knownUsers = [andrea,borja,chase,drazen,ellen,george,las,magnus,oskar,vlad]


genHappyDepositCfg :: Gen DepositConfig
genHappyDepositCfg = do
  benefactor'     <- elements knownUsers
  portions'       <- chooseInteger (1,10)
  inputAmt'       <- chooseInteger (1,10_000) `suchThat` (\x -> x `div` portions' == 0)
  beneficiaries'  <- sublistOf knownUsers `suchThat` (not . null)
  deadlineOffset' <- seconds <$> arbitrary @Integer `suchThat` (>= 10)
  pure $ DepositConfig benefactor' inputAmt' beneficiaries' deadlineOffset' portions'

genHappyWithdrawCfg :: Input' -> StateT Natural Gen (Maybe (WithdrawConfig,Input'))
genHappyWithdrawCfg (Input' [] _) = pure Nothing
genHappyWithdrawCfg (Input' _ []) = pure Nothing
genHappyWithdrawCfg inp@(Input' bs ps) = get >>= \i -> case ps !? i of
  Nothing -> pure Nothing
  Just (Portion' dl amt) ->  modify' (+1) >>  do
    beneficiary      <- lift $ elements bs
    newBeneficiaries <- lift $ sublistOf knownUsers `suchThat` (not . null)
    signatories      <- lift $ sublistOf bs `suchThat` (\ss -> length ss > length bs `div` 2)
    let newInput = updateInput' newBeneficiaries inp
        locked = unvested dl ps
        cfg = WithdrawConfig beneficiary signatories newBeneficiaries dl amt  (max 0 (locked - amt)) inp
    pure . Just $ (cfg, newInput)

genHappyTestConfig :: Gen TestConfig
genHappyTestConfig = do
  depCfg@DepositConfig{..} <- genHappyDepositCfg
  let sch = mkSchedule 0 deadlineOffset (inputAmount `div` portions) portions []
      inp = Input' beneficiaries sch
  withdrawals <- flip evalStateT 0 $ genHappyWithdrawals inp
  pure (depCfg,withdrawals)
 where
   genHappyWithdrawals :: Input' -> StateT Natural Gen [WithdrawConfig]
   genHappyWithdrawals inp = genHappyWithdrawCfg inp >>= \case
     Nothing -> pure []
     Just (cfg,newInp) -> (cfg:) <$> genHappyWithdrawals newInp

runTestWithConfig :: TestConfig -> AuditM ()
runTestWithConfig (depCfg,ws) = do
  void $  mkDeposit 0 depCfg
  lift logBalanceSheet
  runWithdrawals ws
 where
   runWithdrawals :: [WithdrawConfig] -> AuditM ()
   runWithdrawals [] = pure ()
   runWithdrawals (x:xs) =   mkWithdraw x >> lift logBalanceSheet >> runWithdrawals xs

runHappyTests :: IO ()
runHappyTests = do
  tests <- zip [0..] <$> sample' genHappyTestConfig
  forM_ tests $ \(n,t) -> runAuditTest  ("happy #" <> show n) (runTestWithConfig t)
