{-# LANGUAGE
     NumericUnderscores,
     LambdaCase,
     RecordWildCards,
     TemplateHaskell,
     RankNTypes
#-}

module Generators where

import Plutus.Model
import Canonical.Vesting
import Plutus.V2.Ledger.Api (PubKeyHash)
import Control.Monad.State
import Utils
import GHC.Natural
import Test.QuickCheck
import Config


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
   - locked amount == unvested amount
-}

type Seconds = Integer

genSchedule' :: Seconds -> Amount -> Gen Schedule'
genSchedule' _ 0 = pure []
genSchedule' time amt  = do
  time' <- chooseInteger (time,1_000_000)
  amt'  <- chooseInteger (1,amt)
  let port = Portion' (seconds time') amt'
  if amt - amt' == 0
    then pure [port]
    else (port:) <$> genSchedule' time amt


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

-- maybe move this somewhere else?
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
