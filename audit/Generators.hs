{-# LANGUAGE
     NumericUnderscores,
     LambdaCase,
     RecordWildCards,
     TemplateHaskell,
     RankNTypes
#-}

module Generators where

import Plutus.Model
import Canonical.Vesting ()
import Plutus.V2.Ledger.Api (PubKeyHash)
import Control.Monad.State
import Utils
import GHC.Natural
import Test.QuickCheck
import Config
import Data.List
import qualified Data.Set as S
import Control.Lens (set,ix)
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

isUnique :: (Eq a, Ord a) => [a] -> Bool
isUnique xs = length xs == S.size (S.fromList xs)

emptyBeneficiariesOnInput :: TestConfig -> TestConfig
emptyBeneficiariesOnInput (d,ws) = (d {beneficiaries = []},ws)

-- not very performant, shouldn't matter that much in practice
overRandomElem :: (a -> a) -> [a] -> Gen [a]
overRandomElem f as = do
  i <- chooseInt (0,length as)
  let a  = as !! i
      a' = f a
  pure $ set (ix i) a' as

overRandomWithdrawConfig :: (WithdrawConfig -> WithdrawConfig) -> TestConfig -> Gen TestConfig
overRandomWithdrawConfig f (d,ws) = overRandomElem f ws >>= \ws' -> pure (d,ws')

emptyNewBeneficiariesInWithdrawals :: TestConfig -> Gen TestConfig
emptyNewBeneficiariesInWithdrawals = overRandomWithdrawConfig $ \w -> w {newBeneficiaries = []}

emptySignersInWithdrawals :: TestConfig -> Gen TestConfig
emptySignersInWithdrawals = overRandomWithdrawConfig $ \w -> w {signers = []}



type Seconds = Integer

genSchedule' :: Seconds -> Amount -> Gen Schedule'
genSchedule' _ 0 = pure []
genSchedule' time amt  = do
  time' <- chooseInteger (time+1001,1_000_000_000)
  amt'  <- chooseInteger (1,amt)
  let port = Portion' (seconds time') amt'
  if amt - amt' == 0
    then pure [port]
    else (port:) <$> genSchedule' time' (amt - amt')

genHappyDepositCfg :: Gen DepositConfig
genHappyDepositCfg = do
  benefactor'     <- elements knownUsers
  inputAmt'       <- chooseInteger (1,10_000)
  portions'       <- sortOn deadline' <$> genSchedule' 100 inputAmt' `suchThat` isUnique
  beneficiaries'  <- sublistOf knownUsers `suchThat` (not . null)
  pure $ DepositConfig
           benefactor'
           inputAmt'
           beneficiaries'
           (seconds 100)
           portions'

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
  let inp = Input' beneficiaries portions
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
    sched =  [Portion' (seconds 100) 100]
    depositCfg = DepositConfig {
      benefactor     = andrea,
      inputAmount    = 100,
      beneficiaries  = [drazen,chase,vlad],
      deadlineOffset = seconds 100,
      portions       = sched
    }
    withdrawCfg = WithdrawConfig {
      beneficiary    = drazen,
      signers    = [drazen,chase],
      newBeneficiaries = [drazen],
      toBeneficiary  = 100,
      toScript       = 0,
      validStart  = seconds 200,
      oldInput = Input' [drazen,chase,vlad] sched
    }
