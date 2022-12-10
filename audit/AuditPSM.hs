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
import Utils
import GHC.Natural
import Test.QuickCheck
import Config
import Endpoints
import Generators (genHappyTestConfig)

runTestWithConfig :: TestConfig -> AuditM ()
runTestWithConfig (depCfg,ws) = do
  void $ runDeposit 0 depCfg
  lift logBalanceSheet
  runWithdrawals ws
 where
   runWithdrawals :: [WithdrawConfig] -> AuditM ()
   runWithdrawals [] = pure ()
   runWithdrawals (x:xs) =   runWithdraw x >> lift logBalanceSheet >> runWithdrawals xs

runHappyTests :: IO ()
runHappyTests = do
  tests <- zip [0..] <$> sample' genHappyTestConfig
  forM_ tests $ \(n,t) -> runAuditTest  ("happy #" <> show n) (runTestWithConfig t)
