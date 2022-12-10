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
import Generators (genHappyTestConfig, emptyBeneficiariesOnInput)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure

runTestWithConfig :: TestConfig -> AuditM ()
runTestWithConfig (depCfg,ws) = do
  void $ runDeposit 0 depCfg
  lift logBalanceSheet
  runWithdrawals ws
 where
   runWithdrawals :: [WithdrawConfig] -> AuditM ()
   runWithdrawals [] = pure ()
   runWithdrawals (x:xs) =   runWithdraw x >> lift logBalanceSheet >> runWithdrawals xs

auditTests :: String -> Gen TestConfig -> IO TestTree
auditTests name genConfig = do
  tests <- zip [0..] <$> replicateM 1000 (generate genConfig)
  let trees = flip map  tests $ \(n,t) -> auditTest  (name <>" #" <> show n) (runTestWithConfig t)
  pure $ testGroup (name <> " tests") trees

happyTests :: IO TestTree
happyTests = auditTests "happy" genHappyTestConfig

emptyBeneficiariesOnInputTests :: IO TestTree
emptyBeneficiariesOnInputTests = expectFail
  <$> auditTests
      "emptyBeneficiariesOnInput"
      (emptyBeneficiariesOnInput <$> genHappyTestConfig)
