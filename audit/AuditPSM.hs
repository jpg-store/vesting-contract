{-# LANGUAGE
     NumericUnderscores,
     LambdaCase,
     RecordWildCards
#-}

module AuditPSM where

import Plutus.Model
import Plutus.Model.Validator.V1
import Canonical.Vesting
import Plutus.V2.Ledger.Api (PubKeyHash)
import Control.Monad (replicateM)
import Plutus.Model.Pretty
import Test.Tasty

vestingScript :: TypedValidator Input Action
vestingScript = mkTypedValidator untypedValidator

defaultConfig :: MockConfig
defaultConfig = defaultAlonzo

initChain :: Mock
initChain = initMock defaultConfig $ adaValue 100_000

setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 10 $ newUser $ adaValue 10_000

runTestIO :: forall a. String -> Run a -> IO ()
runTestIO nm = defaultMain .  testNoErrors (adaValue 100_000) defaultConfig nm

data Users = Users {
   andrea :: !PubKeyHash,
   borja  :: !PubKeyHash,
   chase  :: !PubKeyHash,
   drazen :: !PubKeyHash,
   ellen  :: !PubKeyHash,
   george :: !PubKeyHash,
   las    :: !PubKeyHash,
   magnus :: !PubKeyHash,
   oskar  :: !PubKeyHash,
   vlad   :: !PubKeyHash
}

identifyUsers :: Run Users
identifyUsers = setupUsers >>= \case
  [a, b, c, d, e, g, l, m, o, v] -> pure $ Users a b c d e g l m o v
  _                              -> error "boom!"

superSimpleTest :: Run ()
superSimpleTest = do
  Users{..} <- identifyUsers
  now <- currentTime
  let amount = adaValue 100
  spending <- spend andrea amount
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
