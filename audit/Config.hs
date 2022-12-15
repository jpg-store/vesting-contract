module Config where

import Utils
import Plutus.V1.Ledger.Time (POSIXTime)
import Canonical.Vesting (Input (..), Portion (..))
import Plutus.Model.Ada (adaValue)
import Data.List (foldl')

data DepositConfig = DepositConfig {
  benefactor      :: !KnownUser,   -- The user who pays initial funds to the script
  inputAmount     :: !Integer,     -- The amount paid by the benefactor to the script
  beneficiaries   :: ![KnownUser], -- The users authorized to withdraw from the script
  deadlineOffset  :: !POSIXTime,   -- The difference between "now" and the deadline for the first portion (and between portions)
  portions        :: !Schedule'
}

data WithdrawConfig = WithdrawConfig {
  beneficiary      :: !KnownUser,
  signers          :: ![KnownUser],
  newBeneficiaries :: ![KnownUser],
  validStart       :: !POSIXTime,
  toBeneficiary    :: !Integer,
  toScript         :: !Integer,
  oldInput         :: !Input'
}

totalLocked :: [WithdrawConfig] -> Integer
totalLocked [] = 0
total (WithdrawConfig{..}:ws) = toBeneficiary + total ws

type TestConfig = (DepositConfig,[WithdrawConfig])

testConfig :: DepositConfig -> [WithdrawConfig] -> TestConfig
testConfig = (,)

-- Variant datum types for testing with ADA only values (for simplicity)
data Portion' = Portion' {deadline' :: POSIXTime, amount' :: Integer} deriving (Show, Eq, Ord)

type Schedule' = [Portion']

data Input' = Input' {
  beneficiaries' :: [KnownUser],
  schedule' :: Schedule'
}

toInput :: Input' -> AuditM Input
toInput (Input' bs' ps') = do
  bs <- users bs'
  let ps =  map (\(Portion' dl amt) -> Portion dl (adaValue amt)) ps'
  pure $ Input bs ps

updateInput' :: [KnownUser] -> Input' -> Input'
updateInput' bs (Input' _ sch) = Input' bs sch

unvested :: POSIXTime -> Schedule' -> Integer
unvested now   = foldl' (\acc p -> amount' p + acc) 0
                            . filter (\t -> deadline' t >= now)
