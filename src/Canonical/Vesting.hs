{-# LANGUAGE NoImplicitPrelude #-}

module Canonical.Vesting
  ( vesting
  , validator
  , Input(..)
  , Action(..)
  , Portion(..)
  , Schedule
  -- * Required for testing the vesting contract
  , Vesting
  , vestingValidator
  , vestingAddr
  , vestingValHash
  ) where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import Codec.Serialise
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import           Plutus.V1.Ledger.Credential
import           Plutus.V2.Ledger.Contexts
import           Plutus.V1.Ledger.Scripts
import           Plutus.V1.Ledger.Address
import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.Crypto
import           Plutus.V2.Ledger.Tx
import           Plutus.V1.Ledger.Time
import           Plutus.V1.Ledger.Interval
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as TypedValidators
import qualified Plutus.Script.Utils.V2.Scripts as ScriptUtils
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Canonical.Shared
import qualified Plutonomy
#include "DebugUtilities.h"

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------
data Portion = Portion
  { deadline :: POSIXTime
  , amount :: Value
  }

type Schedule = [Portion]

data Input = Input
  { beneficiaries :: [PubKeyHash]
  , schedule :: Schedule
  }

data Action = Disburse [PubKeyHash]
-------------------------------------------------------------------------------
-- Boilerplate
-------------------------------------------------------------------------------
instance Eq Portion where
  x == y
    =  deadline x == deadline y
    && amount   x == amount y

instance Eq Input where
  x == y
    =  beneficiaries x == beneficiaries y
    && schedule x == schedule y

unstableMakeIsData ''Portion
unstableMakeIsData ''Action
unstableMakeIsData ''Input
-------------------------------------------------------------------------------
{-

Batch Transaction Exploit Protection

If multiple script inputs were allowed in a single transaction, it would be
possible for a beneficiary to take an unvested amount from one vesting schedule
by combining it with a vested schedule and having the script output satisfy the
terms of both vesting schedules.

As an example, take the following vesting schedules, which both go to the same beneficiary:
  * in vesting schedule A, 10 Ada vests at 2 months and another 10 Ada vests at 4 months
  * in vesting schedule B, 10 Ada vests at 1 month and another 10 Ada vests at 2 months

If the beneficiary does not unlock any value from B until after 3 months, they can construct
a transaction that allows them to unlock 20 Ada from schedule A and zero from
schedule B. There would still be 20 Ada left locked in the script, which would satisfy
the validator for schedules A and B. The beneficiary could then create a second
transaction to unlock the value from schedule B.
-}
-------------------------------------------------------------------------------
{-# INLINABLE isScriptAddress #-}
isScriptAddress :: Address -> Bool
isScriptAddress Address { addressCredential } = case addressCredential of
  ScriptCredential _ -> True
  _ -> False

-- Verify that there is only one script input and get it's
{-# INLINABLE onlyOneScriptInput #-}
onlyOneScriptInput :: TxInfo -> Bool
onlyOneScriptInput info =
  let
    isScriptInput :: TxInInfo -> Bool
    isScriptInput = isScriptAddress . txOutAddress . txInInfoResolved

  in case filter isScriptInput . txInfoInputs $ info of
    [_] -> True
    _ ->  False

getOnlyOfThisTypeContinuingOutputsAndDatum
  :: DataConstraint(a)
  => [(DatumHash, Datum)]
  -> ValidatorHash
  -> [TxOut]
  -> (a, Value)
getOnlyOfThisTypeContinuingOutputsAndDatum datums vh outs =
  let
    thisScriptInputs = filter
      (\TxOut {..} -> addressCredential txOutAddress
        == ScriptCredential vh)
      outs
  in case thisScriptInputs of
    [TxOut {..}] -> case txOutDatum of
      OutputDatum (Datum dbs) -> (FROM_BUILT_IN_DATA("getOnlyOfThisTypeContinuingOutputsAndDatum conversion failed datum", dbs), txOutValue)
      OutputDatumHash dh -> (extractData datums dh, txOutValue)
      NoOutputDatum -> TRACE_ERROR("Missing Datum Hash")
    _ -> TRACE_ERROR("Wrong count of this script")

getOnlyInputValueOfThisScript
  :: ValidatorHash
  -> [TxInInfo]
  -> Value
getOnlyInputValueOfThisScript vh outs =
  let
    thisScriptInputs = filter
      (\TxInInfo {..} -> addressCredential (txOutAddress txInInfoResolved)
        == ScriptCredential vh)
      outs
  in case thisScriptInputs of
    [TxInInfo {..}] -> txOutValue txInInfoResolved
    _ -> TRACE_ERROR("Wrong count of this script")


signedByAMajority :: [PubKeyHash] -> [PubKeyHash] -> Bool
signedByAMajority allKeys signingKeys
  = length (filter (`elem` allKeys) signingKeys) > (length allKeys `divide` 2)
-------------------------------------------------------------------------------
-- Validator
-------------------------------------------------------------------------------
{-
This is a validator for a vesting contract. It is configured with a beneficiary
and a vesting schedule, a list of deadlines and the amount available to the
beneficiary after the deadline has passed.

At any point, the beneficiary can unlock any amount that has vested.
They are free to send the value to any address they choose.
If they only unlock a part of what's vested, they can create another
transaction to unlock the remaining  That also means when
we're 100% vested, there can still be some value locked in the script,
but the beneficiary can create another transcation to unlock that

Caveat: If the benefactor puts in too little value to begin with,
when the first vesting deadline (and possibly later deadlines)
is reached, the full value (or even a portion of it) may not be accessible
to the beneficiary. The beneficiary will have to wait until a later vesting
deadline. 100% of the value will always be accessible after all deadlines have passed.
-}
{-# INLINABLE mkValidator #-}
mkValidator :: Input -> Action -> ScriptContext -> Bool
mkValidator datum action ctx =
  let
    info :: TxInfo
    info = scriptContextTxInfo ctx

    thisValidator :: ValidatorHash
    thisValidator = ownHash ctx

  in case action of
    Disburse newKeys ->
      let
        -- Vested portions are the ones that the deadline is before
        -- the time the transaction is valid in
        isVested :: Portion -> Bool
        isVested portion = deadline portion `before` txInfoValidRange info

        -- Total value left to vest, e.g. the amount that must stay locked.
        unvested :: Value
        unvested = mconcat . fmap amount . filter (not . isVested) . schedule $ datum

        outputValid :: Bool
        outputValid = if isZero unvested
          then True
          else
            let
              locked   :: Value
              theOutDatum :: OutputDatum
              (theOutDatum, !locked) = case scriptOutputsAt thisValidator info of
                [(x, y)] -> (x, y)
                _ -> TRACE_ERROR("expected exactly one continuing output")

              newDatum :: Input
              newDatum = case theOutDatum of
                OutputDatum (Datum dbs) -> FROM_BUILT_IN_DATA("newDatum conversion failed datum", dbs)
                OutputDatumHash dh -> case findDatum dh info of
                  Nothing -> TRACE_ERROR("datum not found")
                  Just (Datum d) -> FROM_BUILT_IN_DATA("newDatum conversion failed datum hash", d)
                NoOutputDatum -> TRACE_ERROR("Missing Datum Hash")

            -- Ensure the datum has not been modified.
            in TRACE_IF_FALSE("Datum has been modified!",
                (datum { beneficiaries = newKeys } == newDatum))
              -- Make sure there is enough still locked in the script
              -- to satisfy the remainder of unvested portions to be fulfilled.
            && TRACE_IF_FALSE("Not enough value remains locked to fulfill vesting schedule",
                (locked `geq` unvested))

        signedByEnoughBeneficiaries :: Bool
        signedByEnoughBeneficiaries = signedByAMajority (beneficiaries datum) (txInfoSignatories info)

        newKeysAreNotEmpty :: Bool
        newKeysAreNotEmpty = not (null newKeys)

      in TRACE_IF_FALSE("expected exactly one script input", (onlyOneScriptInput info))
      && TRACE_IF_FALSE("Beneficiary's signature missing", signedByEnoughBeneficiaries)
      && outputValid
      && TRACE_IF_FALSE("New Beneficiaries are empty", newKeysAreNotEmpty)

-------------------------------------------------------------------------------
-- Boilerplate
-------------------------------------------------------------------------------
wrapValidator
    :: BuiltinData
    -> BuiltinData
    -> BuiltinData
    -> ()
wrapValidator = wrap mkValidator

validator :: Validator
validator = Plutonomy.optimizeUPLC $ mkValidatorScript
    $$(compile [|| wrapValidator ||])

-------------------------------------------------------------------------------
-- Entry point
-------------------------------------------------------------------------------
vesting :: PlutusScript PlutusScriptV2
vesting
  = PlutusScriptSerialised
  . SBS.toShort
  . LB.toStrict
  $ serialise
    validator


---

data Vesting

instance TypedValidators.ValidatorTypes Vesting where
  type DatumType Vesting = Input
  type RedeemerType Vesting = Action

typedValidator :: TypedValidators.TypedValidator Vesting
typedValidator =
  TypedValidators.mkTypedValidator @Vesting
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [||go||])
  where
    go = TypedValidators.mkUntypedValidator @Input @Action

vestingValidator :: Validator
vestingValidator = TypedValidators.validatorScript typedValidator

vestingAddr :: Address
vestingAddr = scriptHashAddress $ ScriptUtils.validatorHash validator

vestingValHash :: ValidatorHash
vestingValHash = ScriptUtils.validatorHash vestingValidator 
