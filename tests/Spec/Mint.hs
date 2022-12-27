{-# LANGUAGE NoImplicitPrelude #-}
module Spec.Mint (mkPolicy, currencySymbol, mintingPolicy, mintingPolicyHash, vestingAC, vestingToken) where

import Ledger qualified
import Plutus.Script.Utils.V2.Scripts qualified as ScriptUtils
import Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies (mkUntypedMintingPolicy)
import Plutus.V2.Ledger.Contexts
import PlutusTx qualified
import PlutusTx.Prelude
import Ledger.Value qualified as Value

{-# INLINEABLE mkPolicy #-}
mkPolicy :: () -> ScriptContext -> Bool
mkPolicy _ ctx =
  traceIfFalse "Let me mint" _check
  where
    _info = scriptContextTxInfo ctx
    _check :: Bool
    _check = 1 == (1 :: Integer)
    _someWork = sort [1..100] :: [Integer]

mintingPolicy :: ScriptUtils.MintingPolicy
mintingPolicy =
  Ledger.mkMintingPolicyScript
    $$(PlutusTx.compile [||mkUntypedMintingPolicy mkPolicy||])

currencySymbol :: Ledger.CurrencySymbol
currencySymbol = ScriptUtils.scriptCurrencySymbol mintingPolicy

mintingPolicyHash :: Ledger.MintingPolicyHash
mintingPolicyHash = Ledger.mintingPolicyHash (Ledger.Versioned mintingPolicy Ledger.PlutusV2)

vestingToken :: Ledger.TokenName
vestingToken = "VestingToken"

vestingAC :: Ledger.AssetClass
vestingAC = Value.assetClass currencySymbol vestingToken

