{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Spec.Utils
  ( toDatum
  , toRedeemer
  , totalValueInInput
  , findVestingUtxo
  , mkVesting
  , mkVesting'
  , getVestingUtxos
  , unlockVesting
  , unlockVesting'
  , usersPPkhs
  , mintNativeTokens
  )
  where

import Control.Monad.Reader ( asks, MonadTrans(lift) )
import Data.Map (Map)
import Data.Map          qualified as Map
import Data.Text (Text)
import Data.Void (Void)
import Canonical.Vesting (Input(..), Schedule, Action (..), Vesting)
import Canonical.Vesting qualified as Vesting
import Ledger
    ( TxOutRef,
      TxId,
      Value,
      ChainIndexTxOut(ScriptChainIndexTxOut),
      Datum(..),
      Redeemer(Redeemer),
      Versioned (..),
      Language (PlutusV2),
      MintingPolicy,
      MintingPolicyHash, TokenName, PaymentPubKeyHash, POSIXTimeRange
    )
import Ledger qualified
import Ledger.Constraints qualified as Constraints
import Plutus.Contract (Contract, EmptySchema)
import Plutus.Contract   qualified as Contract
import PlutusTx (ToData, toBuiltinData, fromBuiltinData)
import Spec.Setup
import BotPlutusInterface.Constraints (submitBpiTxConstraintsWith, mustValidateInFixed)
import Control.Monad (void)

mkVesting :: [User] -> Schedule -> AuditM (Input,TxId)
mkVesting users schedule = do
  ppkhs <- asks (fmap Ledger.unPaymentPubKeyHash . fromUsers (`elem` users))

  let vestingDatum = Input ppkhs schedule

      txSK = Constraints.mustPayToOtherScriptWithInlineDatum
               Vesting.vestingValHash
               (toDatum vestingDatum)
               (totalValueInInput vestingDatum)

  txId <- lift $ submitBpiTxConstraintsWith @Void mempty txSK []
  lift $ Contract.awaitTxConfirmed $ Ledger.getCardanoTxId txId
  return (vestingDatum, Ledger.getCardanoTxId txId)

mkVesting' :: [User] -> Schedule -> Value -> AuditM (Input,TxId)
mkVesting' users schedule val = do
  ppkhs <- asks (fmap Ledger.unPaymentPubKeyHash . fromUsers (`elem` users))

  let vestingDatum = Input ppkhs schedule

      txSK = Constraints.mustPayToOtherScriptWithInlineDatum
               Vesting.vestingValHash
               (toDatum vestingDatum)
               val

  txId <- lift $ submitBpiTxConstraintsWith @Void mempty txSK []
  lift $ Contract.awaitTxConfirmed $ Ledger.getCardanoTxId txId
  return (vestingDatum, Ledger.getCardanoTxId txId)

unlockVesting :: TxOutRef -> [User] -> Input -> Action -> AuditM (Input, TxId)
unlockVesting oref users input action = do

  void $ lift $ Contract.waitNSlots 5

  utxos <- getVestingUtxos
  ppkhs <- mapM userPPkh users
  (_, startTime) <- lift Contract.currentNodeClientTimeRange

  let
      firstUser = head ppkhs
      newInput = input{beneficiaries=newBenficiaries}
      (Disburse newBenficiaries) = action
      validationTime = Ledger.from startTime


      txSk = mconcat
        [ Constraints.mustSpendScriptOutput oref (toRedeemer action)
        , Constraints.mustPayToOtherScriptWithDatumInTx Vesting.vestingValHash (toDatum newInput) mempty
        , Constraints.mustPayToPubKey firstUser (vestedValue validationTime newInput)
        , mconcat (map Constraints.mustBeSignedBy ppkhs)
        ]

      lkps = mconcat
        [ Constraints.unspentOutputs utxos
        , Constraints.otherScript (Versioned Vesting.vestingValidator PlutusV2)
        ]

  txId <- lift $ submitBpiTxConstraintsWith @Vesting lkps txSk (mustValidateInFixed validationTime)

  lift $ Contract.awaitTxConfirmed $ Ledger.getCardanoTxId txId
  pure (newInput, Ledger.getCardanoTxId txId)

unlockVesting' :: TxOutRef -> [User] -> Input -> Action -> Value -> AuditM (Input, TxId)
unlockVesting' oref users input action val = do

  void $ lift $ Contract.waitNSlots 5

  utxos <- getVestingUtxos
  ppkhs <- mapM userPPkh users
  (_, startTime) <- lift Contract.currentNodeClientTimeRange

  let
      firstUser = head ppkhs
      newInput = input{beneficiaries=newBenficiaries}
      (Disburse newBenficiaries) = action
      validationTime = Ledger.from startTime


      txSk = mconcat
        [ Constraints.mustSpendScriptOutput oref (toRedeemer action)
        , Constraints.mustPayToOtherScriptWithDatumInTx Vesting.vestingValHash (toDatum newInput) mempty
        , Constraints.mustPayToPubKey firstUser val
        , mconcat (map Constraints.mustBeSignedBy ppkhs)
        ]

      lkps = mconcat
        [ Constraints.unspentOutputs utxos
        , Constraints.otherScript (Versioned Vesting.vestingValidator PlutusV2)
        ]

  txId <- lift $ submitBpiTxConstraintsWith @Vesting lkps txSk (mustValidateInFixed validationTime)

  lift $ Contract.awaitTxConfirmed $ Ledger.getCardanoTxId txId
  pure (newInput, Ledger.getCardanoTxId txId)

findVestingUtxo :: Vesting.Input -> AuditM (Maybe TxOutRef)
findVestingUtxo vestingDatum = do
  utxos <-  lift $ traverse getValidatorDatum =<< Contract.utxosAt Vesting.vestingAddr

  let refAndDatums :: Map TxOutRef (Maybe Vesting.Input)
      refAndDatums = fmap (>>= (fromBuiltinData . getDatum)) utxos

  case Map.toList (Map.filter (== Just vestingDatum) refAndDatums) of
    ((oref,_):_) -> return (Just oref)
    _         -> return Nothing

  where

    getValidatorDatum :: ChainIndexTxOut -> Contract () EmptySchema Text (Maybe Ledger.Datum)
    getValidatorDatum ScriptChainIndexTxOut{..} = do
      Contract.datumFromHash (fst _ciTxOutScriptDatum)
    getValidatorDatum _                         = return Nothing

mintNativeTokens :: MintingPolicy -> MintingPolicyHash -> TokenName -> AuditM TxId
mintNativeTokens mp mpsh tn = do
  let txSk = Constraints.mustMintCurrencyWithRedeemer mpsh Ledger.unitRedeemer tn 10_000
      lkps = Constraints.mintingPolicy (Versioned mp PlutusV2)

  txId <- lift $ submitBpiTxConstraintsWith @Void lkps txSk []
  lift $ Contract.awaitTxConfirmed $ Ledger.getCardanoTxId txId
  pure (Ledger.getCardanoTxId txId)
  

getVestingUtxos :: AuditM (Map TxOutRef ChainIndexTxOut)
getVestingUtxos = lift $ Contract.utxosAt Vesting.vestingAddr

totalValueInInput :: Input -> Value
totalValueInInput = mconcat . fmap Vesting.amount . Vesting.schedule

toDatum :: forall a. ToData a => a -> Datum
toDatum = Datum . toBuiltinData

toRedeemer :: forall a. ToData a => a -> Redeemer
toRedeemer = Redeemer . toBuiltinData

vestedValue :: POSIXTimeRange -> Input -> Value
vestedValue timeRange =  foldr (\portion totalAmt -> if predicate portion then Vesting.amount portion <> totalAmt else totalAmt) mempty . Vesting.schedule

  where

    predicate :: Vesting.Portion -> Bool
    predicate portion = Vesting.deadline portion `Ledger.before` timeRange

usersPPkhs = withUser Andrea  (asks getUsers :: AuditM (Map User Ledger.PaymentPubKeyHash))

userPPkh :: User -> AuditM PaymentPubKeyHash
userPPkh user = do
  mppkh <- asks (Map.lookup user . getUsers)
  case mppkh of
    Just ppkh -> return ppkh
    Nothing -> error ("User: " <> show user <> " is not present in the map. This is not possible!")
