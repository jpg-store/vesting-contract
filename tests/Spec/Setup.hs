{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Spec.Setup
  ( AuditM
  , Return
  , Users(..)
  , UserData(..)
  , User(..)
  , shouldHave
  , execVestingTest
  , fromUsers
  , withUser
  )
  where

import BotPlutusInterface.Types (LogContext (ContractLog), LogLevel (Error), LogType (AnyLog))
import Test.Plutip.Internal.BotPlutusInterface.Wallet (BpiWallet)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (State, execState,modify)
import Data.Default (Default(def))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Ledger (PaymentPubKeyHash(PaymentPubKeyHash))
import Plutus.Contract (Contract, EmptySchema)
import Plutus.Contract qualified as Contract
import Test.Plutip.Contract (TestWallets(TestWallets), assertExecutionWith, withContractAs, TestWallet (twExpected), ValueOrdering (VEq))
import Test.Plutip.Contract.Types (TestWallet(TestWallet))
import Test.Plutip.Options (TraceOption(ShowTraceButOnlyContext))
import Test.Plutip.Internal.Types (ClusterEnv)
import Data.List.NonEmpty (NonEmpty)
import Test.Tasty (TestTree)
import Ledger.Value (Value)
import qualified Ledger.Ada as Ada


defCollateralSize :: Integer
defCollateralSize = 10_000_000

defWallet :: TestWallet
defWallet = TestWallet [fromInteger defCollateralSize, 10_000_000_000] Nothing

defPPKH :: PaymentPubKeyHash
defPPKH = PaymentPubKeyHash "72cae61f85ed97fb0e7703d9fec382e4973bf47ea2ac9335cab1e3fe" 

type family Context (user :: UserData) where
  Context 'Wallet = TestWallet
  Context 'PKH = PaymentPubKeyHash

data UserData = Wallet | PKH

data User = Andrea
          | Borja
          | Chase
          | Drazen
          | Ellen
          | Gergo
          | Las
          | Magnus
          | Oskar
          | Vlad
            deriving stock (Show, Enum, Bounded, Eq, Ord)

newtype Users d = Users { getUsers :: Map User (Context d) }
                  deriving stock (Generic)

instance Default (Users 'Wallet) where
  def = Users $ Map.fromList $ zip [minBound..maxBound] (repeat defWallet)

instance Default (Users 'PKH) where
  def = Users $ Map.fromList $ zip [minBound..maxBound] (repeat defPPKH)

deriving stock instance (Show (Context d)) => Show (Users d)

type AuditM a = ReaderT (Users 'PKH) (Contract () EmptySchema Text) a
type Return = (TestWallets, IO (ClusterEnv, NonEmpty BpiWallet) -> TestTree)
type WalletState = State (Users 'Wallet) ()


-- execVestingTest :: (String
--                   -> Test.Plutip.Contract.TestRunner w e a
--                   -> [Predicate w e a]
--                   -> (TestWallets, IO (ClusterEnv, NonEmpty BpiWallet) -> TestTree))
execVestingTest name s = assertExecutionWith [ShowTraceButOnlyContext ContractLog $ Error [AnyLog]]
                          name
                          (toWallets $ execState (s :: WalletState) def)

-- withUser :: (User
--                   -> ReaderT (Users 'PKH) (Contract w0 s e0) a0
--                   -> Test.Plutip.Contract.TestRunner w0 e0 a0)
withUser user m = withContractAs (fromEnum (user :: User)) $ \ppkhs -> do
                  ppkh <- Contract.ownFirstPaymentPubKeyHash

                  let users :: Users 'PKH
                      users = Users
                            $ Map.fromList
                            $ zip [minBound .. maxBound] (insertAt (fromEnum user) ppkh ppkhs)

                  runReaderT m users
  where

    insertAt :: Int -> t -> [t] -> [t]
    insertAt 0 x as     = x:as
    insertAt n x (a:as) = a : insertAt (n - 1) x as
    insertAt _ x []     = [x]

toWallets :: Users 'Wallet -> TestWallets
toWallets = TestWallets . NonEmpty.fromList . fmap snd .  Map.toList . getUsers

fromUsers :: (User -> Bool) -> Users d -> [Context d]
fromUsers f = map snd . Map.toList . Map.filterWithKey (\k _ -> f k) . getUsers

shouldHave :: User -> Value -> WalletState
shouldHave user value = 
  modify (Users
          . Map.update
              (\w -> Just $ w {twExpected = Just (VEq, value <> Ada.lovelaceValueOf defCollateralSize )}) user
          . getUsers)
  
