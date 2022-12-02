{-# LANGUAGE
     NumericUnderscores,
     LambdaCase,
     RecordWildCards,
     TemplateHaskell,
     RankNTypes
#-}

module Utils where

import Plutus.Model
import Plutus.Model.Validator.V1
import Canonical.Vesting
import Plutus.V2.Ledger.Api (PubKeyHash)
import Control.Monad (replicateM)
import Plutus.Model.Pretty
import Test.Tasty
import Control.Lens
    ( (<&>), view, makeLenses, ReifiedLens(Lens), ReifiedLens' )
import Control.Monad.Reader
import qualified Data.Time.Clock.POSIX as Time
import Plutus.V1.Ledger.Time (POSIXTime)
import Plutus.V1.Ledger.Interval (from)


{- PSM Mock Blockchain Setup -}
defaultConfig :: MockConfig
defaultConfig = defaultAlonzo

setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 10 $ newUser $ adaValue 10_000

runTestIO :: forall a. String -> Run a -> IO ()
runTestIO nm = defaultMain .  testNoErrorsTrace (adaValue 100_000) defaultConfig nm

{- Typed Validator -}

type Vesting = TypedValidator Input Action

vestingScript :: Vesting
vestingScript = mkTypedValidator untypedValidator


{- Users -}
data Users = Users {
   _andrea' :: !PubKeyHash,
   _borja'  :: !PubKeyHash,
   _chase'  :: !PubKeyHash,
   _drazen' :: !PubKeyHash,
   _ellen'  :: !PubKeyHash,
   _george' :: !PubKeyHash,
   _las'    :: !PubKeyHash,
   _magnus' :: !PubKeyHash,
   _oskar'  :: !PubKeyHash,
   _vlad'   :: !PubKeyHash
}

makeLenses ''Users

-- Because we're stuck on a GHC < 9.2.5 we need this to avoid impredicative types errors
type KnownUser = ReifiedLens' Users PubKeyHash

andrea, borja, chase, drazen, ellen, george, las, magnus, oskar, vlad :: KnownUser
andrea = Lens andrea'
borja = Lens borja'
chase = Lens chase'
drazen = Lens drazen'
ellen = Lens ellen'
george = Lens george'
las = Lens las'
magnus = Lens magnus'
oskar = Lens oskar'
vlad = Lens vlad'

-- Create our users
identifyUsers :: Run Users
identifyUsers = setupUsers >>= \case
  [a, b, c, d, e, g, l, m, o, v] -> pure $ Users a b c d e g l m o v
  _                              -> error "boom!"

{- PSM's `Run` is typically used at the bottom of a transformer stack,
   we use a ReaderT Users stack to reduce redundant code -}

type AuditM a = ReaderT Users Run a

runAuditM :: AuditM a -> Run a
runAuditM ma = identifyUsers >>= runReaderT ma

runAuditTest :: String -> AuditM () -> IO ()
runAuditTest msg ma = runTestIO msg (runAuditM ma)

user :: KnownUser -> AuditM PubKeyHash
user (Lens u) = ask <&> view u

users :: [KnownUser] -> AuditM [PubKeyHash]
users usrs = withUsers usrs pure

withUser :: KnownUser -> (PubKeyHash -> AuditM a) -> AuditM a
withUser u f = user u >>= f

withUsers :: [KnownUser] -> ([PubKeyHash] -> AuditM a) -> AuditM a
withUsers us f = mapM user us >>= f

{- Utilities for constructing datums and redeemers -}

adaPortion :: POSIXTime -> Integer -> Portion
adaPortion t amt = Portion t $ adaValue amt

disburse :: [KnownUser] -> AuditM Action
disburse users = withUsers users $ pure . Disburse

mkInput :: [KnownUser] -> Schedule -> AuditM Input
mkInput users sch = withUsers users $ pure . flip Input sch

-- We should run tests in both Inline and Hash modes, this makes signatures less verbose
type Mode t = (DatumType t -> DatumMode (DatumType t))

{- Misc -}

type Pred a = a -> Bool

unique :: Pred b -> [b] -> Maybe b
unique p bs = case filter p bs of
  [b] -> Just b
  _   -> Nothing

inputThat :: Pred Input -> Pred (TxBox Vesting)
inputThat p = p . txBoxDatum
