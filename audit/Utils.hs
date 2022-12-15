{-# LANGUAGE
     NumericUnderscores,
     LambdaCase,
     RecordWildCards,
     TemplateHaskell,
     RankNTypes
#-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Utils where

import Plutus.Model
import Plutus.Model.Validator.V2
import Canonical.Vesting
import Plutus.V2.Ledger.Api (PubKeyHash)
import Test.Tasty
import Control.Lens
    ( (<&>), view, makeLenses, ReifiedLens(Lens), ReifiedLens' )
import Control.Monad.Reader
import Plutus.V1.Ledger.Time (POSIXTime)
import GHC.Natural (Natural)
import Text.Pretty.Simple
import qualified Data.Text.Lazy  as T



{- PSM Mock Blockchain Setup -}
defaultConfig :: MockConfig
defaultConfig = defaultBabbage

setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 10 $ newUser $ adaValue 10_000

mkTestTree :: forall a. String -> Run a -> TestTree
mkTestTree  = testNoErrors (adaValue 100_000) defaultConfig

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
data  KnownUser = KnownUser String (ReifiedLens' Users PubKeyHash)

instance Eq KnownUser where
  (KnownUser a _) == (KnownUser b _) = a == b

instance Ord KnownUser where
  (KnownUser a _) <= (KnownUser b _) = a <= b

andrea, borja, chase, drazen, ellen, george, las, magnus, oskar, vlad :: KnownUser
andrea = KnownUser "andrea" $ Lens andrea'
borja  = KnownUser "borja"  $ Lens borja'
chase  = KnownUser "chase"  $ Lens chase'
drazen = KnownUser "drazen" $ Lens drazen'
ellen  = KnownUser "ellen"  $ Lens ellen'
george = KnownUser "george" $ Lens george'
las    = KnownUser "las"    $ Lens las'
magnus = KnownUser "magnus" $ Lens magnus'
oskar  = KnownUser "oskar"  $ Lens oskar'
vlad   = KnownUser "vlad"   $ Lens vlad'

knownUsers :: [KnownUser]
knownUsers = [andrea,borja,chase,drazen,ellen,george,las,magnus,oskar,vlad]

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

auditTest :: String -> AuditM () -> TestTree
auditTest msg ma = mkTestTree msg (runAuditM ma)

user :: KnownUser -> AuditM PubKeyHash
user (KnownUser _ (Lens u)) = ask <&> view u

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
disburse usrs = withUsers usrs $ pure . Disburse

mkInput :: [KnownUser] -> Schedule -> AuditM Input
mkInput usrs sch = withUsers usrs $ pure . flip Input sch


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

updateInput :: [PubKeyHash] -> Input -> Input
updateInput bs (Input _ sch) = Input bs sch

(!?) :: [a] -> Natural -> Maybe a
[]     !? _     = Nothing
(x:_)  !? 0 = Just x
(_:xs) !? n = xs !? (n-1)

logTx :: Tx -> Run ()
logTx = logInfo . T.unpack . pShow

multisignTx :: [PubKeyHash] -> Tx -> Run Tx
multisignTx usrs tx = foldM (flip signTx) tx usrs

-- orphan instance to help w/ testing

deriving instance Show Portion

type Amount = Integer
type ToUser = Amount
type ToScript = Amount
type Signers = [KnownUser]
