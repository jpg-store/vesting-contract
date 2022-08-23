{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Cardano.Api hiding (TxId, Value)
import qualified Cardano.Api.Shelley as Shelley
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Options.Applicative as OA
import           Plutus.V1.Ledger.Api
import qualified Plutus.V1.Ledger.Time as Time

import           Canonical.Vesting
import           Data.List.Split
import qualified PlutusTx.AssocMap as M
import           Text.Read
import           Data.String
import qualified PlutusTx.Prelude as PlutusTx
import           PlutusTx.These
import Options.Generic
import qualified Data.Text as T

instance ParseField PubKeyHash where
  parseField x y z w = fromString <$> parseField x y z w
  readField = fromString <$> readField

instance ParseFields PubKeyHash where
  parseFields x y z w = fromString <$> parseFields x y z w

instance ParseRecord PubKeyHash where
  parseRecord = fromString <$> parseRecord

main :: IO ()
main = run =<< getRecord "Escrow Compiler"

parseValue :: String -> Maybe Value
parseValue = parseValue' . words

parseValue' :: [String] -> Maybe Value
parseValue' = go mempty where
  go (Value acc) xs = case xs of
    [] -> Just $ Value acc
    [countStr] -> do
      count <- readMaybe countStr
      let
        thePolicyId = adaSymbol
        tokenName = adaToken

      Just $ Value
                 $ these id id (\x y -> these id id (+) PlutusTx.<$> M.union x y) PlutusTx.<$> M.union (M.singleton thePolicyId $ M.singleton tokenName count) acc
    countStr:asset:rest -> do
      count <- readMaybe countStr
      (thePolicyId, tokenName) <- case splitOn "." asset of
        [thePolicyId, tokenName] -> Just (fromString thePolicyId, fromString tokenName)
        _
          | asset == "lovelace" -> Just (adaSymbol, adaToken)
          | otherwise -> Nothing

      let newAcc = Value
                 $ these id id (\x y -> these id id (+) PlutusTx.<$> M.union x y) PlutusTx.<$> M.union (M.singleton thePolicyId $ M.singleton tokenName count) acc

      go newAcc rest

data WriteOptions = WriteOptions
  { woOutput :: FilePath
  }
  deriving(Generic)

data DatumOptions = DatumOptions
  { doOutput :: Maybe FilePath
  , doBeneficiaries :: [PubKeyHash]
  , doPortion :: [Portion]
  }
  deriving(Generic)

data Options
  = WriteScript WriteOptions
  | WriteDatum  DatumOptions
  deriving(Generic)

fieldModifier :: Modifiers
fieldModifier = lispCaseModifiers
  { fieldNameModifier = fieldNameModifier lispCaseModifiers . drop 2
  }

instance ParseRecord WriteOptions where
  parseRecord = parseRecordWithModifiers fieldModifier
instance ParseRecord DatumOptions where
  parseRecord = parseRecordWithModifiers fieldModifier
instance ParseRecord Options where
  parseRecord
    = subparser
    $  command "write" (WriteScript <$> info parseRecord idm)
    <> command "datum" (WriteDatum <$> info parseRecord idm)

parsePortionFromString :: String -> Either String Portion
parsePortionFromString s =
  case break (== ':') s of
    (ts, ':' : as) -> Portion <$> parseDeadline <*> parseAmount where
      parseDeadline = case reads @Integer ts of
        [(r, "")] -> Right . Time.fromMilliSeconds . fromInteger $ r * 1000
        _ -> Left . mconcat $ [ "Cannot parse deadline `", ts, "`" ]
      parseAmount = case Main.parseValue as of
        Nothing -> Left "Failed to parse Value"
        Just v -> Right v

    (_, _) -> Left "Missing amount"

parsePortionOpt :: ReadM Portion
parsePortionOpt = eitherReader parsePortionFromString

instance ParseRecord Portion where
  parseRecord = option parsePortionOpt . mconcat $
      [ long "portion"
      , OA.metavar "<deadline>:<amount>"
      , help "When an amount vests. Deadline is seconds since the Unix epoch (1970-01-01 00:00 UTC). Amount is in lovelaces. Can appear multiple times."
      ]

instance ParseField Portion where
  parseField msg longName shortName _ = option parsePortionOpt
      $  maybe mempty (long . T.unpack) longName
      <> maybe mempty short shortName
      <> OA.metavar "<deadline>:<amount>"
      <> maybe mempty (help . T.unpack) msg

  readField = parsePortionOpt

instance ParseFields Portion

run :: Options -> IO ()
run = \case
  WriteScript WriteOptions {..} -> writeSC woOutput
  WriteDatum DatumOptions {..} -> writeDatum doOutput doBeneficiaries doPortion

minAda :: Integer
minAda = 1000000

writeSC :: FilePath -> IO ()
writeSC output = do
  result <- writeFileTextEnvelope output Nothing vesting
  case result of
    Left err -> print $ displayError err
    Right () -> putStrLn $ "wrote validator to file " ++ output

writeDatum :: Maybe FilePath -> [PubKeyHash] -> Schedule -> IO ()
writeDatum output beneficiaries schedule = write . plutusDataToJSON $ datum where
  write = maybe LBS.putStr LBS.writeFile output
  datum = Input beneficiaries schedule

plutusDataToJSON :: ToData a => a -> LBS.ByteString
plutusDataToJSON
  = Aeson.encode
  . (scriptDataToJson ScriptDataJsonDetailedSchema)
  . Shelley.fromPlutusData
  . toData
