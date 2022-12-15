{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (ReaderT))
import Control.Monad (void)
import Data.Default (def)
import Test.Plutip.Internal.BotPlutusInterface.Wallet (
  BpiWallet (..),
  addSomeWallet,
  mkMainnetAddress,
  walletPkh,
 )
import Test.Plutip.Internal.LocalCluster (
  startCluster,
  stopCluster,
 )
import Test.Plutip.Internal.Types (
  ClusterEnv,
  nodeSocket,
 )
import Test.Plutip.Tools.ChainIndex qualified as CI
import Test.Plutip.Config (PlutipConfig)
import System.Directory

main :: IO ()
main = do
  let
      slotLen = 2

      plutipConfig :: PlutipConfig
      plutipConfig = def

      addSomeWalletWithCollateral funds =
        addSomeWallet (toAda 10 : funds)

      numWallets :: Int
      numWallets = 5

      walletFunds = replicate numWallets [toAda 10_000]

  putStrLn "Starting cluster..."
  (st, _) <- startCluster plutipConfig $ do

    wallets <- mapM addSomeWalletWithCollateral walletFunds

    mapM_ (`CI.awaitWalletFunded` slotLen) wallets

    liftIO $ putStrLn "Waiting for wallets to be funded..."

    separate

    mapM_ printWallet $ zip wallets [1..]
    printNodeRelatedInfo

    liftIO (setupwallets wallets)


  putStrLn "Cluster is running. Press Enter to stop."
    >> void getLine
  putStrLn "Stopping cluster"

  stopCluster st
  removeDirectoryRecursive "./temp"

  where

    basePath :: String
    basePath = "./temp/mainnet"

    setupwallets :: [BpiWallet] -> IO ()
    setupwallets (x:xs) = do
      createDirectoryIfMissing True basePath
      setupwallet "benefactor" x

      let walletNames = zipWith (\n y -> y ++ show n) [( 1 :: Int )..] (replicate (length xs) "beneficiary")
      mapM_ (uncurry setupwallet) $ zip walletNames xs

    setupwallet :: String -> BpiWallet -> IO ()
    setupwallet name wallet = do
      writeFile (basePath ++ name ++ "-pkh.txt") $ show (walletPkh wallet)
      writeFile (basePath ++ name ++ ".addr") (mkMainnetAddress wallet)
      writeFile (basePath ++ name ++ ".skey") $ show (signKey wallet)
 

    printNodeRelatedInfo = ReaderT $ \cEnv -> do
      putStrLn $ "Node socket: " <> last (words $ show (nodeSocket cEnv))

    separate = liftIO $ putStrLn "\n------------\n"

    printWallet :: (BpiWallet, Int) -> ReaderT ClusterEnv IO ()
    printWallet (w, n) = liftIO $ do
      putStrLn $ "Wallet " ++ show n ++ " PKH: " ++ show (walletPkh w)
      putStrLn $ "Wallet " ++ show n ++ " mainnet address: " ++ show (mkMainnetAddress w)

    toAda = (* 1_000_000)
