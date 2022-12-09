{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (ReaderT), ask, lift)
import Control.Monad (void)
import Data.Default (def)
import Test.Plutip.Internal.BotPlutusInterface.Wallet (
  BpiWallet,
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
import Data.Time (NominalDiffTime)
import Test.Plutip.Config (PlutipConfig)
import Numeric.Positive (Positive)

main :: IO ()
main = do
  let
      slotLen :: NominalDiffTime
      slotLen = 0.2

      plutipConfig :: PlutipConfig
      plutipConfig = def

      addSomeWalletWithCollateral :: [Positive] -> ReaderT ClusterEnv IO BpiWallet
      addSomeWalletWithCollateral funds =
        addSomeWallet (toAda 10 : funds)

      numWallets :: Int
      numWallets = 5

      walletFunds :: [[Positive]]
      walletFunds = replicate numWallets [toAda 10_000]

  putStrLn "Starting cluster..."
  (st, _) <- startCluster plutipConfig $ do

    wallets <- mapM addSomeWalletWithCollateral walletFunds

    mapM_ (`CI.awaitWalletFunded` slotLen) wallets

    liftIO $ putStrLn "Waiting for wallets to be funded..."

    separate

    mapM_ printWallet $ zip wallets [1..]


  putStrLn "Cluster is running. Press Enter to stop."
    >> void getLine
  putStrLn "Stopping cluster"

  stopCluster st

  where

    mkscript :: [BpiWallet] -> ReaderT ClusterEnv IO ()
    mkscript wallets = do
      cEnv <- ask

      let nodeSocketPath = last $ words $ show $ nodeSocket cEnv

      return ()

    printNodeRelatedInfo = ReaderT $ \cEnv -> do
      putStrLn $ "Node socket: " <> last (words $ show (nodeSocket cEnv))

    separate = liftIO $ putStrLn "\n------------\n"

    printWallet :: (BpiWallet, Int) -> ReaderT ClusterEnv IO ()
    printWallet (w, n) = liftIO $ do
      putStrLn $ "Wallet " ++ show n ++ " PKH: " ++ show (walletPkh w)
      putStrLn $ "Wallet " ++ show n ++ " mainnet address: " ++ show (mkMainnetAddress w)

    toAda = (* 1_000_000)
