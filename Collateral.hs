module Main where

import Control.Exception (throwIO)
import System.Environment (getArgs)

import qualified Data.Aeson as Aeson

import GeniusYield.TxBuilder
import GeniusYield.Types

import GeniusYield.OrderBot.Config

parseArgs :: IO (FilePath, FilePath)
parseArgs = do
  args <- getArgs
  case args of
    [configPath, signingKeyPath] -> pure (configPath, signingKeyPath)
    _ ->
      throwIO . userError $
        "expected exactly three command line arguments, in order: \n"
          ++ "  1. Path to the GY framework config-file\n"
          ++ "  2. Path to the signing key to create a collateral for\n"

main :: IO ()
main = do
  (confFile, skeyPath) <- parseArgs
  res <- Aeson.eitherDecodeFileStrict' confFile
  (netId, providers) <- case res of
    Left err -> throwIO $ userError $ "error reading config-file: " <> err
    Right cfg@GYCoreConfig {cfgNetworkId} -> do
      putStrLn $ "read configuration: " <> show cfg
      withCfgProviders cfg $ pure . (cfgNetworkId,)
  -- The path to new user signing key
  skey <- readPaymentSigningKey skeyPath
  let addr = addressFromPubKeyHash netId . pubKeyHash $ paymentVerificationKey skey
      -- dummy utxo ref, doesn't matter.
      collateral = "b0b27bf4e5415f7509eddefdb48e833750204d20c35edfd2c28331be3e62fcc5#1"
  -- create collateral
  txBody <-
    runGYTxMonadNode netId providers addr collateral . pure $
      mustHaveOutput
        GYTxOut
          { gyTxOutAddress = addr
          , gyTxOutDatum = Nothing
          , gyTxOutValue = valueFromLovelace 5_000_000 -- 5 ada collateral
          }
  let tx = signTx txBody skey
  tid <- gySubmitTx providers tx
  putStrLn $ "collateral: " ++ show tid ++ "#1"
