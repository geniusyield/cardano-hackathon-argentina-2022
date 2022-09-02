module GeniusYield.OrderBot (OrderBot (..), ExecutionStrategy (..), runOrderBot) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, bracket, handle, throwIO)
import Control.Monad (forever, unless)
import qualified Data.Aeson as Aeson
import Data.Foldable (fold)
import qualified Data.Text as Txt

import GeniusYield.OrderBot.Config (GYCoreConfig (GYCoreConfig, cfgNetworkId), withCfgProviders)
import GeniusYield.OrderBot.DataSource (closeDB, connectDB)
import GeniusYield.OrderBot.MatchingStrategy (
  IndependentStrategy,
  MatchExecutionInfo,
  executionSkeleton,
 )
import GeniusYield.OrderBot.OrderBook (
  populateOrderBook,
  withEachAsset,
 )

import GeniusYield.TxBuilder (runGYTxMonadNode)
import GeniusYield.Types

-- | The order bot is product type between bot info and "execution strategies".
data OrderBot = OrderBot
  { -- | Signing key of the bot.
    botSkey :: !GYPaymentSigningKey
  , -- | UTxO ref of the collateral UTxO in the bot's wallet.
    botCollateral :: !GYTxOutRef
  , -- | The execution strategy, which includes and governs the matching strategy.
    botExecutionStrat :: !ExecutionStrategy
  , -- | Function that can be used to filter out uninteresting orders/pools.
    --
    --    Before retrieving all information on an order/pool, this function will be applied
    --    on the 'GYAssetClass'(s) it contains. If it returns true for _any of the assets_
    --    contained within, the order/pool will be fetched.
    botAssetFilter :: GYAssetClass -> Bool
  , -- | Handler for exceptions that may be raised during bot run. It'll also have access to the matched orders.
    --
    --    NOTE: This handler is when used during tx build and submission step. It is assumed all other
    --    steps are pure and should not raise exceptions under usual circumstances. This also applies to the
    --    OrderBook population step (when user supplied 'populateOrderBook') is called - which is similarly
    --    assumed to not fail.
    --
    --    If you find exceptions outside of the tx build/submission step common, you may wrap 'runOrderBot' with
    --    a "Control.Exception.try" or similar and handle/restart your bot from there.
    botOnException :: [MatchExecutionInfo] -> SomeException -> IO ()
  , -- | How many microseconds to wait after a tx submission before rescanning the chain for orders.
    botRescanDelay :: Int
  }

{- |
Currently, we only have the parallel execution strategy: MultiAssetTraverse, where each order book
for each unique asset pair (see: "GeniusYield.OrderBot.Types.equivalentAssetPair") is processed
independently.
-}
newtype ExecutionStrategy = MultiAssetTraverse IndependentStrategy

runOrderBot ::
  -- | Path to the config file for the GY framework.
  FilePath ->
  OrderBot ->
  IO ()
runOrderBot
  confFile
  OrderBot
    { botSkey
    , botCollateral
    , botExecutionStrat = MultiAssetTraverse strat
    , botAssetFilter
    , botOnException
    , botRescanDelay
    } = do
    e <- Aeson.eitherDecodeFileStrict' confFile
    (netId, providers) <- case e of
      Left err -> throwIO $ userError $ "error reading config-file: " <> err
      Right cfg@GYCoreConfig {cfgNetworkId} -> do
        putStrLn $ "read configuration: " <> show cfg
        withCfgProviders cfg $ pure . (cfgNetworkId,)
    let botPkh = pubKeyHash $ paymentVerificationKey botSkey
        botAddr = addressFromPubKeyHash netId botPkh
    putStrLn $
      "Starting bot with given credentials\n"
        ++ "  Public key hash: "
        ++ show (pubKeyHashToPlutus botPkh)
        ++ "\n"
        ++ "  Address: "
        ++ Txt.unpack (addressToText botAddr)
    putStrLn $ "Starting bot at address: " ++ Txt.unpack (addressToText botAddr)
    bracket (connectDB netId providers) closeDB $ \conn -> forever $ do
      putStrLn "Rescanning for orders..."

      -- First we populate the multi asset orderbook, using the provided 'populateOrderBook'.
      book <- populateOrderBook conn botAssetFilter

      -- Now we pass each asset pair's orderbook to the provided execution strategy.
      let matchesToExecute = withEachAsset strat book

      {- This part builds and submits the transaction from the returned matches.

      This part has the highest chances of throwing exceptions, as it's extremely
      stateful. The user provided exception handler is used to wrap this flow.
      -}
      unless (null matchesToExecute) . handle (botOnException matchesToExecute) $ do
        txBody <-
          runGYTxMonadNode netId providers botAddr botCollateral
            . fmap fold
            $ traverse executionSkeleton matchesToExecute
        let tx = signTx txBody botSkey
        tid <- gySubmitTx providers tx
        putStrLn $ "Submitted order matching transaction with id: " ++ show tid

      {- Block production on the chain takes time. One has to wait for some amount of time before the
      blockchain state properly changes and another transaction can be submitted. -}
      putStrLn "Waiting to rescan for orders..."
      threadDelay botRescanDelay
