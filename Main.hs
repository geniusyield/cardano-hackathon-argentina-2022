{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Exception (throwIO)
import Data.Bifunctor
import qualified Data.ByteString.Lazy as BSL
import Data.Function ((&))
import GHC.Natural
import System.Environment (getArgs)

import qualified Data.Aeson.Encode.Pretty as AesonE

import GeniusYield.OrderBot
import GeniusYield.OrderBot.MatchingStrategy (
  MatchExecutionInfo,
  completeFill,
  partialFill,
 )
import GeniusYield.OrderBot.OrderBook.List
import GeniusYield.OrderBot.Types (
  OrderInfo (..),
  OrderType (BuyOrder, SellOrder),
  Volume (..),
 )
import GeniusYield.Types

sampleStrategy :: OrderBook -> [MatchExecutionInfo]
sampleStrategy OrderBook {..} =
  if price (highestBuy buyOrders) >= price (lowestSell sellOrders)
    then go (unOrders sellOrders) (unOrders buyOrders)
    else []
  where
    go :: [OrderInfo 'SellOrder] -> [OrderInfo 'BuyOrder] -> [MatchExecutionInfo]
    go [] _ = []
    go _ [] = []
    go (order : ss) bs =
      findMatches vh bs & \case
        (matches, bs') -> if null matches then go ss bs' else matches
      where
        Volume vl vh = volume order

        findMatches ::
          Natural ->
          [OrderInfo 'BuyOrder] ->
          ([MatchExecutionInfo], [OrderInfo 'BuyOrder])
        findMatches 0 bos = ([completeFill order], bos)
        findMatches v []
          | (vh - v) > vl = ([partialFill order (vh - v)], [])
          | otherwise = ([], [])
        findMatches v (bo : bos)
          | v == maxFillBO = ([completeFill order, completeFill bo], bos)
          | v > maxFillBO && v >= minFillBO =
            findMatches (v - maxFillBO) bos & \case
              ([], _) -> ([], bo : bos)
              (ms, bos') -> (completeFill bo : ms, bos')
          | v < maxFillBO
              && v >= minFillBO
              && (vh - v) > vl =
            ([completeFill order, partialFill bo v], bos)
          | otherwise = second (bo :) $ findMatches v bos
          where
            Volume minFillBO maxFillBO = volume bo

parseArgs :: IO FilePath
parseArgs = do
  args <- getArgs
  case args of
    [file] -> return file
    _ ->
      throwIO . userError $
        "expected exactly three command line arguments, in order: \n"
          ++ "  1. Path to the GY framework config-file\n"

main :: IO ()
main = do
  confFile <- parseArgs
  skey <- readPaymentSigningKey "bot.skey"
  let collateral = "5b8b20b5b66defb1fd73a3399a7f1d898d8af948890424d94efb4612713570ab#1"
  runOrderBot confFile $
    OrderBot
      { botSkey = skey
      , botCollateral = collateral
      , botExecutionStrat = MultiAssetTraverse $ \_ bk -> sampleStrategy bk
      , botRescanDelay = 50_000_000 -- 50 Seconds
      , botAssetFilter = const True -- Fetch everything!
      , botOnException = \matchSet err -> do
          print err
          BSL.putStr $ AesonE.encodePretty matchSet
          putStrLn "\n"
      }
