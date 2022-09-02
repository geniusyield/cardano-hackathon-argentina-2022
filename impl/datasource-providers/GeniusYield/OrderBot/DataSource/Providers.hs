{-# LANGUAGE RecordWildCards #-}

module GeniusYield.OrderBot.DataSource.Providers (
  Connection,
  connectDB,
  closeDB,
  withEachAssetOrders,
  queryLiquidityPositions,
) where

import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import GeniusYield.DEX.Api.Liquidity
import GeniusYield.DEX.Api.PartialOrder
import GeniusYield.OrderBot.Types
import GeniusYield.TxBuilder
import GeniusYield.Types

data Connection = Connection !GYNetworkId {-# UNPACK #-} !GYProviders

type OrderData = (# OrderAssetPair, [OrderInfo 'BuyOrder], [OrderInfo 'SellOrder] #)

connectDB :: GYNetworkId -> GYProviders -> IO Connection
connectDB netId providers = pure $ Connection netId providers

closeDB :: Connection -> IO ()
closeDB = const $ return ()

withEachAssetOrders :: Connection -> (GYAssetClass -> Bool) -> (a -> OrderData -> a) -> a -> IO a
withEachAssetOrders c assetFilter f acc = do
  infoMap <- allOrderInfos c assetFilter
  pure $
    Map.foldlWithKey'
      ( \acc oaip someOrderInfos ->
          let (buys, sells) =
                foldl'
                  ( \(!buys, !sells) (SomeOrderInfo oInf@OrderInfo {orderType}) -> case orderType of
                      SBuyOrder -> (oInf : buys, sells)
                      SSellOrder -> (buys, oInf : sells)
                  )
                  ([], [])
                  someOrderInfos
           in f acc (# oaip, buys, sells #)
      )
      acc
      infoMap

queryLiquidityPositions :: Connection -> (GYAssetClass -> Bool) -> IO [LiquidityPositionInfo]
queryLiquidityPositions c assetFilter = map LiquidityPositionInfo . Map.elems <$> runQuery c (listPools assetFilter)

runQuery :: Connection -> GYTxQueryMonadNode a -> IO a
runQuery (Connection nid providers) = runGYTxQueryMonadNode nid providers

allOrderInfos :: Connection -> (GYAssetClass -> Bool) -> IO (Map OrderAssetPair [SomeOrderInfo])
allOrderInfos c assetFilter = do
  partialOrderInfos <- runQuery c $ partialOrders assetFilter
  return $ foldl' f Map.empty partialOrderInfos
  where
    f m (partialOrderInfoToOrderInfo -> info@(SomeOrderInfo OrderInfo {assetInfo})) =
      Map.insertWith (++) assetInfo [info] m

partialOrderInfoToOrderInfo :: PartialOrderInfo -> SomeOrderInfo
partialOrderInfoToOrderInfo PartialOrderInfo {..} =
  mkOrderInfo poiRef poiOfferedAsset poiAskedAsset poiMinFilling poiOfferedAmount $ rationalToGHC poiPrice
