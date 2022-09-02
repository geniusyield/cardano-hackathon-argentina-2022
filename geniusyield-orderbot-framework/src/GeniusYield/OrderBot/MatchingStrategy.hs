module GeniusYield.OrderBot.MatchingStrategy (
  IndependentStrategy,
  FillType (..),
  MatchExecutionInfo (..),
  MatchResult,
  completeFill,
  partialFill,
  executionSkeleton,
) where

import Data.Aeson (ToJSON (toJSON), (.=))
import qualified Data.Aeson as Aeson
import Data.Ratio (denominator, numerator, (%))
import Data.Text (Text)
import Numeric.Natural (Natural)

import GeniusYield.DEX.Api.Liquidity (PoolInfo (..), SwapDirection, swapPool)
import GeniusYield.DEX.Api.PartialOrder (completelyFillPartialOrder, partiallyFillPartialOrder)
import GeniusYield.OrderBot.OrderBook (OrderBook)
import GeniusYield.OrderBot.Types
import GeniusYield.TxBuilder (GYTxMonad, GYTxSkeleton)
import GeniusYield.Types.TxOutRef (showTxOutRef)

{- | A matching strategy has access to the 'OrderBook' for a single asset pair, alongside all its
relevant query functions. It must produce a 'MatchResult' which has information on how to execute
the order matching transaction.
-}
type IndependentStrategy = (OrderAssetPair -> OrderBook -> MatchResult)

-- TODO: Use a smarter representation?
data MatchExecutionInfo
  = forall t. OrderExecutionInfo !FillType {-# UNPACK #-} !(OrderInfo t)
  | LiquidityPositionExecutionInfo
      !SwapDirection
      !Natural
      -- ^ How much to swap.
      !LiquidityPositionInfo

-- "smart constructors"

completeFill :: OrderInfo t -> MatchExecutionInfo
completeFill = OrderExecutionInfo CompleteFill

partialFill :: OrderInfo t -> Natural -> MatchExecutionInfo
partialFill o n = OrderExecutionInfo (PartialFill n) o

instance ToJSON MatchExecutionInfo where
  toJSON (OrderExecutionInfo fillT OrderInfo {orderRef, orderType, assetInfo, volume, price = Price {getPrice = x}}) =
    Aeson.object
      [ "utxoRef" .= showTxOutRef orderRef
      , "volumeMin" .= volumeMin volume
      , "volumeMax" .= volumeMax volume
      , "price" .= prettyRatio x
      , "commodity" .= commodityAsset assetInfo
      , "currency" .= currencyAsset assetInfo
      , "type" .= prettySOrderType orderType
      , "fillType" .= show fillT
      ]
    where
      prettyRatio x
        | denominator x == 1 = toJSON $ numerator x
        | otherwise = toJSON $ show (numerator x) <> " % " <> show (denominator x)
  toJSON (LiquidityPositionExecutionInfo swapDr diff (LiquidityPositionInfo p)) =
    Aeson.object
      [ "poolInfo" .= p
      , "swapDirection" .= swapDr
      , "diffAmount" .= diff
      ]

-- Just having this inline in "type .= ..." above fails type checking in a weird way...
prettySOrderType :: SOrderType t -> Text
prettySOrderType SBuyOrder = "Buy"
prettySOrderType SSellOrder = "Sell"

{- | "Fill" refers to the _volume_ of the order filled. Therefore, its unit is always the 'commodityAsset'.

Of course, 'CompleteFill' just means the whole order is filled, whether it's buy or sell.

'PartialFill' means slightly different things for the two order types. But the 'Natural' field within
always designates the 'commodityAsset'.

For sell orders, `PartialFill n` indicates that n amount of commodity tokens will be sold from the order,
and the respective payment will be made in the currency asset.

For buy orders, `PartialFill n` indicates that n amount of
commodity tokens should be bought, and the corresponding price (orderPrice * n), _floored_ if necessary,
must be paid by the order.

**NOTE**: The 'n' in 'PartialFill n' must not be the max volume of the order. Use 'CompleteFill' in those scenarios.
-}
data FillType = CompleteFill | PartialFill Natural deriving stock (Eq, Show)

executionSkeleton :: GYTxMonad m => MatchExecutionInfo -> m GYTxSkeleton
executionSkeleton (OrderExecutionInfo CompleteFill OrderInfo {orderRef}) = completelyFillPartialOrder orderRef
executionSkeleton (OrderExecutionInfo (PartialFill n) OrderInfo {orderRef, orderType = SSellOrder}) =
  partiallyFillPartialOrder orderRef n
executionSkeleton (OrderExecutionInfo (PartialFill n) OrderInfo {orderRef, orderType = SBuyOrder, price}) =
  partiallyFillPartialOrder orderRef . floor $ (toInteger n % 1) * getPrice price
executionSkeleton (LiquidityPositionExecutionInfo swapDr diff (LiquidityPositionInfo PoolInfo {piRef})) =
  swapPool piRef diff swapDr

{- | The result of order matching - should contain information to perform execute order and LP transactions.

Essentially, all orders (and pool swaps) in a list of 'MatchExecutionInfo's are matched with each other.

All of their tokens are put into one big transaction bucket, which is then auto balanced to pay each other.
Any extra tokens are returned to the bot wallet - this is known as arbitrage profit.
-}
type MatchResult = [MatchExecutionInfo]
