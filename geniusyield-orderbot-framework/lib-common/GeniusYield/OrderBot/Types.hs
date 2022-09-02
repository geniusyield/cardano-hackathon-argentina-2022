module GeniusYield.OrderBot.Types (
  OrderInfo (OrderInfo, orderRef, orderType, assetInfo, volume, price),
  SomeOrderInfo (SomeOrderInfo),
  OrderAssetPair (currencyAsset, commodityAsset),
  LiquidityPositionInfo (..),
  OrderType (..),
  SOrderType (..),
  Volume (..),
  Price (..),
  mkOrderInfo,
  mkAssetPair,
  equivalentAssetPair,
  innerPoolInfo,
) where

import Data.Kind (Type)
import Data.Ratio (denominator, numerator, (%))
import Numeric.Natural (Natural)

import GeniusYield.Types.TxOutRef (GYTxOutRef)
import GeniusYield.Types.Value (GYAssetClass (..))

import GeniusYield.DEX.Api.Liquidity (PoolInfo)

-------------------------------------------------------------------------------
-- Information on DEX orders relevant to a matching strategy
-------------------------------------------------------------------------------

{- | For each unique 'OrderAssetPair' (see: 'equivalentAssetPair'): there are 2
'OrderInfo' types: 'OrderInfo BuyOrder', and 'OrderInfo SellOrder'.

For a buy order, the volume indicates the amount of 'commodityAsset' being bought. Whereas
the price indicates the requested price, in 'currencyAsset', of each 'commodityAsset' being bought.

For a sell order, the volume indicates the amount of 'commodityAsset' being sold. Whereas
the price indicates the requested price, in 'currencyAsset', for each 'commodityAsset' being sold.

See: 'mkOrderInfo'.
-}
type OrderInfo :: OrderType -> Type
data OrderInfo t = OrderInfo
  { orderRef :: GYTxOutRef
  , orderType :: SOrderType t
  , assetInfo :: OrderAssetPair
  , -- | Volume of the 'commodityAsset', either being bought or sold.
    volume :: Volume
  , -- | Price of each 'commodityAsset', in 'currencyAsset'.
    price :: Price
  }
  deriving stock (Eq, Show)

-- | Existential that can encapsulate both buy and sell orders.
data SomeOrderInfo = forall t. SomeOrderInfo (OrderInfo t)

deriving stock instance Show SomeOrderInfo

-- Q: Should owner key, owner addr, and timestamp be part of this?

{- | Given primitive data about a DEX order on the chain, canonicalize
its asset pair to determine the commodity asset and currency asset. Then,
determine the type of the order (buy/sell) and yield a 'exists t. OrderInfo t'.

Also see: 'mkAssetPair'. This function uses the same logic to determine commodity, currency, and ordertype.

== How does one deal with converting volume and price?

Inside an 'OrderInfo', the 'volume' always uses the 'commodityAsset' as its unit, whereas the 'price' uses the
'currencyAsset' as its unit. In the DEX however, all orders are really sell orders. They are offering some amount of some
asset, and asking for another.

For sell orders, where the offered asset in the DEX order is deemed to be a 'commodityAsset', there is no conversion
necessary. 'volume' is simply in terms of the offered asset amount and minFilling. Similarly, 'price' is the same as
the DEX order's price.

But what about buy orders? These are the orders that are offering an asset which is deemed to be a 'currencyAsset'.
And they are asking for an asset which is deemed to be a 'commodityAsset'.

In that case, the price is simply the DEX order's price but flipped (e.g x % y -> y % x).

The volume conversion is slightly more involved, the max volume is the DEX order's price multiplied by the
DEX order's offered amount. If the result is not a whole number, it is ceiled - because more payment is always
accepted, but less is not.

== Example

>>> mkOrderInfo
      "4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189#1"
      (GYToken "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" "GOLD")
      GYLovelace
      3
      6
      (1 % 2)
= SomeOrderInfo (OrderInfo
    { orderRef = GYTxOutRef (TxIn "4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189" (TxIx 1))
    , orderType = SSellOrder
    , assetInfo = OAssetPair
        { currencyAsset = GYLovelace
        , commodityAsset = GYToken "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" "GOLD"
        }
    , volume = Volume {volumeMin = 3, volumeMax = 6}
    , price = Price {getPrice = 1 % 2}
    })

>>> mkOrderInfo
      "4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189#2"
      GYLovelace
      (GYToken "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" "GOLD")
      5
      10
      2
= SomeOrderInfo (OrderInfo
    { orderRef = GYTxOutRef (TxIn "4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189" (TxIx 2))
    , orderType = SBuyOrder
    , assetInfo = OAssetPair {currencyAsset = GYLovelace, commodityAsset = GYToken "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" "GOLD"}
    , volume = Volume {volumeMin = 10, volumeMax = 20}
    , price = Price {getPrice = 1 % 2}
    })
-}
mkOrderInfo ::
  -- | The order reference.
  GYTxOutRef ->
  -- | The offered asset class in the order.
  GYAssetClass ->
  -- | The asked asset class in the order.
  GYAssetClass ->
  -- | The minimum filling amount (in offered asset) for the order.
  Natural ->
  -- | The total amount of the offered asset in the order.
  Natural ->
  -- | The price, in asked asset, for each offered asset in the order.
  Rational ->
  SomeOrderInfo
mkOrderInfo oref offeredAsset askedAsset minFilling offeredAmount askedPrice = case orderType of
  BuyOrder ->
    let maxVolume = ceiling $ (toInteger offeredAmount % 1) * askedPrice
        minVolume = ceiling $ (toInteger minFilling % 1) * askedPrice
     in builder SBuyOrder (Volume minVolume maxVolume) $ Price (denominator askedPrice % numerator askedPrice)
  SellOrder -> builder SSellOrder (Volume minFilling offeredAmount) $ Price askedPrice
  where
    (# oap, orderType #) = mkAssetPair offeredAsset askedAsset
    builder :: SOrderType t -> Volume -> Price -> SomeOrderInfo
    builder t vol = SomeOrderInfo . OrderInfo oref t oap vol

-------------------------------------------------------------------------------
-- Order classification components.
-------------------------------------------------------------------------------

data OrderType = BuyOrder | SellOrder deriving stock (Eq, Show)

data SOrderType t where
  SBuyOrder :: SOrderType BuyOrder
  SSellOrder :: SOrderType SellOrder

deriving stock instance Eq (SOrderType t)
deriving stock instance Show (SOrderType t)

-------------------------------------------------------------------------------
-- Order components
-------------------------------------------------------------------------------

{- | The amount of the commodity asset (being brought or sold), represented as a closed interval.

This is particularly relevant for orders that support partial filling. Indeed, for a partially fillable
order, the volume is _dynamic_. It has to be _at least_ `minFilling`, and _at most_ `offeredAmount`.

Say a partial order selling 30 A tokens for some B tokens. The order placer has set the `minFilling` to
10 - suggesting that anyone wishing to buy some of these A tokens (but not all) must buy _at least_ 10 A tokens.
Therefore, its A token 'Volume' is (10, 30).

For regular orders, where partial fills are not permitted, and one must buy the whole offered amount - 'volumeMin',
and 'volumeMax' are the same, equal to the `offeredAmount`. For example, a non-partially fillable order selling 30
A tokens for some B tokens, will have its 'Volume' set to (30, 30).

volumeMin should always be <= volumeMax. Users are responsible for maintaining this invariant.
-}
data Volume = Volume
  { -- | Minimum bound of the Order volume interval.
    volumeMin :: !Natural
  , -- | Maximum bound of the Order volume interval.
    volumeMax :: !Natural
  }
  deriving stock (Eq, Show, Ord)

-- Q: Could we use 'Int's instead in this volume interval here?

instance Semigroup Volume where
  (Volume minV1 maxV1) <> (Volume minV2 maxV2) = Volume (minV1 + minV2) (maxV1 + maxV2)
  {-# INLINEABLE (<>) #-}

instance Monoid Volume where
  mempty = Volume 0 0
  {-# INLINEABLE mempty #-}

-- | The amount of currency asset (per commodity asset) offered or asked for in an order.
newtype Price = Price {getPrice :: Rational} deriving stock (Show, Eq, Ord)

instance Semigroup Price where
  p1 <> p2 = Price $ getPrice p1 + getPrice p2
  {-# INLINEABLE (<>) #-}

instance Monoid Price where
  mempty = Price 0
  {-# INLINEABLE mempty #-}

{- | The asset pair in a DEX Order.

All 'OrderAssetPair's constructed out of equivalent raw asset pairs, must compare equal. See: 'equivalentAssetPair'.

For each unique asset pair (see: 'mkAssetPair'), one asset is chosen as the "commodity" (being sold), and the other
is chosen as the "currency" - this makes it simpler to perform order matching.
-}
data OrderAssetPair = OAssetPair
  { currencyAsset :: !GYAssetClass
  , commodityAsset :: !GYAssetClass
  }
  deriving stock (Eq, Ord, Show)

{- | Two asset pairs are considered "equivalent" (but not strictly equal, as in 'Eq'), if they contain
the same 2 assets irrespective of order. i.e (A, B) and (B, A) are equivalent.

All 'OrderAssetPair's constructed out of equivalent raw asset pairs, must compare equal.

Also see: 'mkAssetPair'.
-}
equivalentAssetPair :: (GYAssetClass, GYAssetClass) -> (GYAssetClass, GYAssetClass) -> Bool
equivalentAssetPair x@(a1, b1) y@(a2, b2) = x == y || (a1 == b2 && a2 == b1)

{- | Create a canonical 'OrderAssetPair' by marking one of the assets as a commodity, and the other
as currency. This follows the rules of 'OrderAssetPair' construction as governed by 'equivalentAssetPair'.

Depending on the offered asset or asked asset, the order type is also determined with respect to the
canonical 'OrderAssetPair'.

For buy orders, currencyAsset ≡ offeredAsset; commodityAsset ≡ askedAsset

For sell orders, currencyAsset ≡ askedAsset; commodityAsset ≡ offeredAsset

== Example

>>> mkAssetPair (GYToken "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" "GOLD") GYLovelace
= (# OAssetPair
      { currencyAsset = GYLovelace
      , commodityAsset = GYToken "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" "GOLD"
      }
    , SellOrder
  #)
>>> mkAssetPair GYLovelace (GYToken "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" "GOLD")
= (# OAssetPair
      { currencyAsset = GYLovelace
      , commodityAsset = GYToken "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" "GOLD"
      }
    , BuyOrder
  #)
-}
mkAssetPair ::
  -- | Asset class of the offered asset in the order.
  GYAssetClass ->
  -- | Asset class of the asked asset in the order.
  GYAssetClass ->
  (# OrderAssetPair, OrderType #)
mkAssetPair offeredAsset askedAsset
  | offeredAsset <= askedAsset = (# OAssetPair {commodityAsset = askedAsset, currencyAsset = offeredAsset}, BuyOrder #)
  | otherwise = (# OAssetPair {commodityAsset = offeredAsset, currencyAsset = askedAsset}, SellOrder #)

-------------------------------------------------------------------------------
-- Information on DEX LPs relevant to a matching strategy
-------------------------------------------------------------------------------

newtype LiquidityPositionInfo = LiquidityPositionInfo PoolInfo

innerPoolInfo :: LiquidityPositionInfo -> PoolInfo
innerPoolInfo (LiquidityPositionInfo p) = p
