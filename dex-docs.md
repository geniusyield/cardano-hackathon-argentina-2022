# DEX

## Table of Content

- [DEX](#dex)
  - [Table of Content](#table-of-content)
  - [Introduction](#introduction)
  - [Orders](#orders)
  - [Partially Fillable Orders](#partially-fillable-orders)
  - [Liquidity Positions](#liquidity-positions)
  - [NFT](#nft)

## Introduction

The purpose of the DEX smart contracts is to implement a _DEX_, a _Decentralized Exchange_, i.e. a means for users to swap (Cardano native) tokens,
which include ada and arbitrary custom tokens. This is _Decentralized_ in the sense that no central authority
(like a traditional, centralized exchange) is needed: Users can swap without needing to trust some authority or each other.

We anticipate two types of users, those who are primarily interested in swapping one kind of token for another and _liquidity providers_,
who facilitate swaps by providing liquidity (i.e. large amounts of tokens).
Normal users have one sort of token and want to exchange it for another.
Liquidity providers want to earn fees by making it easier for normal users to perform their swaps.

The process works as follows: Users who want to swap _place orders_ on the blockchain (in the form of an UTxO "sitting" at
a script address). They include their "offer" (the tokens they want to swap) in their order and state their price (in the datum).
Anybody who is willing to pay the price can do so by _filling_ the order, i.e. paying the price in exchange for the offered tokens.

Liquidity providers open _liquidity positions_ by placing tokens of two sorts in an UTxO on the blockchain at another script address.
These liquidity positions specify a price, and anybody can put tokens of the one sort into a position in exchange for getting tokens of the other
sort out - at the specified price.

Liquidity positions make it easier to fill orders: If an order offers some amount of a token A in exchange for some amount of a token B
and if there is a liquidity position containing A's and B's in sufficient quantity and at an appropriate price,
then the order can be filled by utilizing the liquidity position.

Orders and liquidity positions are not "aware" of each other and can be freely combined in transactions.
It is in principle possible to fill several orders in one transaction using several liquidity positions.

Anybody can fill orders and use liquidity positions for swaps. We anticipate that there will be a large number of "bots",
programs that scan the blockchain for open orders and use diverse strategies to fill them - with or without the help of liquidity positions.
Those bots can "pocket" any arbitrage they encounter, which will motivate bot authors and lead to a healthy competition among them,
resulting in orders being filled swiftly and the market becoming as efficient as possible.

Our DEX architecture is very open and extensible, but at the moment, we support two types of orders and one type of liquidity positions.
In total, we have four different smart contracts powering our DEX:

 - [_Orders_](#orders). A simple order that has to be filled in one transaction.
 - [_Partially Fillable Orders_](#partially-fillable-orders). A more complex order that can be filled in several transaction,
   each only taking part of the offered tokens and paying part of the price.
 - [_Liquidity Positions_](#liquidity-positions). Deposits of two types of tokens that can be swapped in both directions at a specified price.
 - [_NFT_](#nft). A simple NFT minting policy which is used by both partially fillable orders and liquidity positions
   to track their "identity" over their lifetime. There is no mutable state in the EUTxO-model. To simulate state, UTxO's have to be consumed,
   and new ones (with modified value and/or datum) have to be created. NFT's are passed from one UTxO to the other to provide continuity
   and to identify the old and new UTxO as representing the "same" abstract order respectively liquidity position.

## [Orders](Order.hs)

Orders contain an arbitrary value (which can in principle be a _bag_ of different tokens)
and specify a _price_ (which is again an arbitrary value, not necessarily just consisting of one token).
Their datum specifies the _pubkey hash_ of the _owner_ (the user placing the order), the owner's _address_ and the _price_.

Orders support two types of "actions" (ways to consume them), they can be _cancelled_ or _filled_.

 - A transaction _cancelling_ an order needs to be signed by the owner.
 - To _fill_ an order, the specified price needs to be paid to the owner's address.

Orders are supposed to be composable with other orders and with liquidity positions, so we must be careful to not run afoul of the
_double satisfaction problem_, where a malicious player could fill several orders but only pay once.
It is surprisingly difficult to write a validator in a way that prevents this, because validators run independently of each other
and can't communicate, so a validator can't simply "claim" a specific payment for itself.

Our solution to this problem is the same as for our [Token Sale](../TokenSale/README.md) contracts and explained
[there](../TokenSale/README.md#orders).

## [Partially Fillable Orders](PartialOrder.hs)

The partial order mechanism allows one to lock their offered tokens inside a validator, which
can then be either completely or partially filled/bought by another user - according to the price.

Each order contains a specific NFT to identify it. Its `TokenName` must be
present in the order datum upon order creation. The `CurrencySymbol` of this NFT is defined in [NFT.hs](NFT.hs) (see [NFT](#nft)!).

This order validator is parameterized over three arguments:

- The NFT minting policy ID [NFT.hs](NFT.hs)

- A constant lovelace amount to pay for the order-filler's (buyer's) transaction fees, for a single transactions.

  A total fee amount is calculated based on this initial fee parameter, and the maximum number of partial filling
  transactions that can be done for the order - this total fee amount should be present at the UTxO during initial
  construction.

  For example, if the offer amount is 110. min filling is 20, and fee is 2 Ada, then the maximum number of partial transactions
  that can be done is $\lceil 110/20\rceil = 6$. So, $6\cdot 2 = 12$ Ada should be provided to the UTxO during construction to pay for each
  of those transactions' fees.

- A constant lovelace amount to pay for the deposit after a partial fill.

  This is similar to the fee amount in purpose. This corresponds to a minimum Ada amount that has to be present in an UTxO
  alongside other tokens.

  In the above example, there can be at most 6 fillings, so at most 5 _partial_ fillings, which means the deposit should be included
  five times upon order creation.

Once an order is properly constructed, it can be interacted with in one of three ways:

1. _Cancel_ - The order is cancelled, allowing the UTxO with the offer amount to be reclaimed.

    Requirements:
      - Must be signed by the owner of the order. The owner's pub key hash is put into the order datum upon creation.
      - The NFT identifying the order must be burnt, lest it escapes into another UTxO.

2. _Complete fill_ - The full offered amount is bought, for the full price.

    Requirements:
      - Payment must be made to a UTxO at `podOwnerAddr`, containing a datum that contains the
        `TxOutRef` of the order UTxO. (i.e `ownRef` within the validator).
        The owner address is put into the datum during construction.
      - The payment must be greater than, or equal to, the price (in the asked-for asset) for the full
        offer amount, it must also include the _full_ deposit amount. However, the transaction fee for one transaction
        (corresponding to the fee parameter) may be deducted.
      - The NFT identifying the order must be burnt, lest it escapes into another UTxO.

3. _Partial fill_ - a portion of the offered amount is bought, for the corresponding price.

    Requirements:
      - The portion/amount must be positive, but less than the remaing total amount,
        and it must be greater than or equal to `podMinFilling`, which is put
        inside the datum during order creation. This is effectively the minimum amount one must buy even during a partial filling.
      - Payment must be made to a UTxO at `podOwnerAddr`, containing a datum that contains the
        `TxOutRef` of the order UTxO. (i.e `ownRef` within the validator)
        The owner address is put into the datum during construction.
      - The payment must be greater than, or equal to, the price (in the asked-for asset) for the corresponding portion
        of the offer amount, it must also include the deposit amount (single UTxO deposit), as given by the parameter.
        However, the transaction fee for one transaction (corresponding to the fee parameter) may be deducted.
      - A single continuing output must be present, with the remaining amount inside, alongside an updated datum.
        In particular, the `podOfferedAmount` will be subtracted by the amount that has just been sold, and one deposit can be deducted.

## [Liquidity Positions](Liquidity.hs)

Liquidity providers can _open_ a _liquidity position_ by placing two sorts of tokens (with arbitrary amounts) at a UTxO
"sitting" at the script address. Valid liquidity positions need to contain an [NFT](#nft) to track their identity,
and they need to specify various parameters in the datum.

The most important operation supported by such a liquidity position is that of _swapping_ one sort of tokens for the other:
A transaction consumes the UTxO and recreates a new one at the same address with adjusted amounts of tokens (according to the price).

Each such swapping transaction needs to pay a _fee_ to the liquidity provider. This fee is paid in the token being swapped
and remains in the position, so that the amount of tokens in the position grows with each swap.
Part[^1] of that fee is meant for GeniusYield, but also remains in the position until it is withdrawn or the position is closed.

The _datum_ contains the following information:

```haskell
data LiquidityDatum = LiquidityDatum
    { ldOwner     :: !PubKeyHash                           -- ^ Pubkey hash of the owner (i.e. the liquidity provider).
    , ldNFT       :: !TokenName                            -- ^ Token name of the NFT tracking the identity of the liquidity position.
    , ldFrom      :: !AssetClass                           -- ^ One of the tokens in the position.
    , ldTo        :: !AssetClass                           -- ^ The other token in the position.
    , ldPrice     :: !(PlutusTx.Integer, PlutusTx.Integer) -- ^ The price for one unit of the "from" token in terms of the "to" token, given as numerator and denominator of the actual rational price.
    , ldFee       :: !(PlutusTx.Integer, PlutusTx.Integer) -- ^ The fee due for each swap, given as numerator and denominator of the actual rational proportion.
    , ldGYFeeFrom :: !PlutusTx.Integer                     -- ^ GeniusYield fees in terms of the "from" token contained in the position.
    , ldGYFeeTo   :: !PlutusTx.Integer                     -- ^ GeniusYield fees in terms of the "to" token contained in the position.
    , ldMinFrom   :: !PlutusTx.Integer                     -- ^ Minimal amount of "from" tokens that need to be swapped.
    }
```

For a liquidity position to be considered valid, a number of conditions have to be met. Our offchain code will not consider invalid positions,
and nobody else should either - all bets are off for invalid position.
The validator must guarantee that no operation causes a position to become invalid.
These condiftions are:

 - The specified NFT must be present in the position.
 - `ldFrom` and `ldTo` must be different.
 - `ldPrice` must be positive.
 - `ldFee` must not be negative.
 - `ldGYFeeFrom` must not be negative, and the amount of "from" tokens in the position must be greater or equal than `ldGYFeeFrom`.
 - `ldGYFeeTo` must not be negative, and the amount of "to" tokens in the position must be greater or equal than `ldGYFeeTo`.

The following operations are supported:

```haskell
data LiquidityAction = Swap        -- ^ Perform a swap.
                     | CollectFees -- ^ Collect the GeniusYield fees.
                     | Update      -- ^ Update the parameters of the position.
                     | Close       -- ^ Close the position.
```

These operations work as follows:

 - `Swap`: A transaction consumes the position and recreates it with updated values and datum.
   Let us consider an example where the position contains 127 "from" tokens and 315 "to" tokens,
   where the price is `(3,2)` (i.e. 1.5), the fees are `(1,20)` (i.e. 5%) and the GeniusYield fees are 10%.
   Let us assume that `ldGYFeeFrom` is 10 and `ldGYFeeTo` is 5, i.e. 10 of the 127 "from" tokens belong to Geniusyield,
   as do 5 of the 315 "to" tokens.

   A user wants to swap 104 "from" tokens for "to" tokens.

   The fees are 5% of 104, i.e. 5.2, which are rounded up to 6 "from" tokens. Of those, 10% go to GeniusYield, which is - we round up again -
   one "from" token.

   104 - 6 = 98 tokens remain for the swap. Using the price of 1.5, that gives us 147 "to" tokens[^2].
   So the user places 104 "from" tokens in the position and gets 147 "to" tokens out of it.
   The new position contains 127+104=231 "from" tokens (of which 10+1=11 belong to GeniusYield) and 315-147=168 "to" tokens
   (of which 5 belong to GeniusYield).

   The datum of the new UTxO is identical to the old datum with exception of the `ldGYFeeFrom` field, which is now 11 instead of 10.

   Let us now assume that another user wants to swap 77 "to" tokens.

   The fees are 5% of 77, which is 3.85, rounded up to 4. 10% of that, again rounded up, is one token for GeniusYield.
   Of the 77 "to" tokens, 77-4=73 remain for the swap. Seeing as we swap in the _opposite_ direction, the price is now 2/3,
   which gives us 48.666... "from" tokens. These are rounded _down_ to 48.

   The user therefore puts 77 "to" tokens in, leaving 168+77=245 "to" tokens, and takes 48 out, leaving 183. Of those,
   11 "from" tokens belong to GeniusYield, as do 5+1=6 "to" tokens.

   The datum of the newest UTxO is again identical except for the `ldGYFeeTo` field, which is now 6.

   Users can swap arbitrary amounts, but they obviously can only get out what is there. So in our example, a user swapping "from" tokens
   can at most receive 245-6=239 "to" tokens, and a user swapping "to" tokens can at most receive 183-11=172 "from" tokens.
   She would probably refrain from swaps giving her less than the price dictates, but she is free to do so. All the validator checks
   is that the liquidity provider and GeniusYield get what is their due
   (and that the NFT remains in the position and that the datum is updated correctly).

 - `CollectFees`: GeniusYield can submit a transaction to collect their fees.
   Such a transaction must be signed by GeniusYield, it must (at most) take out the amount specified by the `ldGYFeeFrom` and
   `ldGYFeeTo` fields, and it must update the datum to `ldGYFeeFrom` = `ldGYFeeTo` = 0.

 - `Update`: The liquidity provider can update certain datum fields and take liquidity out or put new liquidity in, provided

     - The NFT remains in the position.
     - The GeniusYield fee remains in the position (and is not changed in the datum).
     - The two assets "from" and "to" are not changed.
     - The position remains valid (positive price, non-negative fees).

 - `Close`: The liquidity provider can close the position, provided the NFT is burnt and GeniusYield is paid their fees[^3].

## [NFT](NFT.hs)

The purpose of this minting policy is to guarantee that every token with this policy - independent of its token name - is an NFT,
i.e. a _non-fungible token_, a token that can only exist at most once.
The minting policy is _not_ parameterized and thus statically known, which makes it easy to use in other contracts that are in need of an NFT.
Once one has convinced oneself that each token with this policy is indeed an NFT, one can simply use this policy whenever an NFT is required.

This is achieved by making use of a redeemer for the minting transaction. The redeemer specifies a UTxO (via its `TxOutRef`) that has to be
consumed by the minting transaction. The minting policy checks that the specified UTxO is indeed consumed,
that exactly one token is minted (whereas arbitrary _burning_ is allowed)
and that the token name of that one token is the _hash_ of the `TxOutRef` of that UTxO.

Since UTxO's are unique and can only exist once, this guarantees that only one token with this minting policy and token name can ever be minted
(except, of course, if there is a _hash collision_, but we deem this risk to be negligible).

[^1]: The ratio of fees going to GeniusYield is a global system parameter.
[^2]: If the amount of "to" tokens was not integral, we would round _down_ to the next integer.
