# geniusyield-orderbot

This is an order matching bot implementation that is meant to be modular and polymorphic. It uses backpack to support this goal.

Solid resource for learning backpack: https://github.com/danidiaz/really-small-backpack-example

# Development environment

In theory, you only really need `cabal` (and similar Haskell tooling) - and some generally expected system libraries and tooling to work with this project. You should also have `cardano-cli` available to generate a signing key.

You may also need to install a few system libraries: `libsecp256k1` and `libsodium`. Depending on your linux distribution, you may have to install the `-dev` versions which include the C headers.

In particular, a sufficiently updated version of `libsecp256k1` must be available. Older versions do not have `secp_extrakeys.h` which is required. If your distribution does not have up to date packages, you may have to build from source. `libsecp256k1` should also be built with the flags `--enable-module-schnorrsig --enable-experimental`.

If you want to use a haskell.nix based environment instead which includes all these (but takes a long time to initialize the first time), you can use the provided `flake.nix`.

Of course, you must have [nix](https://nixos.org/download.html) set up with the [flakes feature enabled](https://nixos.wiki/wiki/Flakes#Enable_flakes).

You must also set up the IOG binary caches following [the official guide](https://github.com/input-output-hk/haskell.nix/blob/master/docs/tutorials/getting-started.md#setting-up-the-binary-cache). If you do not do this, you'll end up building several copies of GHC from source - which is an extremely long process that may potentially fail.

Once you have the above all set up, you can run `nix develop` within the project root which will drop you inside a shell with everything you need, including `cabal`. After this point, you can do `cabal build all` and similar commands. You can also utilize the provided Makefile commands, such as `make orderbot-blockfrost` - which will start the orderbot with the `config-blockfrost.json` config.

Do note that the first time you set up this nix shell, it will take _a long_ time to build everything. However, next runs should be fast unless you severely modify the dependencies. Adding some deps from hackage into your cabal file is fine - but modifying the index-state may lead to build failures as newer packages might not be supported by the libraries! If you must use hackage packages newer than what is provided by the index-state, ask the organizers for help. They may be able to provide you with cabal `constraints` and `allow-newer` stanzas to fix build failures.

## Setting up config files

The two provided configs are missing the respective API key/token, which you will have to provide.

# Components

The orderbot has three modular components, two of them are modularized through backpack, and one of them is modularized just by virtue of being a higher order function.

The backpack-modularized components are:
- The data source where the orderbook is populated from (`lib-datasource`)
- The orderbook data structure (`lib-orderbook`)

The other component is the matching strategy. Which is a function that has access to an orderbook for each order asset pair, and performs order matchings to yield information on which orders/pools to match.

The returned information is then used to create and submit matching transactions.

# Adding implementation for components

The implementation for the data source component is already provided for participants to use. It is defined in `impl/datasource-providers`.

Participants will have to implement their own orderbook. An example implementation is provided in `impl/orderbook-list`. But of course, it is intentionally suboptimal! However, it should serve as a good example to get started with and even run the bot with!

You'll note that the Orderbook signature defines many orderbook querying functions. These are only provided as hints for the participants. You are not required to be limited by only using these functions in your strategy - you may have more functions here that you import in your `Main` file! Though you must provide at least these functions to make sure the signature is compatible with the implementation.

There is extensive documentation on understanding the types that are to expected to be used by all orderbook implementations, in `GeniusYield.OrderBot.Types`. It explains concepts with regards to asset pairs, orders and how they are separated into buy/sell etc.

You should **especially read** `mkOrderInfo` if you want to understand how DEX orders (which are always "sell" orders) are distinguished into buy and sell orders. This distinction is made to make it easier for the matching strategy writer to match orders.

# Provided implementations and running the bot

Once you have these two implementations, you can link them up in the `Main` file that will contain the bot execution itself. The provided `Main` file uses the provided datasource component (which Participants will also use), as well as the example `orderbook-list` implementation. You may check out the respective stanza in the `geniusyield-orderbot.cabal` file for this executable (`geniusyield-orderbot-exe`), which showcases how cabal mixins may be used to link up backpack signatures with their implementations:

```cabal
executable         geniusyield-orderbot-exe
  import:          common-lang
  import:          common-deps
  import:          common-ghc-opts
  main-is:         Main.hs
  build-depends:
    , geniusyield-orderbot-framework
    , geniusyield-orderbot-framework:common
    , geniusyield-orderbot:datasource-providers
    , geniusyield-orderbot:orderbook-list
  mixins:
    , geniusyield-orderbot:orderbook-list requires
        (GeniusYield.OrderBot.DataSource as GeniusYield.OrderBot.DataSource.Providers)
    , geniusyield-orderbot-framework requires
        ( GeniusYield.OrderBot.DataSource as GeniusYield.OrderBot.DataSource.Providers
        , GeniusYield.OrderBot.OrderBook as GeniusYield.OrderBot.OrderBook.List
        )
  ghc-options:
    -O2 -threaded -rtsopts -with-rtsopts=-N
```

Most likely, participants will replace the `geniusyield-orderbot:orderbook-list` dependency with their own implementation library. Once they have done that, the mixin must be accurately updated.

Consider, a custom orderbook implementation defined in a cabal sublibrary `geniusyield-orderbot:orderbook-tree`. Suppose, the implementation module in said library is: `GeniusYield.OrderBot.OrderBook.Tree`. Then, the new stanza would look like:

```cabal
executable         geniusyield-orderbot-exe
  import:          common-lang
  import:          common-deps
  import:          common-ghc-opts
  main-is:         Main.hs
  build-depends:
    , geniusyield-orderbot-framework
    , geniusyield-orderbot-framework:common
    , geniusyield-orderbot:datasource-providers
    , geniusyield-orderbot:orderbook-tree
  mixins:
    , geniusyield-orderbot:orderbook-tree requires
        (GeniusYield.OrderBot.DataSource as GeniusYield.OrderBot.DataSource.Providers)
    , geniusyield-orderbot-framework requires
        ( GeniusYield.OrderBot.DataSource as GeniusYield.OrderBot.DataSource.Providers
        , GeniusYield.OrderBot.OrderBook as GeniusYield.OrderBot.OrderBook.Tree
        )
  ghc-options:
    -O2 -threaded -rtsopts -with-rtsopts=-N
```

# Customizing the bot

Now that you know how to set up the project itself and link components. Of course the next step is to write a matching strategy that beats out everyone else's! A very simple sample strategy is provided in the example `Main` file. Of course, it uses the list based orderbook and therefore it uses list functions in the strategy. You can use your custom functions in your strategy once you import the respective Orderbook implementation.

What else is there to customize? Quite a lot actually! Here's what the `OrderBot` type looks like:

```hs
-- See: GeniusYield.OrderBot

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
```

Once you have this data type constructed, you simply pass it to the function `GeniusYield.OrderBot.runOrderBot`. Let's see what the example strategy uses:

```hs
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
```

We will discuss the `skey` and `collateral` [below](#logistics-and-orchestration), for now: notice that you are able to set the following:

- The execution strategy itself, which contains the respective matching strategy.

  Currently, there is only one matching strategy: `MultiAssetTraverse`. What does this mean? In the real world, the DEX orders
  will consist of many many unique asset pairings; which means there will be an `OrderBook` for each asset pair. The `MultiAssetTraverse`
  is meant to independently traverse over each of these `OrderBook`s and yield matches.

  For the hackathon, there will only be one asset pair. The identifier of which, will be provided to you by the organizers. So in that sense, `MultiAssetTraverse` is the only execution strategy that makes sense and of course it will only have one orderbook within it.

  The execution strategy then takes a function as an argument. _This_ is your matching strategy. You will have access to the orderbook and its respective unique `OrderAssetPair` within this strategy, and you are expected to return information on matches in form of `MatchExecutionInfo`.

  There is extensive documentation on `OrderAssetPair` and relevant types in `GeniusYield.OrderBot.Types` - you should read this to understand how to work with them in your matching strategy.

  Documentation on `MatchExecutionInfo` can be found in `GeniusYield.OrderBot.MatchingStrategy`.

- The rescan delay. This is the amount of micro seconds to wait after a transaction submission before the blockchain is scanned again for changes in the DEX.

  Remember that changes in the blockchain take time! You cannot submit transactions back to back. It is the participants' responsibility to figure out the optimal waiting time!

  Make it too high, and your bot will slow down compared to others. Make it too low and you'll be wasting precious computation resources on transactions that ultimately fail!

- An asset filtering function. Only orders with assets for which this function returns true will be fully fetched. It is highly recommended that Participants utilize this to increase throughput. The final competition will use a constant `PolicyId`, which will be given to you by the organizers - you may filter for only the asset classes with that policy id.

  Of course, the example simply fetches everything.

- The exception handler. This handles exceptions raised during transaction build and submission. There _will_ be various exceptions during such a stateful operation. It might range from user errors like improper matching, to contention errors on the chain due to other bots trying to match the same orders! It's the participants' responsibility to potentially scrutinize and handle these exceptions.

  The handler will also have access to the list of `MatchExecutionInfo` returned by your strategy that caused the transaction to fail.

  What about exceptions raised during other operations in `runOrderBot`, other than transaction build/submission? These are not wrapped
  by your provided exception handler. The assumption is that the other components _should not_ fail, ideally. However, if you do see them
  failing, wrap the `runOrderBot` call with a `try`, or `catch`, or similar where you can have another exception handler. However, do note that you'll of course have to call `runOrderBot` again to restart the bot in this handler, since it crashed!

# Logistics and Orchestration

The `Main` executable takes a path to a config file containing some information to run the bots. This is meant to be passed as a command line argument.

There are two sample configs provided: `config-maestro.json` and `config-blockfrost.json`. There are also two `make` commands to run the bot with them, respectively: `make orderbot-maestro` and `make orderbot-blockfrost`

The configuration file describes the blockchain information provider to use. The supported providers can be seen in: `GeniusYield.OrderBot.Config.GYCoreProviderInfo`. We expose two easy to use providers in this repository:

- The Maestro blockchain API, provided by Maestro.
- Blockfrost

Both of these need an API token.

The Maestro API token is simply a number; `config-maestro.json` uses `42` as an example - but it is not the correct API token! Please replace it with the proper API token, which will be provided to you by the Hackathon organizers.

You can obtain a Blockfrost API key by creating a Blockfrost account and then a project with the `Preprod` testnet. Paste said key inside `config-blockfrost.json`.

Finally, you need to create a signing key that the bot will use, as well as a collateral UTxO within the bot's wallet. Save the signing key to a file.

The example main file sets up the scaffolding for using the `skey` and the `collateral`:

```hs
skey <- readPaymentSigningKey "bot.skey"
let collateral = "ca54517fd33bb9abafc1c48eb7e3ee6a7681b5ef38a4eaa8e56ab717b7705622#1"
```

Replace `bot.skey` with the path to your bot's signing key.

Replace `collateral` with a UTxO reference of the collateral in the bot's wallet which it will use to interact with smart contracts. Notice that the `collateral` can be specified using a string that contains the Transaction Id hash and the UTxO index, joined by a `#`.

For instructions on creating a signing key and collateral, refer to: [Setup wallet instructions](setup-wallet.md)

You must provide the wallet address of the bot to be eligible for the final competition!

# Matching strategy tips

- Remember that you should only match orders where the overall arbitrage profit is _at least_ a whole number. Payment to a DEX order is always ceiled, so if the calculated payment is `2.13` tokens, the order actually gets paid `3` tokens. So if you expected arbitrage profits in change, it's not going to happen!

- Don't return too many 'MatchExecutionInfo's! There is a limit to how many transactions can be matched in a single transaction. The exact limit, will have to be found out by the Participants.

  When you try to match too many orders in one transaction, you'll see `ExUnitsTooBigUTxO` within the raised exception.

# Common Transaction Exceptions

- `ExUnitsTooBigUTxO` in the exception means you are trying to match too many orders at once, making the transaction size cross the limit.
- `BalancingErrorInsufficientFunds` in the exception indicates there are not enough of one or many tokens to construct the tranasction. If you see ada in the value that is printed afterwards, it means your bot is out of ada. More often however, this error will be raised if your matching strategy does not return proper order matches and there aren't enough tokens in the transaction bucket to pay an order.
- `GYTxMonadException "partiallyFillPartialOrder: amount x must be smaller than offered amount x` - You are trying to partially fill an order, but the partial fill amount is the max volume of the order. Use `CompleteFill` instead. See `GeniusYield.OrderBot.MatchingStrategy.FillType` for more information.
- `BadInputsUTxO` in the exception that is raised during tx submission, _not creation/balancing_, usually indicates contention. An order you are trying to match is being matched by another transaction.

# Troubleshooting

- HLS will not work in signature modules, nor will it work in modules importing a signature module. Importing implementation modules is fine though!

- The order of type variables in a `forall` must match exactly in the signature and implementation. Remember that the order of type variables in implicit foralls is well defined - that is, it is simply in the order they appear from left to right, in the type signature.

- If you see an error like this in within the nix shell:

  ```
  ghc: panic! (the 'impossible' happened)
  (GHC version 8.10.7:
    getPackageDetails: couldn't find package
  blockfrost-api-0.6.0.0-<hash>
  ```

  Exit and re-enter the nix shell: `exit`, followed by `nix develop`.
