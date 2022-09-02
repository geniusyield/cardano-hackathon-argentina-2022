# Setting up a bot wallet

Create a signing key for the bot:

```
cardano-cli address key-gen \
    --verification-key-file bot.vkey \
    --signing-key-file bot.skey \

cardano-cli address build \
    --payment-verification-key-file bot.vkey \
    --testnet-magic 1 \
    --out-file bot.preprod.addr
```

This will create the files `bot.skey`, `bot.vkey`, and `bot.preprod.addr`: the private signing key, the verification key, and the wallet address on the preprod testnet.

Once you have the address, use the [faucet](https://docs.cardano.org/cardano-testnet/tools/faucet/) to fund it with some tAda. **Make sure you choose the _Preprod_ testnet**.

You will also have to share the wallet address with the Hackathon organizers.

Once the bot wallet is funded (search for the wallet address in https://testnet.cardanoscan.io/), you need to create a collateral. There is a Haskell executable provided that can do this: `geniusyield-collateral-exe`. You will need to run it with a path to the bot config and a path to your newly created `.skey` file. Example using the Maestro provider:

```sh
> cabal run geniusyield-collateral -- config-maestro.json bot.skey
```

It should print out the collateral UTxO reference, which you may simply paste in your Orderbot main file.

