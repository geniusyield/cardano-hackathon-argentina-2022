#!/usr/bin/env bash
cardano-cli address key-gen \
    --verification-key-file bot.vkey \
    --signing-key-file bot.skey \

cardano-cli address build \
    --payment-verification-key-file bot.vkey \
    --testnet-magic 1 \
    --out-file bot.preprod.addr
