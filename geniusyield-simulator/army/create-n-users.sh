#!/usr/bin/zsh

magic=1

for i in {$1..$2}
do
    vkey="army/users/user$i.vkey" \
    skey="army/users/user$i.skey" \
    addr="army/users/user$i.addr" \

    cardano-cli address key-gen \
        --verification-key-file $vkey \
        --signing-key-file $skey \

    cardano-cli address build \
        --payment-verification-key-file $vkey \
        --testnet-magic $magic \
        --out-file $addr
done