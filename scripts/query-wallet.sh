#!/bin/bash

wallet_folder=keys

if [ ! -z "$2" ]; then
    wallet_folder=code/Week0$2/assets
fi

cardano-cli query utxo --address "$(cat "$wallet_folder/$1.addr")" --testnet-magic 2