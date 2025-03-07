#!/bin/bash

assets=code/Week02/assets
keypath=keys
name="$1"
txin="$2"
body="$assets/gift.txbody"
tx="$assets/gift.tx"

# Build gift address 
cardano-cli address build \
    --payment-script-file "assets/fortytwo.plutus" \
    --testnet-magic 2 \
    --out-file "assets/fortytwo.addr"

# Build the transaction
cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 2 \
    --tx-in "$txin" \
    --tx-out "$(cat "$assets/gift.addr") + 3000999 lovelace" \
    --tx-out-inline-datum-file "$assets/json-data/unit.json" \
    --change-address "$(cat "$keypath/$name.addr")" \
    --out-file "$body"
    
# Sign the transaction
cardano-cli transaction sign \
    --tx-body-file "$body" \
    --signing-key-file "$keypath/$name.skey" \
    --testnet-magic 2 \
    --out-file "$tx"

# Submit the transaction
cardano-cli transaction submit \
    --testnet-magic 2 \
    --tx-file "$tx"

tid=$(cardano-cli transaction txid --tx-file "$tx")
echo "transaction id: $tid"
echo "Cardanoscan: https://preview.cardanoscan.io/transaction/$tid"