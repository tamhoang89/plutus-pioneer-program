#!/bin/bash

assets=code/Week03/assets
keypath=keys
contract="$1"
name="$2"
txin="$3"
amount="$4"
datum_file="$5"
body="$assets/trans/send-to-"$contract"-datum-"$datum_file".txbody"
tx="$assets/trans/send-to-"$contract"-datum-"$datum_file".tx"

# Build gift address 
cardano-cli address build \
    --payment-script-file "$assets/$contract.plutus" \
    --testnet-magic 2 \
    --out-file "$assets/$contract.addr"

# Build the transaction
cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 2 \
    --tx-in "$txin" \
    --tx-out "$(cat "$assets/$contract.addr") + $amount lovelace" \
    --tx-out-inline-datum-file "$assets/json-data/$datum_file" \
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