#!/bin/bash

assets=code/Week02/assets
keypath=keys
contract_name="$1"
name="$2"
collateral="$3"
txin="$4"
redeemer_file="$5"

pp="$assets/protocol-parameters.json"
body="$assets/trans/collect-from-"$contract_name"-redeemer-"$redeemer_file".txbody"
tx="$assets/trans/collect-from-"$contract_name"-redeemer-"$redeemer_file".tx"

# Query the protocol parameters \

cardano-cli query protocol-parameters \
    --testnet-magic 2 \
    --out-file "$pp"

# Build the transaction
cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 2 \
    --tx-in "$txin" \
    --tx-in-script-file "$assets/$contract_name.plutus" \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file "$assets/json-data/$redeemer_file" \
    --tx-in-collateral "$collateral" \
    --change-address "$(cat "$keypath/$name.addr")" \
    --protocol-params-file "$pp" \
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