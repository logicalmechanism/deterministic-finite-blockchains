#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
script_path="../dfb-contract/dfb_contract.plutus"

script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic 1097911063)
issuer_address=$(cat wallets/seller-wallet/payment.addr)

# minimum ada to get in
SC_MIN_VALUE=$(${cli} transaction calculate-min-required-utxo \
    --protocol-params-file tmp/protocol.json \
    --tx-out-datum-embed-file data/empty_datum.json \
    --tx-out="$script_address 0" | tr -dc '0-9')
SC_UTXO_VALUE=$(( $SC_MIN_VALUE + 5000000 ))
sc_address_out="$script_address + $SC_UTXO_VALUE"
echo "Script OUTPUT: "${sc_address_out}

# exit

echo -e "\033[0;36m Getting Buyer UTxO Information \033[0m"
# get utxo
${cli} query utxo \
    --testnet-magic 1097911063 \
    --address ${issuer_address} \
    --out-file tmp/issuer_utxo.json

# transaction variables
TXNS=$(jq length tmp/issuer_utxo.json)
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/issuer_utxo.json)
issuer_tx_in=${TXIN::-8}


echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --alonzo-era \
    --protocol-params-file tmp/protocol.json \
    --invalid-hereafter 99999999 \
    --out-file tmp/tx.draft \
    --change-address ${issuer_address} \
    --tx-in ${issuer_tx_in} \
    --tx-out="${sc_address_out}" \
    --tx-out-datum-embed-file data/create_datum.json \
    --metadata-json-file data/one_player_metadata.json \
    --testnet-magic 1097911063)

IFS=':' read -ra VALUE <<< "$FEE"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee \033[0m" ${FEE}
#
# exit
#
echo -e "\033[0;36m Signing Tx \033[0m"
${cli} transaction sign \
    --signing-key-file wallets/seller-wallet/payment.skey \
    --tx-body-file tmp/tx.draft \
    --out-file tmp/tx.signed \
    --testnet-magic 1097911063
#
# exit
#
echo -e "\033[0;36m Submitting Tx \033[0m"
${cli} transaction submit \
    --testnet-magic 1097911063 \
    --tx-file tmp/tx.signed