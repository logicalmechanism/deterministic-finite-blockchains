#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
script_path="../dfb-contract/dfb_contract.plutus"

script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic 1097911063)
issuer_address=$(cat wallets/seller-wallet/payment.addr)

SC_UTXO_VALUE=$(${cli} transaction calculate-min-required-utxo \
    --protocol-params-file tmp/protocol.json \
    --tx-out-datum-embed-file data/empty_datum.json \
    --tx-out="$script_address 0" | tr -dc '0-9')
sc_address_out="$issuer_address + $SC_UTXO_VALUE"
echo "Script OUTPUT: "${sc_address_out}


# exit

echo -e "\033[0;36m Gathering UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic 1097911063 \
    --address ${issuer_address} \
    --out-file tmp/issuer_utxo.json

TXNS=$(jq length tmp/issuer_utxo.json)
if [ "$TXNS" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${issuer_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/issuer_utxo.json)
CTXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in-collateral"' tmp/issuer_utxo.json)
collateral_tx_in=${CTXIN::-19}
seller_tx_in=${TXIN::-8}

echo -e "\033[0;36m Gathering Script UTxO Information  \033[0m"
${cli} query utxo \
    --address ${script_address} \
    --testnet-magic 1097911063 \
    --out-file tmp/script_utxo.json
# transaction variables
TXNS=$(jq length tmp/script_utxo.json)
if [ "$TXNS" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${script_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/script_utxo.json)
script_tx_in=${TXIN::-8}

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --alonzo-era \
    --protocol-params-file tmp/protocol.json \
    --invalid-hereafter 99999999 \
    --out-file tmp/tx.draft \
    --change-address ${issuer_address} \
    --tx-in ${seller_tx_in} \
    --tx-in-collateral ${collateral_tx_in} \
    --tx-in ${script_tx_in}  \
    --tx-in-datum-file data/empty_datum.json \
    --tx-in-redeemer-file data/remove_redeemer.json \
    --tx-out="${sc_address_out}" \
    --required-signer wallets/seller-wallet/payment.skey \
    --tx-in-script-file ${script_path} \
    --testnet-magic 1097911063)

IFS=':' read -ra VALUE <<< "$FEE"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee: \033[0m" $FEE
#
# exit
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file wallets/seller-wallet/payment.skey \
    --tx-body-file tmp/tx.draft \
    --out-file tmp/tx.signed \
    --testnet-magic 1097911063
#
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic 1097911063 \
    --tx-file tmp/tx.signed