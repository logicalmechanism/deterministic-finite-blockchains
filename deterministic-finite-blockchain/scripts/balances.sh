#!/usr/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
script_path="../dfb-contract/dfb_contract.plutus"

SCRIPT_ADDRESS=$(${cli} address build --payment-script-file ${script_path} --testnet-magic 1097911063)
BUYER_ADDRESS=$(cat wallets/buyer-wallet/payment.addr)
SELLER_ADDRESS=$(cat wallets/seller-wallet/payment.addr)

echo
echo "Script Address:" $SCRIPT_ADDRESS
${cli} query utxo --address ${SCRIPT_ADDRESS} --testnet-magic 1097911063

echo
echo "Player 1 Address:" $SELLER_ADDRESS
${cli} query utxo --address ${SELLER_ADDRESS} --testnet-magic 1097911063

echo
echo "Player 2 Address:" $BUYER_ADDRESS
${cli} query utxo --address ${BUYER_ADDRESS} --testnet-magic 1097911063
