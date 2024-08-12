#!/bin/bash

# Exit on any error
set -e

echo "Starting Cardano Wallet on: $CARDANO_NETWORK"

# Determine the network argument for cardano-wallet
if [[ "$CARDANO_NETWORK" == "mainnet" ]]; then
    CARDANO_NETWORK_ARG="--mainnet"
else
    CARDANO_NETWORK_ARG="--testnet /configs/cardano-node/${CARDANO_NETWORK}/byron-genesis.json"
fi

echo "--node-socket /ipc/node.socket"
echo "--database /var/lib/cardano-wallet-db"
echo "--listen-address 0.0.0.0"
echo "Network argument: $CARDANO_NETWORK_ARG"

# Start the cardano-wallet server with the appropriate network argument
cardano-wallet serve \
  --node-socket /ipc/node.socket \
  --database /var/lib/cardano-wallet-db \
  --listen-address 0.0.0.0 \
  $CARDANO_NETWORK_ARG

