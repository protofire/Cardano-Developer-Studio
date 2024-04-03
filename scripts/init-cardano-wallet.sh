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

echo "Network argument: $CARDANO_NETWORK_ARG"

# Ensure the wallet database directory exists
mkdir -p /var/lib/cardano-wallet-db
sudo chmod 755 "$SNAPSHOT_SAVE_PATH"
sudo chown $(whoami) "$SNAPSHOT_SAVE_PATH"

# Start the cardano-wallet server with the appropriate network argument
cardano-wallet serve \
  --node-socket /ipc/node.socket \
  --database /var/lib/cardano-wallet-db \
  --listen-address 0.0.0.0 \
  $CARDANO_NETWORK_ARG

echo "Cardano Wallet has been started."
