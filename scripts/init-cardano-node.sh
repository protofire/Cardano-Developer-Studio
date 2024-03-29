#!/bin/bash

# Exit on any error
set -e

# This script is intended to be run inside the Docker container.

echo "Starting node on: $CARDANO_NETWORK"

echo "--topology: /configs/cardano-node/${CARDANO_NETWORK}/topology.json"
echo "--database-path: /data"
echo "--socket-path: /ipc/node.socket"  # As set in the Docker Compose file
echo "--port: ${CARDANO_NODE_PORT}"  # Adjust as needed
echo "--config: /configs/cardano-node/${CARDANO_NETWORK}/config.json"


sleep 5

cardano-node run \
--topology "/configs/cardano-node/${CARDANO_NETWORK}/topology.json" \
--database-path "/data" \
--socket-path "/ipc/node.socket" \
--port $CARDANO_NODE_PORT \
--config "/configs/cardano-node/${CARDANO_NETWORK}/config.json"