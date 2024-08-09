#!/bin/sh

# Exit on any error
set -e

echo "Starting Ogmios on: $CARDANO_NETWORK"

echo "--host 0.0.0.0"
echo "--node-config /configs/cardano-node/${CARDANO_NETWORK}/config.json"
echo "--node-socket /ipc/node.socket"

ogmios \
  --host 0.0.0.0 \
  --node-config /configs/cardano-node/${CARDANO_NETWORK}/config.json \
  --node-socket /ipc/node.socket &

ogmios_status=$?

if [ $ogmios_status -ne 0 ]; then
  echo "Failed to start Ogmios: $ogmios_status"
  exit $ogmios_status
fi

echo "Ogmios has been started."

tail -f /dev/null