#!/bin/sh

# Exit on any error
set -e

# --since 16588737.4e9bbbb67e3ae262133d94c3da5bffce7b1127fc436e7433b87668dba34c354a \
# --match "*/stake_vkh14hkp0sncfkt76spuqydtww4r900hfmsseezztzlh9sjkkjx6d7q" \
# --since origin \

#preview address
#--match "addr_test1qz4ll7yrah8h5t3cv2qptn4mw22judsm9j9zychhmtuuzmszd3hm6w02uxx6h0s3qgd4hxgpvd0qzklnmahcx7v0mcysptyj8l" \
#--since 11092156.fcae04fa9a8e44f9e75a15c610e8f249d428a48b88740ca0dd654042a7b7a29b \

#preprod address
#--match "addr_test1vrn4cmmgj0qypkpnys0c2w8xek2j4ncagg7s6rmrxv6d2wssdnudf" \
#--since 56971002.b5ccc6af85b85195f5c12030fdcdbb2d98f1f854cffca0d67b4e0b7dddc109cd \


echo "Starting Kupo on: $CARDANO_NETWORK"

echo "--host 0.0.0.0 --port 1442"
echo "--node-socket /ipc/node.socket"
echo "--node-config /configs/cardano-node/${CARDANO_NETWORK}/config.json"
echo "--defer-db-indexes"
echo "--match ${KUPO_MATCH_PATTERN}"
echo "--since ${KUPO_SINCE}"
echo "--workdir /var/lib/kupo-db"

sleep 5

kupo  --host 0.0.0.0 --port 1442 \
  --node-socket /ipc/node.socket \
  --node-config /configs/cardano-node/${CARDANO_NETWORK}/config.json \
  --defer-db-indexes \
  --match ${KUPO_MATCH_PATTERN} \
  --since ${KUPO_SINCE} \
  --workdir /var/lib/kupo-db


# kupo_status=$?

# if [ $kupo_status -ne 0 ]; then
#   echo "Failed to start Kupo: $kupo_status"
#   exit $kupo_status
# fi

