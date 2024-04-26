#!/bin/bash
network=1 # Use 0 for testnet, 1 for mainnet
pkh=49cb549adadda936fc3e11318e1acec9d960bb3eefcaa8f4504398a8

bech32_pub_key=$(echo $pkh | xxd -r -p | bech32 ed25519_pk)
address=$(echo $bech32_pub_key | cardano-address address payment --network-tag $network | cardano-address address build --network-tag $network)

echo "Generated Address: $address"



echo  $pkh | xxd -r -p > payment_keyhash.bin