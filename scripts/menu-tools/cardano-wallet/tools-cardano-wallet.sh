#!/bin/bash

source "$(dirname "${BASH_SOURCE[0]}")/../../utils/utils.sh"

setWorkspaceDir

source "$WORKSPACE_ROOT_DIR_ABSOLUTE/scripts/menu-tools/cardano-wallet/tools-addresses.sh"

# Function to select a Cardano Wallet container
select_wallet_container() {
    if ! select_container 'cardano-wallet-container'; then
        return 1  # Propagate the selection failure
    fi
}

# Function to generate mnemonic
generate_mnemonic() {
    local container=$1
    # Generate the mnemonic without echoing any extra information
    docker exec -it "$container" cardano-wallet recovery-phrase generate --size 15 | tr -d '\r' | tr -d '\n'
}

generate_mnemonic_menu() {
    local container=$1
    echo "Generating a 15-word mnemonic..."
    # Call generate_mnemonic and capture its output
    mnemonic=$(generate_mnemonic "$container")
    echo "Mnemonic generated. Please keep it safe."
    echo "$mnemonic"
    echo ""
}

# Function to create a wallet
generate_and_create_wallet() {
    local container=$1
    CARDANO_WALLET_PORT=$(docker exec -it "$container" printenv CARDANO_WALLET_PORT | tr -d '\r')
    
    echo "Container: ${container}"
    echo "CARDANO_WALLET_PORT: ${CARDANO_WALLET_PORT}"
    
    echo "Generating a 15-word mnemonic..."
    # Call generate_mnemonic and capture its output
    mnemonic=$(generate_mnemonic "$container")
    # Convert the mnemonic to a JSON array format
    mnemonic_json_array=$(echo "$mnemonic" | gawk '{for(i=1;i<=NF;i++) printf "\"%s\"%s", $i, (i<NF?", ":"")}')
    echo "Mnemonic generated. Please keep it safe."
    # echo "$mnemonic"
    # echo ""
    # echo "mnemonic json array:"
    echo "$mnemonic_json_array"
    echo ""
    
    read -p "Enter Wallet Name: " wallet_name
    read -sp "Enter Wallet Passphrase: " wallet_passphrase
    echo ""
    
    if [ -f /.dockerenv ]; then
        # echo "Running inside a Docker container."
        BASE_URL="http://host.docker.internal:${CARDANO_WALLET_PORT}"
    else
        # echo "Running on the host."
        BASE_URL="http://localhost:${CARDANO_WALLET_PORT}"
    fi

    echo ${BASE_URL}
    
    response=$(curl -s -f -X POST ${BASE_URL}/v2/wallets \
        -H "Content-Type: application/json" \
        -d "{
        \"name\": \"$wallet_name\",
        \"mnemonic_sentence\":[$mnemonic_json_array],
        \"passphrase\": \"$wallet_passphrase\"
    }" 2>&1)
    
    status=$?
    
    if [ $status -ne 0 ]; then
        echo "Error creating wallet: $response"
        echo "{\"status\": \"error\", \"message\": \"$response\"}"
    else
        echo "Wallet creation response: $response"
        echo "{\"status\": \"success\", \"wallet_name\": \"$wallet_name\", \"mnemonic_json_array\": \"$mnemonic_json_array\", \"wallet_passphrase\": \"$wallet_passphrase\"}"
    fi
}

# Function to create and save a wallet
generate_create_and_save_wallet() {
    local container=$1
    CARDANO_WALLET_PORT=$(docker exec -it "$container" printenv CARDANO_WALLET_PORT | tr -d '\r')
    
    echo "Container: ${container}"
    echo "CARDANO_WALLET_PORT: ${CARDANO_WALLET_PORT}"
    
    echo "Generating a 15-word mnemonic..."
    # Call generate_mnemonic and capture its output
    mnemonic=$(generate_mnemonic "$container")
    # Convert the mnemonic to a JSON array format
    mnemonic_json_array=$(echo "$mnemonic" | gawk '{for(i=1;i<=NF;i++) printf "\"%s\"%s", $i, (i<NF?", ":"")}')
    echo "Mnemonic generated. Please keep it safe."
    # echo "$mnemonic"
    # echo ""
    # echo "mnemonic json array:"
    echo "$mnemonic_json_array"
    echo ""
    
    read -p "Enter Wallet Name: " wallet_name
    read -sp "Enter Wallet Passphrase: " wallet_passphrase
    echo ""
    
    if [ -f /.dockerenv ]; then
        # echo "Running inside a Docker container."
        BASE_URL="http://host.docker.internal:${CARDANO_WALLET_PORT}"
    else
        # echo "Running on the host."
        BASE_URL="http://localhost:${CARDANO_WALLET_PORT}"
    fi

    response=$(curl -s -f -X POST ${BASE_URL}/v2/wallets \
        -H "Content-Type: application/json" \
        -d "{
        \"name\": \"$wallet_name\",
        \"mnemonic_sentence\":[$mnemonic_json_array],
        \"passphrase\": \"$wallet_passphrase\"
    }" 2>&1)
    
    status=$?
    
    if [ $status -ne 0 ]; then
        echo "Error creating wallet: $response"
    else
        echo "Wallet creation response: $response"
        file="$WORKSPACE_ROOT_DIR_ABSOLUTE/.priv/wallets/${wallet_name}/${wallet_name}.json"
        mkdir -p "$(dirname "$file")"

        # Ensure the EOM is not indented
        cat > "$file" <<EOM
{
    "name": "$wallet_name",
    "mnemonic_sentence": [$mnemonic_json_array],
    "passphrase": "$wallet_passphrase"
}
EOM
        echo "Saved Wallet file to $file"

        # Call the function to process the saved wallet file
        process_saved_wallet_file "$container" "$file"
    fi

}

# Function to process the saved wallet file
process_saved_wallet_file() {

    local container=$1

    local filePath=$2

    echo "Processing saved wallet file: $filePath"

    mnemonicPhrase=$(cat "$filePath" | jq -r '(.mnemonic_sentence | join(" "))' )
    echo ""
    echo "Mnemonic: $mnemonicPhrase"

    docker exec -i "$container" cardano-wallet key from-recovery-phrase Shelley <<< "$mnemonicPhrase" > "${filePath%.json}.root.prv"

    cat "${filePath%.json}.root.prv" | docker exec -i "$container" cardano-wallet key walletid > "${filePath%.json}.id"

    walletId=$(cat "${filePath%.json}.id")
    echo ""
    echo "Wallet Id: $walletId"

    cat "${filePath%.json}.root.prv" \
      | docker exec -i "$container" cardano-wallet key child 1852H/1815H/0H/0/0 > "${filePath%.json}.prv"

    cat "${filePath%.json}.prv" | docker exec -i "$container" cardano-wallet key public --with-chain-code > "${filePath%.json}.pub"

    echo ""
    echo "File: ${filePath%.json}.root.prv:"
    # cat "${filePath%.json}.root.prv"
    echo "Inspect:"
    docker exec -i "$container" cardano-wallet key inspect <<< "$(cat ${filePath%.json}.root.prv)"
    
    echo ""
    echo "File: ${filePath%.json}.prv:"
    # cat "${filePath%.json}.prv"
    echo "Inspect:"
    docker exec -i "$container" cardano-wallet key inspect <<< "$(cat ${filePath%.json}.prv)"
    
    echo ""
    echo "File: ${filePath%.json}.pub:"
    # cat "${filePath%.json}.pub"
    echo "Inspect:"
    docker exec -i "$container" cardano-wallet key inspect <<< "$(cat ${filePath%.json}.pub)"

    echo ""
    echo "Generating Wallet Private Key and Signing Key: "
    docker cp -q "${filePath%.json}.root.prv" "$container:/tmp/root.prv"
    docker exec -i "$container" cardano-cli key convert-cardano-address-key --signing-key-file /tmp/root.prv --shelley-payment-key --out-file /tmp/root.skey
    docker cp -q "$container:/tmp/root.skey" "${filePath%.json}.root.skey"
    echo "File: ${filePath%.json}.root.skey"
    # cat "${filePath%.json}.root.skey"

    docker cp -q "${filePath%.json}.prv" "$container:/tmp/prv"
    docker exec -i "$container" cardano-cli key convert-cardano-address-key --signing-key-file /tmp/prv --shelley-payment-key --out-file /tmp/skey
    docker cp -q "$container:/tmp/skey" "${filePath%.json}.skey"
    echo "File: ${filePath%.json}.skey"
    # cat "${filePath%.json}.skey"

    echo ""
    echo "Generating Wallet Public Key and Payment Verification Key:"
    docker cp -q "${filePath%.json}.skey" "$container:/tmp/skey"
    docker exec -i "$container" cardano-cli key verification-key --signing-key-file /tmp/skey --verification-key-file /tmp/vkey
    docker cp -q "$container:/tmp/vkey" "${filePath%.json}.vkey"
    echo "File: ${filePath%.json}.vkey"
    # cat "${filePath%.json}.vkey"

    echo ""
    echo "Generating Wallet Public Key Hash - Payment Verification Key Hash:"
    cat "${filePath%.json}.pub" | docker exec -i "$container" cardano-wallet key hash --hex > "${filePath%.json}.pkh"
    walletPubHashHex=$(cat "${filePath%.json}.pkh")
    echo "File: ${filePath%.json}.pkh"
    # cat "${filePath%.json}.pkh"


    # With cardano cli from reading the content of${filePath%.json}.pub"
    # verificationKey=$(cat "${filePath%.json}.pub")
    # docker exec -i "$container" cardano-cli address key-hash --payment-verification-key "$verificationKey"
    # 
    # With cardano cli from ${filePath%.json}.vkey file
    # docker exec -i "$container" cardano-cli address key-hash --payment-verification-key-file "${filePath%.json}.vkey"
    # 
    # add to cardano wallet a file to server
    # docker exec -i "$container" curl -H "content-type: application/json" -XPOST -d @"$filePath" localhost:$CARDANO_WALLET_PORT/v2/wallets
    
    echo ""
    echo "Generating Wallet Addresses:"
    CARDANO_WALLET_PORT=$(docker exec -it "$container" printenv CARDANO_WALLET_PORT | tr -d '\r')
    if [ -f /.dockerenv ]; then
        # echo "Running inside a Docker container."
        BASE_URL="http://host.docker.internal:${CARDANO_WALLET_PORT}"
    else
        # echo "Running on the host."
        BASE_URL="http://localhost:${CARDANO_WALLET_PORT}"
    fi

    addresses=$(curl -s -H "content-type: application/json" -XGET "${BASE_URL}/v2/wallets/$walletId/addresses" | jq -r '.[]')
    echo $addresses | jq -r '.id' >${filePath%.json}.addrs
    address=$(echo $addresses | jq -r '.id'| sed -n 1p)
    echo $address>${filePath%.json}.addr
    echo "File: ${filePath%.json}.addrs"
    echo "File: ${filePath%.json}.addr"

}

# Function to list all created wallets
list_wallets() {
    local container=$1
    echo "Listing all wallets..."
    CARDANO_WALLET_PORT=$(docker exec -it "$container" printenv CARDANO_WALLET_PORT | tr -d '\r')
    # echo "Container: ${container}"
    # echo "CARDANO_WALLET_PORT: ${CARDANO_WALLET_PORT}"
    if [ -f /.dockerenv ]; then
        # echo "Running inside a Docker container."
        BASE_URL="http://host.docker.internal:${CARDANO_WALLET_PORT}"
    else
        # echo "Running on the host."
        BASE_URL="http://localhost:${CARDANO_WALLET_PORT}"
    fi
    response=$(curl -s "${BASE_URL}/v2/wallets")
    echo "Wallets: $response"

    echo "List of wallet IDs and names:"
    echo "$response" | jq -r '.[] | "ID: \(.id), Name: \(.name)"'
}

# Function to list UTXOs for a given wallet
list_utxos() {
    local container=$1
    echo "Enter Wallet ID:"
    read walletId

    CARDANO_WALLET_PORT=$(docker exec -it "$container" printenv CARDANO_WALLET_PORT | tr -d '\r')
    if [ -f /.dockerenv ]; then
        # echo "Running inside a Docker container."
        BASE_URL="http://host.docker.internal:${CARDANO_WALLET_PORT}"
    else
        # echo "Running on the host."
        BASE_URL="http://localhost:${CARDANO_WALLET_PORT}"
    fi

    CARDANO_NETWORK=$(docker exec -it "$container" printenv CARDANO_NETWORK | tr -d '\r')
    determine_network_with_magic "$CARDANO_NETWORK" "$WORKSPACE_ROOT_DIR_ABSOLUTE/configs"

    addresses=$(curl -s -H "content-type: application/json" -XGET "${BASE_URL}/v2/wallets/$walletId/addresses" | jq -r '.[]')

    echo "Do you want to see all addresses? (y/n)"
    read -n 1 -s option
    echo
    utxos=""
    
    if [[ $option = "y" ]]; then 
        # echo "Wallet's addresses:"
        # echo $addresses | jq -r '.id' | nl 

        for address in $(echo $addresses | jq -r '.id'); do
            echo "Listing UTXOs for address: $address"
            address_utxos=$(docker exec -i "$container" cardano-cli query utxo --socket-path /ipc/node.socket --address "$address" --$CARDANO_NETWORK_WITH_MAGIC)
            utxos+="$address_utxos"$'\n'
        done
    else
        address=$(echo $addresses | jq -r '.id'| sed -n 1p)
        echo "Listing UTXOs for address: $address"
        utxos=$(docker exec -i "$container" cardano-cli query utxo --socket-path /ipc/node.socket --address "$address" --$CARDANO_NETWORK_WITH_MAGIC)
    fi

    echo "UTXOs:"
    echo "$utxos"
}

# Function to fetch network information
fetch_network_information() {
    local container=$1
    echo "Fetching network information..."
    CARDANO_WALLET_PORT=$(docker exec -it "$container" printenv CARDANO_WALLET_PORT | tr -d '\r')
    if [ -f /.dockerenv ]; then
        # echo "Running inside a Docker container."
        BASE_URL="http://host.docker.internal:${CARDANO_WALLET_PORT}"
    else
        # echo "Running on the host."
        BASE_URL="http://localhost:${CARDANO_WALLET_PORT}"
    fi
    response=$(curl -s "${BASE_URL}/v2/network/information")
    echo "Network Information: $response"

    CARDANO_NETWORK=$(docker exec -it "$container" printenv CARDANO_NETWORK | tr -d '\r')
    determine_network_with_magic "$CARDANO_NETWORK" "$WORKSPACE_ROOT_DIR_ABSOLUTE/configs"
}

# Function to select a Cardano Wallet container and navigate between options
cardano_wallet_tools() {
    while select_wallet_container; do
        while true; do
            
            echo "----"
            echo "MENU - Selected container: $selected_container"
            echo "----"
            echo "1) Generate mnemonic"
            echo "2) Create wallet"
            echo "3) Export Wallet to File"
            echo "4) List wallets ids and names"
            echo "5) List wallets utxos"
            echo "6) Addresses Tools"
            echo "7) Fetch network information"
            echo "8) Docker Logs"
            echo "9) Delete this Container and Optionally Its Volumes"
            echo "0) Return Main Menu"
            read -p "Enter your choice or 0 to exit: " tool_choice
            echo "----"
            
            case $tool_choice in
                1) generate_mnemonic_menu "$selected_container"
                    read -p "Press Enter to continue..."
                ;;
                2) generate_and_create_wallet "$selected_container"
                    read -p "Press Enter to continue..."
                ;;
                3) generate_create_and_save_wallet "$selected_container"
                    read -p "Press Enter to continue..."
                ;;
                4) list_wallets "$selected_container"
                    read -p "Press Enter to continue..."
                ;;
                5) list_utxos "$selected_container"
                    read -p "Press Enter to continue..."
                ;;
                6) addresses_tools "$selected_container"
                ;;
                7) fetch_network_information "$selected_container"
                    read -p "Press Enter to continue..."
                ;;
                8)
                    monitor_logs "$selected_container"
                    read -p "Press Enter to continue..."
                ;;
                9)
                    delete_wallet_container_and_associated_icarus "$selected_container"
                    read -p "Press Enter to continue..."
                    break 2 # Breaks out of the current loop and the container selection loop
                ;;
                
                0) break 2 # Breaks out of both the inner loop and the container selection loop
                ;;
                
                *)
                    echo "Invalid choice, please select a valid option."
                    read -p "Press Enter to continue..."
                    
                ;;
            esac
        done
    done
}