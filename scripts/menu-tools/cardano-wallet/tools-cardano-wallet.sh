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
    echo "$mnemonic"
    echo ""
    echo "mnemonic json array:"
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
    fi
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
}

# Function to select a Cardano Wallet container and navigate between options
cardano_wallet_tools() {
    while select_wallet_container; do
        while true; do
            
            echo "----"
            echo "MENU - Selected container: $selected_container"
            echo "----"
            echo "1) Generate mnemonic"
            echo "2) Generate mnemonic and create wallet"
            echo "3) List wallets"
            echo "4) Addresses Tools"
            echo "5) Fetch network information"
            echo "6) Docker Logs"
            echo "7) Delete this Container and Optionally Its Volumes"
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
                3) list_wallets "$selected_container"
                    read -p "Press Enter to continue..."
                ;;
                4) addresses_tools "$selected_container"
                ;;
                5) fetch_network_information "$selected_container"
                    read -p "Press Enter to continue..."
                ;;
                6)
                    monitor_logs "$selected_container"
                    read -p "Press Enter to continue..."
                ;;
                7)
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