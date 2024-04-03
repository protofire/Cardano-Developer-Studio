#!/bin/bash

source "$(dirname "${BASH_SOURCE[0]}")/utils.sh"

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
    echo "Generating a 15-word mnemonic..."
    # Call generate_mnemonic and capture its output
    mnemonic=$(generate_mnemonic "$container")
    # Convert the mnemonic to a JSON array format
    mnemonic_json_array=$(echo "$mnemonic" | awk '{for(i=1;i<=NF;i++) printf "\"%s\"%s", $i, (i<NF?", ":"")}')
    echo "Mnemonic generated. Please keep it safe."
    echo "$mnemonic"
    echo ""
    
    read -p "Enter Wallet Name: " wallet_name
    read -sp "Enter Wallet Passphrase: " wallet_passphrase
    echo ""
    
    CARDANO_WALLET_PORT=$(docker exec -it "$container" printenv CARDANO_WALLET_PORT | tr -d '\r')
    
    response=$(curl -s -X POST http://localhost:${CARDANO_WALLET_PORT}/v2/wallets \
        -H "Content-Type: application/json" \
        -d "{
            \"name\": \"$wallet_name\",
            \"mnemonic_sentence\":[$mnemonic_json_array],
            \"passphrase\": \"$wallet_passphrase\"
    }")
    echo "Wallet creation response: $response"
}


# Function to list all created wallets
list_wallets() {
    local container=$1
    echo "Listing all wallets..."
    CARDANO_WALLET_PORT=$(docker exec -it "$container" printenv CARDANO_WALLET_PORT | tr -d '\r')
    response=$(curl -s "http://localhost:${CARDANO_WALLET_PORT}/v2/wallets")
    echo "Wallets: $response"
}

# Function to fetch network information
fetch_network_information() {
    local container=$1
    echo "Fetching network information..."
    CARDANO_WALLET_PORT=$(docker exec -it "$container" printenv CARDANO_WALLET_PORT | tr -d '\r')
    response=$(curl -s "http://localhost:${CARDANO_WALLET_PORT}/v2/network/information")
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
            echo "4) Fetch network information"
            echo "5) Delete this Container and Optionally Its Volumes"
            echo "6) Exit to container selection"
            read -p "Enter your choice or 6 to exit: " tool_choice
            echo "----"
            
            case $tool_choice in
                1) generate_mnemonic_menu "$selected_container" ;;
                2) generate_and_create_wallet "$selected_container" ;;
                3) list_wallets "$selected_container" ;;
                4) fetch_network_information "$selected_container" ;;
                5)
                    delete_wallet_container_and_associated_icarus "$selected_container"
                    break 2 # Breaks out of the current loop and the container selection loop
                ;;
                6) break ;;
                *)
                    echo "Invalid choice, please select a valid option."
                ;;
            esac
            read -p "Press Enter to continue..."
        done
    done
}