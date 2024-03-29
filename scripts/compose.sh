#!/bin/bash

# Exit on any error
set -e

# If SCRIPT_DIR is not set, determine the directory where compose.sh resides
# This makes compose.sh more robust and standalone-capable
if [[ -z "${SCRIPT_DIR}" ]]; then
    SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
    export WORKSPACE_ROOT_DIR_ABSOLUTE="$SCRIPT_DIR/.."
    export CONFIG_DIR_ABSOLUTE="$WORKSPACE_ROOT_DIR_ABSOLUTE/configs"
fi

# Example function to save environment variables to a file
save_env_variables() {
    echo "CARDANO_NETWORK=${CARDANO_NETWORK}" > .env.cardano
    # Add other variables as needed
}

# Example function to load environment variables from a file
load_env_variables() {
    if [[ -f ".env.cardano" ]]; then
        source .env.cardano
        export CARDANO_NETWORK  # Make sure this is exported
        echo "CARDANO_NETWORK=${CARDANO_NETWORK}" 
    else
        echo "No saved environment variables found. Please set up the Cardano Node first."
        exit 1
    fi
}

# Check if the specified snapshot exists, and if not, offer alternatives
check_and_offer_snapshots() {
    if [ -f "$final_snapshot_path" ]; then
        echo "Snapshot file already exists. Skipping download."
        USE_SNAPSHOT_PATH="$final_snapshot_path"
    else
        echo "The latest snapshot for node version $CARDANO_NODE_VERSION not found at $final_snapshot_path."
        # List available snapshots
        available_snapshots=($(ls "$SNAPSHOT_SAVE_PATH" | grep .lz4))
        if [ ${#available_snapshots[@]} -eq 0 ]; then
            echo "No previously downloaded snapshots found."
            USE_SNAPSHOT_PATH=""
        else
            echo "Available snapshots:"
            for i in "${!available_snapshots[@]}"; do
                echo "$((i+1))) ${available_snapshots[$i]}"
            done
            read -p "Enter the number of the snapshot to use (or press Enter to download the latest): " snapshot_choice
            if [[ -n "$snapshot_choice" && $snapshot_choice =~ ^[0-9]+$ && $snapshot_choice -ge 1 && $snapshot_choice -le ${#available_snapshots[@]} ]]; then
                USE_SNAPSHOT_PATH="$SNAPSHOT_SAVE_PATH/${available_snapshots[$((snapshot_choice-1))]}"
                echo "Selected snapshot: $USE_SNAPSHOT_PATH"
            else
                echo "Invalid selection or no selection made. Proceeding to download the latest snapshot."
                USE_SNAPSHOT_PATH=""
            fi
        fi
    fi
}

# Function to prompt user to set environment variables
set_node_env_variables() {
    read -p "Enter CARDANO_NODE_VERSION [default: 8.9.0]: " CARDANO_NODE_VERSION
    CARDANO_NODE_VERSION=${CARDANO_NODE_VERSION:-"8.9.0"}
    export CARDANO_NODE_VERSION

    read -p "Enter CARDANO_NETWORK [default: preprod, options: sanchonet-preview-preprod-mainnet]: " CARDANO_NETWORK
    CARDANO_NETWORK=${CARDANO_NETWORK:-preprod}
    export CARDANO_NETWORK

    read -p "Enter CARDANO_NODE_PORT [default: 3001]: " CARDANO_NODE_PORT
    CARDANO_NODE_PORT=${CARDANO_NODE_PORT:-3001}
    export CARDANO_NODE_PORT

    read -p "Enter base CARDANO_NODE_DB_PATH absolute path [default: /var/lib/cardano/data]: " base_CARDANO_NODE_DB_PATH
    CARDANO_NODE_DB_PATH="${base_CARDANO_NODE_DB_PATH:-/var/lib/cardano/data}/$CARDANO_NETWORK"
    echo "Node database will be located at: $CARDANO_NODE_DB_PATH"
    export CARDANO_NODE_DB_PATH

    # Ensure the database directory exists.
    if [ ! -d "$CARDANO_NODE_DB_PATH" ]; then
        echo "Creating directory for CARDANO_NODE_DB_PATH at $CARDANO_NODE_DB_PATH"
        sudo mkdir -p "$CARDANO_NODE_DB_PATH"
    else
        echo "Directory for CARDANO_NODE_DB_PATH already exists."
    fi

    read -p "Download snapshot for faster setup? (yes/no) [default: yes]: " download_snapshot
    download_snapshot=${download_snapshot:-yes}

    if [[ "$download_snapshot" =~ ^[Yy][Ee][Ss]$ ]]; then
        read -p "Enter path for saving snapshot [default: $CONFIG_DIR_ABSOLUTE/cardano-node-snapshot/$CARDANO_NETWORK]: " SNAPSHOT_SAVE_PATH
        SNAPSHOT_SAVE_PATH=${SNAPSHOT_SAVE_PATH:-"$CONFIG_DIR_ABSOLUTE/cardano-node-snapshot/$CARDANO_NETWORK"}
        echo "Snapshots will be saved to: $SNAPSHOT_SAVE_PATH"
        export SNAPSHOT_SAVE_PATH

        echo "Checking for available snapshots for node version $CARDANO_NODE_VERSION..."
        url_network_segment="$CARDANO_NETWORK"
        if [[ "$CARDANO_NETWORK" == "preprod" ]]; then
            url_network_segment="testnet"
        fi

        snapshots_json=$(curl -s "https://downloads.csnapshots.io/$url_network_segment/${url_network_segment}-db-snapshot.json")
        snapshot_info=$(echo "$snapshots_json" | jq --arg NODE_VERSION "$CARDANO_NODE_VERSION" '.[] | select(.node_version == $NODE_VERSION)')

        if [[ -n "$snapshot_info" ]]; then
            snapshot_url=$(echo "$snapshot_info" | jq -r '.file_name')
            final_snapshot_path="$SNAPSHOT_SAVE_PATH/$(basename "$snapshot_url")"

            if [ -f "$final_snapshot_path" ]; then
                echo "Snapshot file already exists. Skipping download."
                USE_SNAPSHOT_PATH="$final_snapshot_path"
            else
                available_snapshots=($(ls "$SNAPSHOT_SAVE_PATH"/*.lz4 2>/dev/null))
                if [ ${#available_snapshots[@]} -gt 0 ]; then
                    echo "Available snapshots:"
                    for i in "${!available_snapshots[@]}"; do
                        echo "$((i+1))) ${available_snapshots[$i]}"
                    done
                    read -p "Select a snapshot to use (or press Enter to download the latest): " snapshot_choice
                    if [[ -n "$snapshot_choice" && "$snapshot_choice" =~ ^[0-9]+$ && "$snapshot_choice" -ge 1 && "$snapshot_choice" -le ${#available_snapshots[@]} ]]; then
                        USE_SNAPSHOT_PATH="${available_snapshots[$((snapshot_choice-1))]}"
                        echo "Using selected snapshot: $USE_SNAPSHOT_PATH"
                    fi
                fi
                if [[ -z "$USE_SNAPSHOT_PATH" ]]; then
                    echo "Downloading and using the latest snapshot..."
                    sudo mkdir -p "$SNAPSHOT_SAVE_PATH"
                    if curl -L "https://downloads.csnapshots.io/$url_network_segment/$snapshot_url" --progress-bar --output "$final_snapshot_path"; then
                        echo "Snapshot downloaded successfully to $final_snapshot_path."
                        USE_SNAPSHOT_PATH="$final_snapshot_path"
                    else
                        echo "Failed to download snapshot. Please check your internet connection or try again later."
                        exit 1
                    fi
                fi
            fi
            echo "Extracting snapshot from $USE_SNAPSHOT_PATH to $CARDANO_NODE_DB_PATH..."
            sudo rm -rf "$CARDANO_NODE_DB_PATH/*"
            sudo mkdir -p "$CARDANO_NODE_DB_PATH"
            if lz4 -c -d "$USE_SNAPSHOT_PATH" | sudo tar -x -C "$CARDANO_NODE_DB_PATH"; then
                sudo mv $CARDANO_NODE_DB_PATH/db/* $CARDANO_NODE_DB_PATH/
                echo "Snapshot extracted successfully."
            else
                echo "Failed to extract snapshot."
                exit 1
            fi
        else
            echo "No snapshot available for node version $CARDANO_NODE_VERSION."
        fi
    else
        echo "Snapshot download skipped."
    fi


    # Install jq if not already available
    if ! command -v jq &> /dev/null; then
        echo "Installing jq..."
        sudo apt-get install jq
    fi

    CARDANO_SHELLEY=$CONFIG_DIR_ABSOLUTE/cardano-node/${CARDANO_NETWORK}/shelley-genesis.json
    if [[ -f "$CARDANO_SHELLEY"  ]];
    then
        if [[ "$CARDANO_NETWORK" == "mainnet" ]]; then 
            CARDANO_NETWORK_WITH_MAGIC="mainnet"
            export CARDANO_NETWORK_WITH_MAGIC
        else
            NETWORKMAGIC_NRO=$(cat $CARDANO_SHELLEY | jq -r '.networkMagic')
            CARDANO_NETWORK_WITH_MAGIC="testnet-magic "$NETWORKMAGIC_NRO
            export CARDANO_NETWORK_WITH_MAGIC
        fi
        echo "Network with magic: $CARDANO_NETWORK_WITH_MAGIC"
     else
        echo "Shelley genesis file not found at $CARDANO_SHELLEY"
        exit 1
    fi
}

set_wallet_env_variables() {
    echo "Setting up Cardano Wallet environment..."
    
    read -p "Enter CARDANO_WALLET_TAG [default: latest]: " CARDANO_WALLET_TAG
    CARDANO_WALLET_TAG=${CARDANO_WALLET_TAG:-latest}
    export CARDANO_WALLET_TAG
    
    # Ensure CARDANO_NETWORK is set; if not, load from saved environment variables
    if [ -z "${CARDANO_NETWORK}" ]; then
        echo "CARDANO_NETWORK could not be determined. Please ensure the Cardano Node setup has been completed."
        exit 1
    fi
    
    # Use a network-specific subdirectory within the CARDANO_WALLET_DB_PATH
    read -p "Enter base CARDANO_WALLET_DB_PATH absolute path [default: /var/lib/cardano/wallet-db]: " CARDANO_WALLET_DB_PATH
    CARDANO_WALLET_DB_PATH="${CARDANO_WALLET_DB_PATH:-/var/lib/cardano/wallet-db}/${CARDANO_NETWORK}"
    echo "Using $CARDANO_WALLET_DB_PATH as the database path for the $CARDANO_NETWORK network."
    export CARDANO_WALLET_DB_PATH
    
    read -p "Enter CARDANO_WALLET_PORT [default: 8090]: " CARDANO_WALLET_PORT
    CARDANO_WALLET_PORT=${CARDANO_WALLET_PORT:-8090}
    export CARDANO_WALLET_PORT
    
    # Optionally, repeat or add other variables specific to the wallet setup here
}

set_dbsync_env_variables() {
    echo "Setting up Cardano DB Sync environment..."

    read -p "Enter POSTGRES_DB [default: dbsync]: " POSTGRES_DB
    POSTGRES_DB=${POSTGRES_DB:-dbsync}
    export POSTGRES_DB

    read -p "Enter POSTGRES_USER [default: postgres]: " POSTGRES_USER
    POSTGRES_USER=${POSTGRES_USER:-postgres}
    export POSTGRES_USER

    read -p "Enter POSTGRES_PASSWORD [default: '']: " POSTGRES_PASSWORD
    POSTGRES_PASSWORD=${POSTGRES_PASSWORD}
    export POSTGRES_PASSWORD

    read -p "Enter POSTGRES_PORT [default: 5432]: " POSTGRES_PORT
    POSTGRES_PORT=${POSTGRES_PORT:-5432}
    export POSTGRES_PORT

    # Optionally, you can prompt for other DB Sync-specific variables here
}

check_node_resources() {
    echo "Checking for Cardano Node resources..."
    
    # Check if the cardano-network is available
    if ! docker network ls | grep -q "cardano-network"; then
        echo "Cardano network not found. Please ensure the Cardano Node is set up."
        exit 1
    fi
    
    # Check if the node-ipc volume exists
    if ! docker volume ls | grep -q "node-ipc"; then
        echo "node-ipc volume not found. Please ensure the Cardano Node is set up."
        exit 1
    fi
    
    # Optional: Check if the Cardano node container is running
    if ! docker ps | grep -q "cardano-node-container"; then
        echo "Cardano node container is not running. Please start the Cardano Node first."
        exit 1
    fi
    
    echo "Cardano Node resources verified."
}

# Function to display menu and read user choice
show_menu() {
    echo "Choose Docker Compose configuration to run:"
    echo "1) Cardano Node"
    echo "2) Cardano Wallet"
    echo "3) Cardano DB Sync"
    echo "4) Exit"
    read -p "Enter choice [1-4]: " choice
    main_choice=$choice
}

# Main script logic
while true; do
    show_menu
    
    case $main_choice in
        1)
            set_node_env_variables
            save_env_variables
            # docker-compose -f "$WORKSPACE_ROOT_DIR_ABSOLUTE/docker-compose/base/docker-compose.base.yml" -f "$WORKSPACE_ROOT_DIR_ABSOLUTE/docker-compose/cardano-node/docker-compose.node.yml" --verbose up -d
            docker-compose -f "$WORKSPACE_ROOT_DIR_ABSOLUTE/docker-compose/cardano-node/docker-compose.node.yml" --verbose up -d
        ;;
        2)
            load_env_variables  # This ensures shared variables like CARDANO_NETWORK are available
            check_node_resources
            set_wallet_env_variables
            docker-compose -f "$WORKSPACE_ROOT_DIR_ABSOLUTE/docker-compose/cardano-wallet/docker-compose.wallet.yml" --verbose up -d
        ;;
        3)
            load_env_variables  # This ensures shared variables like CARDANO_NETWORK are available
            check_node_resources
            set_dbsync_env_variables
            docker-compose -f "$WORKSPACE_ROOT_DIR_ABSOLUTE/docker-compose/cardano-dbsync/docker-compose.dbsync.yml" --verbose up -d
        ;;
        4)
            echo "Exiting."
            exit 0
        ;;
        *)
            echo "Invalid choice, please select a valid option."
        ;;
    esac
done
