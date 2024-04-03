#!/bin/bash

# Exit on any error
set -e
# set -x # Enables a mode of the shell where all executed commands are printed to the terminal

source "$(dirname "${BASH_SOURCE[0]}")/utils.sh"

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
    echo "CARDANO_NODE_VERSION=${CARDANO_NODE_VERSION}" >> .env.cardano
    # Add other variables as needed
}

# Example function to load environment variables from a file
load_env_variables() {
    if [[ -f ".env.cardano" ]]; then
        source .env.cardano
        export CARDANO_NETWORK  # Make sure this is exported
        echo "CARDANO_NETWORK=${CARDANO_NETWORK}"
        export CARDANO_NODE_VERSION
        echo "CARDANO_NODE_VERSION=${CARDANO_NODE_VERSION}"
    else
        echo "No saved environment variables found. Please set up the Cardano Node first."
        exit 1
    fi
}
# Function to prompt user to set environment variables
set_node_env_variables() {
    echo "----"
    echo "Setting up Cardano Node environment..."
    echo "----"
    echo "Installing required packages..."
    install_package jq
    install_package lz4
    install_package curl
    echo "----"
    read -p "Enter CARDANO_NODE_VERSION [default: 8.9.0]: " CARDANO_NODE_VERSION
    CARDANO_NODE_VERSION=${CARDANO_NODE_VERSION:-"8.9.0"}
    export CARDANO_NODE_VERSION
    
    while :; do
        read -p "Enter CARDANO_NETWORK [options: preprod, mainnet] (default: preprod): " CARDANO_NETWORK
        CARDANO_NETWORK=${CARDANO_NETWORK:-preprod}
        
        if [[ "$CARDANO_NETWORK" == "preprod" || "$CARDANO_NETWORK" == "mainnet" ]]; then
            export CARDANO_NETWORK
            break
        else
            echo "Invalid network. Please enter 'preprod' or 'mainnet'."
        fi
    done
    
    read -p "Enter CARDANO_NODE_PORT [default: 3001]: " CARDANO_NODE_PORT
    CARDANO_NODE_PORT=${CARDANO_NODE_PORT:-3001}
    export CARDANO_NODE_PORT
    
    read -p "Enter CARDANO_NODE_HEALTH_PORT [default: 12788]: " CARDANO_NODE_HEALTH_PORT
    CARDANO_NODE_HEALTH_PORT=${CARDANO_NODE_HEALTH_PORT:-12788}
    export CARDANO_NODE_HEALTH_PORT
    
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
    # change permissions
    sudo chmod -R 755 "$CARDANO_NODE_DB_PATH"
    sudo chown -R $(whoami) "$CARDANO_NODE_DB_PATH"
    
    if  [[ "$CARDANO_NETWORK" == "mainnet" || "$CARDANO_NETWORK" == "preprod" ]]; then
        read -p "Download snapshot for faster setup? (yes/no) [default: yes]: " download_snapshot
        download_snapshot=${download_snapshot:-yes}
        
        if [[ "$download_snapshot" =~ ^[Yy][Ee][Ss]$ ]]; then
            read -p "Enter path for saving snapshot [default: $CONFIG_DIR_ABSOLUTE/cardano-node-snapshot/$CARDANO_NETWORK]: " SNAPSHOT_SAVE_PATH
            SNAPSHOT_SAVE_PATH=${SNAPSHOT_SAVE_PATH:-"$CONFIG_DIR_ABSOLUTE/cardano-node-snapshot/$CARDANO_NETWORK"}
            echo "Snapshots will be saved to: $SNAPSHOT_SAVE_PATH"
            export SNAPSHOT_SAVE_PATH
            # ensuring directory permissions are set appropriately.
            if [ ! -d "$SNAPSHOT_SAVE_PATH" ]; then
                echo "Creating directory for snapshots at $SNAPSHOT_SAVE_PATH with appropriate permissions..."
                sudo mkdir -p "$SNAPSHOT_SAVE_PATH"
                sudo chmod 755 "$SNAPSHOT_SAVE_PATH"
                sudo chown $(whoami) "$SNAPSHOT_SAVE_PATH"
            fi
            
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
                echo "Latest snapshot available for download: $snapshot_url"
                if [ -f "$final_snapshot_path" ] && verify_snapshot_integrity "$final_snapshot_path"; then
                    echo "Snapshot file already exists. Skipping download."
                    USE_SNAPSHOT_PATH="$final_snapshot_path"
                else
                    if [ -f "$final_snapshot_path" ];  then
                        echo "Snapshot file already exists but is corrupted."
                    fi
                    while : ; do
                        available_snapshots=($(find "$SNAPSHOT_SAVE_PATH" -maxdepth 1 -name "*.lz4" 2>/dev/null))
                        if [ ${#available_snapshots[@]} -gt 0 ]; then
                            echo "Available snapshots:"
                            for i in "${!available_snapshots[@]}"; do
                                echo "$((i+1))) ${available_snapshots[$i]}"
                            done
                            echo "Select a snapshot to use (or press Enter to download the latest):"
                            read -p "Choice [1-${#available_snapshots[@]}]: " snapshot_choice
                            
                            if [[ -n "$snapshot_choice" && "$snapshot_choice" =~ ^[0-9]+$ && "$snapshot_choice" -ge 1 && "$snapshot_choice" -le ${#available_snapshots[@]} ]]; then
                                selected_snapshot_path="${available_snapshots[$((snapshot_choice-1))]}"
                                if verify_snapshot_integrity "$selected_snapshot_path"; then
                                    echo "Using selected snapshot: $selected_snapshot_path"
                                    USE_SNAPSHOT_PATH="$selected_snapshot_path"
                                    break
                                else
                                    echo "Selected snapshot failed integrity check or is corrupted. Please select a different snapshot."
                                fi
                                elif [[ -z "$snapshot_choice" ]]; then
                                echo "Downloading and using the latest snapshot..."
                                sudo mkdir -p "$SNAPSHOT_SAVE_PATH"
                                if curl -L "https://downloads.csnapshots.io/$url_network_segment/$snapshot_url" --progress-bar --output "$final_snapshot_path" && verify_snapshot_integrity "$final_snapshot_path"; then
                                    echo "Snapshot downloaded and verified successfully."
                                    USE_SNAPSHOT_PATH="$final_snapshot_path"
                                    break
                                else
                                    echo "Failed to download or verify snapshot. Please check your internet connection or try again later."
                                    exit 1
                                fi
                            else
                                echo "Invalid choice, please try again."
                            fi
                        else
                            echo "No available snapshots found. Downloading the latest..."
                            sudo mkdir -p "$SNAPSHOT_SAVE_PATH"
                            if curl -L "https://downloads.csnapshots.io/$url_network_segment/$snapshot_url" --progress-bar --output "$final_snapshot_path" && verify_snapshot_integrity "$final_snapshot_path"; then
                                echo "Snapshot downloaded and verified successfully."
                                USE_SNAPSHOT_PATH="$final_snapshot_path"
                                break
                            else
                                echo "Failed to download snapshot. Please check your internet connection or try again later."
                                exit 1
                            fi
                        fi
                    done
                fi
                sudo rm -rf "$CARDANO_NODE_DB_PATH/*"
                sudo mkdir -p "$CARDANO_NODE_DB_PATH"
                if lz4 -c -d "$USE_SNAPSHOT_PATH" | sudo tar -x -C "$CARDANO_NODE_DB_PATH"; then
                    # Instead of moving the entire db directory, synchronize its contents to the parent directory
                    if [ -d "$CARDANO_NODE_DB_PATH/db" ]; then
                        echo "Merging extracted 'db' directory contents with the target directory..."
                        # Use 'rsync' to merge directories content safely
                        sudo rsync -avh --remove-source-files "$CARDANO_NODE_DB_PATH/db/" "$CARDANO_NODE_DB_PATH/"
                        # After rsync, remove the now empty 'db' directory
                        sudo find "$CARDANO_NODE_DB_PATH/db" -type d -empty -delete
                        # change permissions
                        sudo chmod -R 755 "$CARDANO_NODE_DB_PATH"
                        sudo chown -R $(whoami) "$CARDANO_NODE_DB_PATH"
                    fi
                    echo "Snapshot extracted and merged successfully."
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
    fi
    
    determine_network_with_magic "$CARDANO_NETWORK" "$CONFIG_DIR_ABSOLUTE"
}

set_wallet_env_variables() {
    echo "----"
    echo "Setting up Cardano Wallet environment..."
    
    read -p "Enter CARDANO_WALLET_VERSION [default: 2023.4.14]: " CARDANO_WALLET_VERSION
    CARDANO_WALLET_VERSION=${CARDANO_WALLET_VERSION:-2023.4.14}
    export CARDANO_WALLET_VERSION
    
    # Ensure CARDANO_NETWORK is set; if not, load from saved environment variables
    if [ -z "${CARDANO_NETWORK}" ]; then
        echo "CARDANO_NETWORK could not be determined. Please ensure the Cardano Node setup has been completed."
        exit 1
    fi
    
    read -p "Enter CARDANO_WALLET_PORT [default: 8090]: " CARDANO_WALLET_PORT
    CARDANO_WALLET_PORT=${CARDANO_WALLET_PORT:-8090}
    export CARDANO_WALLET_PORT
    
    read -p "Enter ICARUS_VERSION [default: v2023-04-14]: " ICARUS_VERSION
    ICARUS_VERSION=${ICARUS_VERSION:-v2023-04-14}
    export ICARUS_VERSION
    
    read -p "Enter ICARUS_PORT [default: 4444]: " ICARUS_PORT
    ICARUS_PORT=${ICARUS_PORT:-4444}
    export ICARUS_PORT
}

set_dbsync_env_variables() {
    echo "----"
    echo "Setting up Cardano DB Sync environment..."
    
    read -p "Enter POSTGRES_VERSION [default: 14.10-alpine]: " POSTGRES_VERSION
    POSTGRES_VERSION=${POSTGRES_VERSION:-"14.10-alpine"}
    export POSTGRES_VERSION
    
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
    
    read -p "Enter CARDANO_DBSYNC_VERSION [default: 13.2.0.1]: " CARDANO_DBSYNC_VERSION
    CARDANO_DBSYNC_VERSION=${CARDANO_DBSYNC_VERSION:-13.2.0.1}
    export CARDANO_DBSYNC_VERSION
    
    echo "Checking for existing Docker volumes. If found, you'll have the option to delete them. Should you choose not to delete, ensure that your specified database details (name, user, and password) align with those of the existing setup."
    
    force_delete_docker_volume "cardano-dbsync-postgres-data-${POSTGRES_VERSION:-"14.10-alpine"}-${CARDANO_NODE_VERSION:-"8.9.0"}-${CARDANO_NETWORK:-mainnet}"
    force_delete_docker_volume "cardano-dbsync-data-${CARDANO_DBSYNC_VERSION:-"13.2.0.1"}-${CARDANO_NODE_VERSION:-"8.9.0"}-${CARDANO_NETWORK:-mainnet}"
}


# Function to display menu and read user choice
show_menu() {
    echo "----"
    echo "Docker Compose configuration to run"
    echo "----"
    echo "1) Cardano Node"
    echo "2) Cardano Wallet"
    echo "3) Cardano DB Sync"
    echo "4) Return Main Menu"
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
            PROJECT_NAME=$(echo "cardano-node-${CARDANO_NODE_VERSION:-"8.9.0"}-${CARDANO_NETWORK:-mainnet}" | tr '.:' '_' | tr -d '[:upper:]')
            docker-compose -p $PROJECT_NAME -f "$WORKSPACE_ROOT_DIR_ABSOLUTE/docker-compose/cardano-node/docker-compose.node.yml" --verbose up -d
            read -p "Press Enter to continue..."
        ;;
        2)
            check_node_resources
            set_wallet_env_variables
            PROJECT_NAME=$(echo "cardano-wallet-${CARDANO_WALLET_VERSION:-2023.04.14}-${CARDANO_NODE_VERSION:-"8.9.0"}-${CARDANO_NETWORK:-mainnet}" | tr '.:' '_' | tr -d '[:upper:]')
            docker-compose -p $PROJECT_NAME -f "$WORKSPACE_ROOT_DIR_ABSOLUTE/docker-compose/cardano-wallet/docker-compose.wallet.yml" --verbose up -d
            read -p "Press Enter to continue..."
        ;;
        3)
            check_node_resources
            set_dbsync_env_variables
            PROJECT_NAME=$(echo "cardano-dbsync-${CARDANO_DBSYNC_VERSION:-"13.2.0.1"}-${CARDANO_NODE_VERSION:-"8.9.0"}-${CARDANO_NETWORK:-mainnet}" | tr '.:' '_' | tr -d '[:upper:]')
            docker-compose -p $PROJECT_NAME -f "$WORKSPACE_ROOT_DIR_ABSOLUTE/docker-compose/cardano-dbsync/docker-compose.dbsync.yml" --verbose up -d
            read -p "Press Enter to continue..."
        ;;
        4)
            echo "returning to Main Menu.."
            exit 0
        ;;
        *)
            echo "Invalid choice, please select a valid option."
            read -p "Press Enter to continue..."
        ;;
    esac
    
done
