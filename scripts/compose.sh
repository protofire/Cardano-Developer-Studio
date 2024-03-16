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

# Function to prompt user to set environment variables
set_env_variables() {
    read -p "Enter CARDANO_NODE_VERSION [default: 8.9.0]: " CARDANO_NODE_VERSION
    CARDANO_NODE_VERSION=${CARDANO_NODE_VERSION:-"8.9.0"}
    export CARDANO_NODE_VERSION
    
    read -p "Enter CARDANO_NETWORK [default: mainnet, options: testnet-preview]: " CARDANO_NETWORK
    CARDANO_NETWORK=${CARDANO_NETWORK:-mainnet}
    export CARDANO_NETWORK
    
    read -p "Enter CARDANO_NODE_DB_PATH absolute path [default: /var/lib/cardano/data]: " CARDANO_NODE_DB_PATH
    CARDANO_NODE_DB_PATH=${CARDANO_NODE_DB_PATH:-/var/lib/cardano/data}
    export CARDANO_NODE_DB_PATH
    
    # Check if the NODE_DB directory exists. If not, create it.
    if [ ! -d "$CARDANO_NODE_DB_PATH" ]; then
        echo "Creating directory for CARDANO_NODE_DB_PATH at $CARDANO_NODE_DB_PATH"
        sudo mkdir -p "$CARDANO_NODE_DB_PATH"
    else
        echo "Directory for CARDANO_NODE_DB_PATH already exists at $CARDANO_NODE_DB_PATH"
    fi
    
    read -p "Enter CARDANO_NODE_PORT [default: 3001]: " CARDANO_NODE_PORT
    CARDANO_NODE_PORT=${CARDANO_NODE_PORT:-3001}
    export CARDANO_NODE_PORT
    
}

# Function to display menu and read user choice
show_menu() {
    echo "Choose Docker Compose configuration to run:"
    echo "1) Base"
    echo "2) Cardano Node"
    echo "3) Cardano Wallet"
    echo "4) Cardano DB Sync"
    echo "5) Exit"
    read -p "Enter choice [1-5]: " choice
    main_choice=$choice
}

# Main script logic
while true; do
    show_menu
    
    case $main_choice in
        1)
            set_env_variables
            docker-compose -f "$WORKSPACE_ROOT_DIR_ABSOLUTE/docker-compose/base/docker-compose.base.yml" --verbose up -d
        ;;
        2)
            set_env_variables
            # docker-compose -f "$WORKSPACE_ROOT_DIR_ABSOLUTE/docker-compose/base/docker-compose.base.yml" -f "$WORKSPACE_ROOT_DIR_ABSOLUTE/docker-compose/cardano-node/docker-compose.node.yml" --verbose up -d
            docker-compose -f "$WORKSPACE_ROOT_DIR_ABSOLUTE/docker-compose/cardano-node/docker-compose.node.yml" --verbose up -d
        ;;
        3)
            set_env_variables
            docker-compose -f "$WORKSPACE_ROOT_DIR_ABSOLUTE/docker-compose/base/docker-compose.base.yml" -f "$WORKSPACE_ROOT_DIR_ABSOLUTE/docker-compose/cardano-wallet/docker-compose.wallet.yml" --verbose up -d
        ;;
        4)
            set_env_variables
            docker-compose -f "$WORKSPACE_ROOT_DIR_ABSOLUTE/docker-compose/base/docker-compose.base.yml" -f "$WORKSPACE_ROOT_DIR_ABSOLUTE/docker-compose/cardano-db-sync/docker-compose.db-sync.yml" --verbose up -d
        ;;
        5)
            echo "Exiting."
            exit 0
        ;;
        *)
            echo "Invalid choice, please select a valid option."
        ;;
    esac
done
