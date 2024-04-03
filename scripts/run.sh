#!/bin/bash

# Exit on any error
set -e

source "$(dirname "${BASH_SOURCE[0]}")/utils.sh"
source "$(dirname "${BASH_SOURCE[0]}")/utils-cardano-node.sh"
source "$(dirname "${BASH_SOURCE[0]}")/utils-cardano-wallet.sh"
source "$(dirname "${BASH_SOURCE[0]}")/utils-cardano-dbsync.sh"

# Determine the directory where run.sh resides
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

# Change to the script's directory
cd "$SCRIPT_DIR"

export WORKSPACE_ROOT_DIR_ABSOLUTE="$SCRIPT_DIR/.."
export CONFIG_DIR_ABSOLUTE="$WORKSPACE_ROOT_DIR_ABSOLUTE/configs"

# Function to set execute permissions on necessary scripts
set_script_permissions() {
    # Ensure compose.sh is executable
    chmod +x "$SCRIPT_DIR/compose.sh"
}

# Declare a global variable for main_choice
declare -g main_choice

# Function to display the main menu and capture the choice
show_main_menu() {
    echo "----"
    echo "Main Menu - Choose an option:"
    echo "----"
    echo "1) Docker Compose Workflow"
    echo "2) Cardano Node Testing and Tools"
    echo "3) Cardano Wallet Testing and Tools"
    echo "4) Cardano DB Sync Tools"
    echo "5) Other Tool [Placeholder]"
    echo "6) Exit"
    read -p "Enter choice [1-6]: " main_choice
}

# Function to handle Docker Compose Workflow
docker_compose_workflow() {
    # Ensure scripts have execute permissions
    set_script_permissions
    
    # Use SCRIPT_DIR to reference compose.sh relative to run.sh's location
    "$SCRIPT_DIR/compose.sh"
}


# Main script logic
while true; do
    show_main_menu
    
    case $main_choice in
        1)
            docker_compose_workflow
        ;;
        2)
            cardano_node_tools
        ;;
        3)
            cardano_wallet_tools
        ;;
        4)
            cardano_dbsync_tools
        ;;
        5)
            # Placeholder for other tools
            echo "Other tools (Placeholder)"
            read -p "Press Enter to continue..."
        ;;
        6)
            echo "Exiting."
            exit 0
        ;;
        *)
            echo "Invalid choice, please select a valid option."
            read -p "Press Enter to continue..."
        ;;
    esac
    
done
