#!/bin/bash

# Exit on any error
set -e

source "$(dirname "${BASH_SOURCE[0]}")/utils/utils.sh"

setWorkspaceDir

echo "----"
echo "WORKSPACE_ROOT_DIR_ABSOLUTE=$WORKSPACE_ROOT_DIR_ABSOLUTE"
echo "HOST_PROJECT_PATH=$HOST_PROJECT_PATH"


# Requirements check
echo "----"
echo "Checking requirements..."
check_package_manager || exit 1
echo "----"
# Installation of required packages
echo "Installing required packages..."
install_package jq
install_package lz4
install_package curl
install_package grep
install_package sed
install_package gawk
install_package cut
echo "----"
echo "Checking required software versions..."
check_bash_version "4.0" || exit 1
check_docker_version "19.03" || exit 1
check_docker_compose_version "1.25" || exit 1
echo "----"
echo "Setting permissions for all Bash scripts in $WORKSPACE_ROOT_DIR_ABSOLUTE/scripts..."
find "$WORKSPACE_ROOT_DIR_ABSOLUTE/scripts" -type f -name "*.sh" -exec sudo chmod +x {} \;
echo "Permissions set for all Bash scripts"

#--------------------------------------------------------------------------------

source "$WORKSPACE_ROOT_DIR_ABSOLUTE/scripts/menu-compose/compose.sh"
source "$WORKSPACE_ROOT_DIR_ABSOLUTE/scripts/menu-tools/cardano-node/tools-cardano-node.sh"
source "$WORKSPACE_ROOT_DIR_ABSOLUTE/scripts/menu-tools/cardano-wallet/tools-cardano-wallet.sh"
source "$WORKSPACE_ROOT_DIR_ABSOLUTE/scripts/menu-tools/cardano-dbsync/tools-cardano-dbsync.sh"
source "$WORKSPACE_ROOT_DIR_ABSOLUTE/scripts/menu-tools/ogmios/tools-ogmios.sh"
source "$WORKSPACE_ROOT_DIR_ABSOLUTE/scripts/menu-tools/kupo/tools-kupo.sh"

# Declare a global variable for main_choice
declare -g main_choice

# Function to display the main menu and capture the choice
show_main_menu() {
    echo "----"
    echo "Main Menu - Choose an option:"
    echo "----"
    echo "1) Docker Compose Workflow"
    echo "2) Cardano Node Tools"
    echo "3) Cardano Wallet Tools"
    echo "4) Cardano DB Sync Tools"
    echo "5) Ogmios Tools"
    echo "6) Kupo Tools"
    echo "0) Exit"
    read -p "Enter your choice or 0 to exit: " main_choice
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
           cardano_ogmios_tools
        ;;
        6)
           cardano_kupo_tools
        ;;
        0)
            echo "Exiting."
            exit 0
        ;;
        *)
            echo "Invalid choice, please select a valid option."
            read -p "Press Enter to continue..."
        ;;
    esac
    
done
