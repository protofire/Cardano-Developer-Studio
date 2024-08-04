#!/bin/bash

if [[ -f "$(dirname "${BASH_SOURCE[0]}")/../../../../scripts/utils.sh" ]]; then
    # cuando el script se ejecuta en container
    source "$(dirname "${BASH_SOURCE[0]}")/../../../../scripts/utils.sh"
elif [[ -f "$(dirname "${BASH_SOURCE[0]}")/../../../../../scripts/utils.sh" ]]; then
    # cuando el script se ejecuta en host
    source "$(dirname "${BASH_SOURCE[0]}")/../../../../../scripts/utils.sh"
else
    echo "Error: utils.sh not found in either path."
    exit 1
fi

setWorkspaceDir

# Function to get the project name from the .cabal file
get_project_name() {
    local cabal_file=$(find "$(dirname "${BASH_SOURCE[0]}")/../" -maxdepth 1 -name "*.cabal" | head -n 1)
    if [[ -f "$cabal_file" ]]; then
        grep -i '^name:' "$cabal_file" | awk '{print $2}'
    else
        echo "Error: .cabal file ($cabal_file) not found!" >&2
        exit 1
    fi
}

PROJECT_NAME=$(get_project_name)

# Function to show the main menu
show_main_menu() {
    echo "----"
    echo "Main Menu - Choose an option:"
    echo "----"
    echo "1) Test"
    echo "2) Deploy"
    echo "3) Create Transactions"
    echo "0) Exit"
    read -p "Enter your choice or 0 to exit: " main_choice
}

# Function to test the smart contract
test_smart_contract() {
    echo "Running tests..."
    cabal test "${PROJECT_NAME}Test"
    read -p "Press Enter to continue..."
}

# Function to deploy the smart contract
deploy_smart_contract() {
    echo "Deploying smart contract..."
    cabal run "${PROJECT_NAME}Deploy"
    read -p "Press Enter to continue..."
}

# Function to show the transaction menu
show_transaction_menu() {
    echo "----"
    echo "Transaction Menu - Choose an option:"
    echo "----"
    echo "1) Select Container with Node"
    echo "2) Select Smart Contract Files"
    echo "3) Select Wallet Files"
    echo "4) Balance in Wallet"
    echo "5) Balance in Smart Contract"
    echo "6) Create Vesting Transaction"
    echo "7) Create Claiming Transaction"
    echo "0) Return to Main Menu"
    read -p "Enter your choice or 0 to return: " tx_choice
}

# Placeholder functions for transaction menu options
select_container_with_node() {
    echo "Selecting container with node..."
    # Implementation needed
    read -p "Press Enter to continue..."
}

select_smart_contract_files() {
    echo "Selecting smart contract files..."
    # Implementation needed
    read -p "Press Enter to continue..."
}

select_wallet_files() {
    echo "Selecting wallet files..."
    # Implementation needed
    read -p "Press Enter to continue..."
}

balance_in_wallet() {
    echo "Checking balance in wallet..."
    # Implementation needed
    read -p "Press Enter to continue..."
}

balance_in_smart_contract() {
    echo "Checking balance in smart contract..."
    # Implementation needed
    read -p "Press Enter to continue..."
}

create_vesting_tx() {
    echo "Creating vesting transaction..."
    # Implementation needed
    read -p "Press Enter to continue..."
}

create_claiming_tx() {
    echo "Creating claiming transaction..."
    # Implementation needed
    read -p "Press Enter to continue..."
}

# Main script logic
main() {
    while true; do
        show_main_menu
        case $main_choice in
            1)
                test_smart_contract
            ;;
            2)
                deploy_smart_contract
            ;;
            3)
                while true; do
                    show_transaction_menu
                    case $tx_choice in
                        1)
                            select_container_with_node
                        ;;
                        2)
                            select_smart_contract_files
                        ;;
                        3)
                            select_wallet_files
                        ;;
                        4)
                            balance_in_wallet
                        ;;
                        5)
                            balance_in_smart_contract
                        ;;
                        6)
                            create_vesting_tx
                        ;;
                        7)
                            create_claiming_tx
                        ;;
                        0)
                            break
                        ;;
                        *)
                            echo "Invalid choice, please select a valid option."
                            read -p "Press Enter to continue..."
                        ;;
                    esac
                done
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
}

# Start the main script
main
