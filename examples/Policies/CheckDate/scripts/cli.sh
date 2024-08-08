#!/bin/bash

CURRENT_SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
# echo "Current script directory: $CURRENT_SCRIPT_DIR"

if [[ -f "$CURRENT_SCRIPT_DIR/../../../scripts/utils/utils.sh" ]]; then
    # cuando el script se ejecuta en container
    source "$CURRENT_SCRIPT_DIR/../../../scripts/utils/utils.sh"
    source "$CURRENT_SCRIPT_DIR/../../../scripts/utils/utils-transactions.sh"
elif [[ -f "$CURRENT_SCRIPT_DIR/../../../../scripts/utils/utils.sh" ]]; then
    # cuando el script se ejecuta en host
    source "$CURRENT_SCRIPT_DIR/../../../../scripts/utils/utils.sh"
    source "$CURRENT_SCRIPT_DIR/../../../../scripts/utils/utils-transactions.sh"
else
    echo "Error: utils.sh not found in either path."
    exit 1
fi

setWorkspaceDir

PROJECT_NAME=$(get_project_name)

# Function to show the transaction menu
show_transaction_menu() {
    echo "----"
    echo "Transaction Menu - $PROJECT_NAME - Choose an option:"
    echo "----"
    echo "1) Select Container with Node - Selected: $selected_node_container"
    echo "2) Select Smart Contract Files - Selected: $(basename "$selected_scripts")"
    echo "3) Select Wallet Files - Selected: $(basename "$selected_wallet")"
    echo "4) UTXOs in Wallet"
    echo "5) Create UTXO for Collateral in Wallet"
    echo "6) Create Minting Transaction"
    echo "7) Create Burning Transaction"
    echo "0) Return to Main Menu"
    read -p "Enter your choice or 0 to return: " tx_choice
}

select_contract() {
    echo "Select the contract:"
    echo "1) Param Check After Deadline Validator"
    echo "2) Param Check Before Deadline Validator"
    local contract_choice
    while true; do
        read -p "Enter your choice [1-2]: " contract_choice
        case $contract_choice in
            1) selected_policy="paramCheckAfterDeadlinePolicy"; break;;
            2) selected_policy="paramCheckBeforeDeadlinePolicy"; break;;
            *) echo "Invalid choice, please select a valid option.";;
        esac
    done
    echo "$selected_scripts/$selected_policy.symbol"
    script_cs=$(jq -r '.bytes' "$selected_scripts/$selected_policy.symbol")
    echo "Selected policy: $script_cs"
}

# Main script logic
main() {
    while true; do
        show_main_menu
        case $main_choice in
            1)
              if [[ $DEV_CONTAINER != 0  ]]; then
                cd examples
              fi
                test_smart_contract
              if [[ $DEV_CONTAINER != 0  ]]; then
                cd -
              fi
            ;;
            2)
                if [[ $DEV_CONTAINER != 0  ]]; then
                    cd examples
                fi

                deploy_smart_contract

                if [[ $DEV_CONTAINER != 0  ]]; then
                    cd -
                fi
            ;;
            3)
                while true; do
                    show_transaction_menu
                    case $tx_choice in
                        1)
                            select_node_container
                        ;;
                        2)
                            select_smart_contract_files
                        ;;
                        3)
                            select_wallet_files
                        ;;
                        4)
                            if [[ -z "$selected_node_container" || -z "$selected_wallet" ]]; then
                                echo "Please select the container and wallet files first."
                                read -p "Press Enter to continue..."
                            else
                                utxos_in_wallet
                            fi
                        ;;
                        5)
                            if [[ -z "$selected_node_container" || -z "$selected_wallet" ]]; then
                                echo "Please select the container, and wallet files first."
                                read -p "Press Enter to continue..."
                            else
                                create_collateral_tx
                            fi
                        ;;
                        6)
                            if [[ -z "$selected_node_container" || -z "$selected_scripts" || -z "$selected_wallet" ]]; then
                                echo "Please select the container, smart contract files, and wallet files first."
                                read -p "Press Enter to continue..."
                            else
                                create_minting_tx
                            fi
                        ;;
                        7)
                            if [[ -z "$selected_node_container" || -z "$selected_scripts" || -z "$selected_wallet" ]]; then
                                echo "Please select the container, smart contract files, and wallet files first."
                                read -p "Press Enter to continue..."
                            else
                                create_burning_tx
                            fi
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
                echo "Exiting $PROJECT_NAME"
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
