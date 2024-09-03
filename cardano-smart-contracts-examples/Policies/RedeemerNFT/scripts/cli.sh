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
    local contract_choice
    selected_policy="redeemerNftPolicy"
    script_cs=$(jq -r '.bytes' "$selected_scripts/$selected_policy.symbol")
    echo "Selected policy: $script_cs"
}

# Function to generate redeemer JSON
generate_redeemer_json() {
    local redeemer=$1
    local file_path=$2
    case $redeemer in
        Mint )
            cat <<EOM > "$file_path"
{
    "constructor": 0,
    "fields": []
}
EOM
            ;;
        Burn )
            cat <<EOM > "$file_path"
{
    "constructor": 1,
    "fields": []
}
EOM
            ;;
        *)
            echo "Invalid redeemer type: $redeemer" >&2
            return 1
            ;;
    esac
}

# Main script logic
main() {
    while true; do
        show_main_menu
        case $main_choice in
            1)
              if [[ -z $INSIDE_DEV_CONTAINER  ]]; then
                cd $WORKSPACE_ROOT_DIR_ABSOLUTE/cardano-smart-contracts-examples
              fi
                test_smart_contract
              if [[ -z $INSIDE_DEV_CONTAINER  ]]; then
                cd -
              fi
            ;;
            2)
                select_node_container
                if [[ -z "$selected_node_container" ]]; then
                    echo "No container selected. Returning to main menu."
                    read -p "Press Enter to continue..."
                    continue
                fi

                select_wallet_files
                if [[ -z "$selected_wallet" ]]; then
                    echo "No wallet selected. Returning to main menu."
                    read -p "Press Enter to continue..."
                    continue
                fi

                wallet_address=$(cat "$selected_wallet/$(basename $selected_wallet).addr")
                
                echo "Choosing UTXOs from wallet to use as parameter for policy..."
                select_utxo_parameter=$(select_single_utxo "$selected_node_container" "$wallet_address")
                if [[ $? -ne 0 || -z "$select_utxo_parameter" ]]; then
                    echo "No UTXO selected. Returning to main menu."
                    read -p "Press Enter to continue..."
                    continue
                fi

                if [[ -z $INSIDE_DEV_CONTAINER  ]]; then
                    cd $WORKSPACE_ROOT_DIR_ABSOLUTE/cardano-smart-contracts-examples
                fi

                deploy_smart_contract "$select_utxo_parameter"

                if [[ -z $INSIDE_DEV_CONTAINER  ]]; then
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
                            sw_debug=0
                            if [[ -z "$selected_node_container" || -z "$selected_scripts" || -z "$selected_wallet" ]]; then
                                echo "Please select the container, smart contract files, and wallet files first."
                                read -p "Press Enter to continue..."
                            else
                                create_minting_tx "Mint"
                            fi
                        ;;
                        7)
                            sw_debug=0
                            if [[ -z "$selected_node_container" || -z "$selected_scripts" || -z "$selected_wallet" ]]; then
                                echo "Please select the container, smart contract files, and wallet files first."
                                read -p "Press Enter to continue..."
                            else
                                create_burning_tx "Burn"
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
