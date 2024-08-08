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
    echo "5) UTXOs in Smart Contracts"
    echo "6) Create UTXO for Collateral in Wallet"
    echo "7) Create Vesting Transaction"
    echo "8) Create Claiming Transaction"
    echo "0) Return to Main Menu"
    read -p "Enter your choice or 0 to return: " tx_choice
}

select_contract() {
    echo "Select the contract:"
    echo "1) Datum Check After Deadline Validator"
    echo "2) Datum Check Before Deadline Validator"
    echo "3) Param Check After Deadline Validator"
    echo "4) Param Check Before Deadline Validator"
    local contract_choice
    while true; do
        read -p "Enter your choice [1-4]: " contract_choice
        case $contract_choice in
            1) selected_validator="datumCheckAfterDeadlineValidator"; break;;
            2) selected_validator="datumCheckBeforeDeadlineValidator"; break;;
            3) selected_validator="paramCheckAfterDeadlineValidator"; break;;
            4) selected_validator="paramCheckBeforeDeadlineValidator"; break;;
            *) echo "Invalid choice, please select a valid option.";;
        esac
    done
    script_address=$(cat "$selected_scripts/$selected_validator-$CARDANO_NETWORK_SUFIX.addr")
    echo "Selected address: $script_address"
}

utxos_in_smart_contracts() {
    echo "Checking UTXOs in Smart Contracts..."
    echo ""

    echo "Checking balance in Datum Check After Deadline Validator..."
    script_address=$(cat "$selected_scripts/datumCheckAfterDeadlineValidator-$CARDANO_NETWORK_SUFIX.addr")
    utxos_in_address "$script_address"
    echo ""

    echo "Checking balance in Datum Check Before Deadline Validator..."
    script_address=$(cat "$selected_scripts/datumCheckBeforeDeadlineValidator-$CARDANO_NETWORK_SUFIX.addr")
    utxos_in_address "$script_address"
    echo ""

    echo "Checking balance in Param Check After Deadline Validator..."
    script_address=$(cat "$selected_scripts/paramCheckAfterDeadlineValidator-$CARDANO_NETWORK_SUFIX.addr")
    utxos_in_address "$script_address"
    echo ""

    echo "Checking balance in Param Check Before Deadline Validator..."
    script_address=$(cat "$selected_scripts/paramCheckBeforeDeadlineValidator-$CARDANO_NETWORK_SUFIX.addr")
    utxos_in_address "$script_address"
    echo ""

    read -p "Press Enter to continue..."
}

# Function to generate datum JSON
generate_datum_json() {
    local datum=$1
    local file_path=$2
    case $selected_validator in
        datumCheckAfterDeadlineValidator | datumCheckBeforeDeadlineValidator)
            local posix_time=$(get_posix_time)
            cat <<EOM > "$file_path"
{
    "int": $posix_time
}
EOM
            ;;
        *)
            echo "Invalid datum type: $datum" >&2
            return 1
            ;;
    esac
}

# Function to know if a datum must be used
sw_use_datum() {
    case $selected_validator in
        datumCheckAfterDeadlineValidator | datumCheckBeforeDeadlineValidator)
            return 0
            ;;
        *)
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
                            if [[ -z "$selected_node_container" || -z "$selected_scripts" || -z "$selected_wallet" ]]; then
                                echo "Please select the container, smart contract files, and wallet files first."
                                read -p "Press Enter to continue..."
                            else
                                utxos_in_smart_contracts
                            fi
                        ;;
                        6)
                            if [[ -z "$selected_node_container" || -z "$selected_wallet" ]]; then
                                echo "Please select the container, and wallet files first."
                                read -p "Press Enter to continue..."
                            else
                                create_collateral_tx
                            fi
                        ;;
                        7)
                            if [[ -z "$selected_node_container" || -z "$selected_scripts" || -z "$selected_wallet" ]]; then
                                echo "Please select the container, smart contract files, and wallet files first."
                                read -p "Press Enter to continue..."
                            else
                                create_vesting_tx
                            fi
                        ;;
                        8)
                            if [[ -z "$selected_node_container" || -z "$selected_scripts" || -z "$selected_wallet" ]]; then
                                echo "Please select the container, smart contract files, and wallet files first."
                                read -p "Press Enter to continue..."
                            else
                                create_claiming_tx
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
