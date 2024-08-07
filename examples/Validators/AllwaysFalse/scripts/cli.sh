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
    echo "6) Create Vesting Transaction"
    echo "7) Create Claiming Transaction"
    echo "0) Return to Main Menu"
    read -p "Enter your choice or 0 to return: " tx_choice
}

select_contract() {
    selected_validator="allwaysFalseValidator"
    script_address=$(cat "$selected_scripts/$selected_validator-$CARDANO_NETWORK_SUFIX.addr")
    echo "Selected address: $script_address"
}

utxos_in_smart_contracts() {
    echo "Checking UTXOs in Smart Contracts..."
    echo ""

    echo "Checking balance in AllwaysFalse Validator..."
    script_address=$(cat "$selected_scripts/allwaysFalseValidator-$CARDANO_NETWORK_SUFIX.addr")
    utxos_in_address "$script_address"
    echo ""
    read -p "Press Enter to continue..."
}

# Function to generate datum JSON
generate_datum_json() {
    local validator=$1
    case $validator in
        allwaysFalseValidator )
            echo "{}"
            ;;
    esac
}

create_vesting_tx() {
    echo "Creating vesting transaction..."
    select_contract

    datum_json=$(generate_datum_json "$selected_validator")
    if [[ $? -ne 0 ]]; then
        echo "Failed to generate datum JSON."
        read -p "Press Enter to continue..."
        return
    fi

    # Save datum JSON to a file
    datum_json_file="/tmp/datum.json"
    echo "$datum_json" > "$datum_json_file"
    docker cp "$datum_json_file" "$selected_node_container:$datum_json_file"

    amount_ada=$(select_amount_ada)

    wallet_address=$(cat "$selected_wallet/$(basename $selected_wallet).addr")
    echo "Choosing UTXOs from wallet to use as inputs..."
    select_utxos_output=$(select_utxos "$selected_node_container" "$wallet_address" $amount_ada)
    IFS='|' read -r wallet_tx_in_list total_lovelace_in_list <<< "$select_utxos_output"

    if [[ -n "$wallet_tx_in_list" ]]; then
        IFS=' ' read -ra wallet_tx_ins <<< "$wallet_tx_in_list"
        for wallet_tx_in in "${wallet_tx_ins[@]}"; do
            tx_in_list+=" --tx-in $wallet_tx_in"
        done
    fi
    
    tx_out_list="--tx-out $script_address+$amount_ada --tx-out-inline-datum-file /tmp/datum.json"
    echo "$tx_out_list" 

    build_and_submit_transaction "$selected_node_container" "$selected_wallet" "$tx_in_list" "$tx_out_list" "$wallet_address"

    read -p "Press Enter to continue..."
}

create_claiming_tx() {
    echo "Creating claiming transaction..."
    select_contract

    wallet_address=$(cat "$selected_wallet/$(basename $selected_wallet).addr")
    echo "Choosing UTXOs from wallet to use as inputs..."
    select_utxos_output=$(select_utxos "$selected_node_container" "$wallet_address")
    IFS='|' read -r wallet_tx_in_list total_lovelace_in_list <<< "$select_utxos_output"

    echo "Choosing UTXOs from script address to consume..."
    select_utxos_output=$(select_utxos "$selected_node_container" "$script_address")
    IFS='|' read -r script_tx_in_list total_lovelace_in_script <<< "$select_utxos_output"
    
    tx_in_script_file="$selected_scripts/$selected_validator.plutus"
    docker cp "$tx_in_script_file" "$selected_node_container:/tmp/validator.plutus"

    tx_in_list=""
    
    if [[ -n "$wallet_tx_in_list" ]]; then
        IFS=' ' read -ra wallet_tx_ins <<< "$wallet_tx_in_list"
        for wallet_tx_in in "${wallet_tx_ins[@]}"; do
            tx_in_list+=" --tx-in $wallet_tx_in"
            tx_in_collateral=" --tx-in-collateral $wallet_tx_in"
        done
    fi

    # Handle script tx-in list
    if [[ -n "$script_tx_in_list" ]]; then
        IFS=' ' read -ra script_tx_ins <<< "$script_tx_in_list"
        for script_tx_in in "${script_tx_ins[@]}"; do
            tx_in_list+=" --tx-in $script_tx_in --tx-in-script-file /tmp/validator.plutus --tx-in-inline-datum-present --tx-in-redeemer-value {} $tx_in_collateral"
        done
    fi
    
    echo "$tx_in_list" 

    build_and_submit_transaction "$selected_node_container" "$selected_wallet" "$tx_in_list" "" "$wallet_address"
    
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
                            select_node_container
                        ;;
                        2)
                            select_smart_contract_files
                        ;;
                        3)
                            select_wallet_files
                        ;;
                        4)
                            if [[ -z "$selected_node_container" || -z "$selected_scripts" || -z "$selected_wallet" ]]; then
                                echo "Please select the container, smart contract files, and wallet files first."
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
                            if [[ -z "$selected_node_container" || -z "$selected_scripts" || -z "$selected_wallet" ]]; then
                                echo "Please select the container, smart contract files, and wallet files first."
                                read -p "Press Enter to continue..."
                            else
                                create_vesting_tx
                            fi
                        ;;
                        7)
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
