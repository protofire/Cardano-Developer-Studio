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
    selected_policy="redeemerFtPolicy"
    script_cs=$(jq -r '.bytes' "$selected_scripts/$selected_policy.symbol")
    echo "Selected policy: $script_cs"
}

# Function to generate redeemer JSON
generate_redeemer_json() {
    local redeemer=$1
    local file_path=$2
    case $redeemer in
        mint )
            cat <<EOM > "$file_path"
{
    "constructor": 0,
    "fields": []
}
EOM
            ;;
        burn )
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

create_minting_tx() {
    echo "Creating minting transaction..."
    select_contract

    wallet_address=$(cat "$selected_wallet/$(basename $selected_wallet).addr")
    echo "Choosing UTXOs from wallet to use as inputs..."
    select_utxos_output=$(select_utxos "$selected_node_container" "$wallet_address" 1)
    IFS='|' read -r wallet_tx_in_list total_lovelace_in_list <<< "$select_utxos_output"

    tx_in_list=""
    if [[ -n "$wallet_tx_in_list" ]]; then
        IFS=' ' read -ra wallet_tx_ins <<< "$wallet_tx_in_list"
        for wallet_tx_in in "${wallet_tx_ins[@]}"; do
            tx_in_list+=" --tx-in $wallet_tx_in"
        done
    fi
    
    collateral_utxo=$(select_collateral_utxo "$selected_node_container" "$wallet_address")
    if [[ $? -ne 0 || -z "$collateral_utxo" ]]; then
        echo "No suitable collateral UTXO selected."
        read -p "Press Enter to continue..."
        return 0
    else
        tx_in_collateral=" --tx-in-collateral $collateral_utxo"
    fi
    
    tx_in_list+="$tx_in_collateral"

    # Prompt for token names and amounts
    local tokens=()
    local amounts=()

    while true; do
        read -p "Enter token name (leave empty to finish): " token_name
        if [[ -z "$token_name" && ${#tokens[@]} -eq 0 ]]; then
            echo "No tokens added. Please add at least one token."
            continue
        elif [[ -z "$token_name" ]]; then
            break
        fi

        # Convert token name to HEX
        token_name_hex=$(echo -n "$token_name" | xxd -p)
        echo "Token name in HEX: $token_name_hex"

        while true; do
            read -p "Enter amount of tokens to mint (default 1): " token_amount
            if [[ -z "$token_amount" ]]; then
                token_amount=1
            fi

            # Validate token amount
            if ! [[ "$token_amount" =~ ^-?[0-9]+$ ]]; then
                echo "Invalid amount. Please enter a valid number."
            else
                break
            fi
        done

        tokens+=("$token_name_hex")
        amounts+=("$token_amount")
    done

    # Prepare the minting parameters
    redeemer_json_file="/tmp/redeemer.json"
    generate_redeemer_json "mint" "$redeemer_json_file"
    docker cp "$redeemer_json_file" "$selected_node_container:$redeemer_json_file"

    tx_in_script_file="$selected_scripts/$selected_policy.plutus"
    docker cp "$tx_in_script_file" "$selected_node_container:/tmp/policy.plutus"
    pid=$(docker exec -i "$selected_node_container" cardano-cli transaction policyid --script-file /tmp/policy.plutus)

    mint_params=""
    for i in "${!tokens[@]}"; do
        token_name_hex="${tokens[$i]}"
        token_amount="${amounts[$i]}"
        mint_value="$token_amount $pid.$token_name_hex"
        mint_params+=" --mint \"$mint_value\" --mint-script-file /tmp/policy.plutus --mint-redeemer-file /tmp/redeemer.json"
    done
    
    # echo  "$tx_in_list" "" "$wallet_address" "$mint_params"
    build_and_submit_transaction "$selected_node_container" "$selected_wallet" "$tx_in_list" "" "$wallet_address" "$mint_params"

    read -p "Press Enter to continue..."
}

create_burning_tx() {
    echo "Creating burning transaction..."
    select_contract

    wallet_address=$(cat "$selected_wallet/$(basename $selected_wallet).addr")
    echo "Choosing UTXOs from wallet to use as inputs..."
    select_utxos_output=$(select_utxos "$selected_node_container" "$wallet_address")
    IFS='|' read -r wallet_tx_in_list total_lovelace_in_list <<< "$select_utxos_output"

    tx_in_list=""
    if [[ -n "$wallet_tx_in_list" ]]; then
        IFS=' ' read -ra wallet_tx_ins <<< "$wallet_tx_in_list"
        for wallet_tx_in in "${wallet_tx_ins[@]}"; do
            tx_in_list+=" --tx-in $wallet_tx_in"
            
        done
    fi

    collateral_utxo=$(select_collateral_utxo "$selected_node_container" "$wallet_address")
    if [[ $? -ne 0 || -z "$collateral_utxo" ]]; then
        echo "No suitable collateral UTXO selected."
        read -p "Press Enter to continue..."
        return 0
    else
        tx_in_collateral=" --tx-in-collateral $collateral_utxo"
    fi
    
    tx_in_list+="$tx_in_collateral"
    
    # Prompt for token names and amounts
    local tokens=()
    local amounts=()

    while true; do
        read -p "Enter token name (leave empty to finish): " token_name
        if [[ -z "$token_name" && ${#tokens[@]} -eq 0 ]]; then
            echo "No tokens added. Please add at least one token."
            continue
        elif [[ -z "$token_name" ]]; then
            break
        fi

        # Convert token name to HEX
        token_name_hex=$(echo -n "$token_name" | xxd -p)
        echo "Token name in HEX: $token_name_hex"

        while true; do
            read -p "Enter amount of tokens to burn (default 1): " token_amount
            if [[ -z "$token_amount" ]]; then
                token_amount=1
            fi

            # Validate token amount
            if ! [[ "$token_amount" =~ ^-?[0-9]+$ ]]; then
                echo "Invalid amount. Please enter a valid number."
            else
                break
            fi
        done

        tokens+=("$token_name_hex")
        amounts+=("$token_amount")
    done

    # Prepare the minting parameters
    redeemer_json_file="/tmp/redeemer.json"
    generate_redeemer_json "burn" "$redeemer_json_file"
    docker cp "$redeemer_json_file" "$selected_node_container:$redeemer_json_file"

    tx_in_script_file="$selected_scripts/$selected_policy.plutus"
    docker cp "$tx_in_script_file" "$selected_node_container:/tmp/policy.plutus"
    pid=$(docker exec -i "$selected_node_container" cardano-cli transaction policyid --script-file /tmp/policy.plutus)

    mint_params=""
    for i in "${!tokens[@]}"; do
        token_name_hex="${tokens[$i]}"
        token_amount="${amounts[$i]}"
        mint_value="-$token_amount $pid.$token_name_hex"
        mint_params+=" --mint \"$mint_value\" --mint-script-file /tmp/policy.plutus --mint-redeemer-file /tmp/redeemer.json"
    done
    
    # echo  "$tx_in_list" "" "$wallet_address" "$mint_params"
    build_and_submit_transaction "$selected_node_container" "$selected_wallet" "$tx_in_list" "" "$wallet_address" "$mint_params"

    read -p "Press Enter to continue..."

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

                deploy_smart_contract "$select_utxo_parameter"
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
