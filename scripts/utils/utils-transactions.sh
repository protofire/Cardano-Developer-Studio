
# Function to get the project name from the .cabal file
get_project_name() {
    local cabal_file
    # Adding more debugging statements
    cabal_file=$(find "$CURRENT_SCRIPT_DIR/../" -maxdepth 1 -name "*.cabal" | head -n 1)
    if [[ -f "$cabal_file" ]]; then
        grep -i '^name:' "$cabal_file" | awk '{print $2}'
    else
        echo "Error: .cabal file ($cabal_file) not found!" >&2
        exit 1
    fi
}


# Function to show the main menu
show_main_menu() {
    echo "----"
    echo "Main Menu - $PROJECT_NAME - Choose an option:"
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
    echo "Deploying smart contract... $WORKSPACE_ROOT_DIR_ABSOLUTE"
    cabal run "${PROJECT_NAME}Deploy" "$WORKSPACE_ROOT_DIR_ABSOLUTE"
    read -p "Press Enter to continue..."
}


# Specific function for node container selection
select_node_container() {
    echo "Selecting container with node..."
    if ! select_container 'cardano-node-container' 0; then
        if [ $? -eq 2 ]; then
            return 0  # Return 0 in any case
        else
            return 0
        fi
    else
        selected_node_container=$selected_container
        CARDANO_NETWORK=$(docker exec -it "$selected_node_container" printenv CARDANO_NETWORK | tr -d '\r')
        if [[ "$CARDANO_NETWORK" == "mainnet" ]]; then
            CARDANO_NETWORK_SUFIX="Mainnet"
        else
            CARDANO_NETWORK_SUFIX="Testnet"
        fi
        determine_network_with_magic "$CARDANO_NETWORK" "$WORKSPACE_ROOT_DIR_ABSOLUTE/configs"
    fi
    # read -p "Press Enter to continue..."
}

select_smart_contract_files() {
    echo "Selecting smart contract files..."
    parent_folder="$WORKSPACE_ROOT_DIR_ABSOLUTE/export/$PROJECT_NAME"  # Replace with your actual parent folder path
    if ! select_folder "$parent_folder"; then
        if [ $? -eq 2 ]; then
            # echo "User chose to exit without selecting a folder."
            return 0 
        else
            # echo "Failed to select a folder."
            return 0 
        fi
    else
        # echo "Selected folder: $selected_folder"
        # Perform actions with $selected_folder
        selected_scripts=$selected_folder
    fi
    # read -p "Press Enter to continue..."
}

select_wallet_files() {
    echo "Selecting wallet files..."
    parent_folder="$WORKSPACE_ROOT_DIR_ABSOLUTE/.priv/wallets"  # Replace with your actual parent folder path
    if ! select_folder "$parent_folder"; then
        if [ $? -eq 2 ]; then
            # echo "User chose to exit without selecting a folder."
            return 0 
        else
            # echo "Failed to select a folder."
            return 0 
        fi
    else
        # echo "Selected folder: $selected_folder"
        # Perform actions with $selected_folder
        selected_wallet=$selected_folder
    fi
    # read -p "Press Enter to continue..."
}

utxos_in_wallet() {
    echo "Checking balance in wallet..."
    address=$(cat "$selected_wallet/$(basename $selected_wallet).addr")
    utxos_in_address "$address"
    read -p "Press Enter to continue..."
}

utxos_in_address() {
    local address=$1 
    echo "Listing UTXOs for address: $address"
    utxos=$(docker exec -i "$selected_node_container" cardano-cli query utxo --socket-path /ipc/node.socket --address "$address" --$CARDANO_NETWORK_WITH_MAGIC)
    echo "UTXOs:"
    echo "$utxos"
}

select_amount_ada() {
    # TODO: get total balance and check if it is enough
    local amount_ada
    while true; do
        read -p "Enter the amount of ADA (minimum 1000000, default 2000000): " amount_ada
        if [[ -z "$amount_ada" ]]; then
            amount_ada=2000000
        fi
        if [[ $amount_ada -ge 1000000 ]]; then
            echo "$amount_ada"
            break
        else
            echo "Amount of ADA must be at least 1000000."
        fi
    done
}

# Function to prompt user for POSIX time input and validate it
get_posix_time() {
    local default_posix_time=$(date +%s)
    while true; do
        read -p "Enter POSIX time (seconds since epoch, default $default_posix_time): " posix_time
        if [[ -z "$posix_time" ]]; then
            posix_time=$default_posix_time
        fi
        if [[ "$posix_time" =~ ^[0-9]+$ ]]; then
            echo $posix_time
            return
        else
            echo "Invalid POSIX time. Please enter a non-negative integer."
        fi
    done
}

# Function to prompt user for public key hash and validate it
get_pkh() {
    while true; do
        read -p "Enter public key hash: " pkh 
        if [[ "$pkh" =~ ^[0-9a-fA-F]{56}$ ]]; then
            echo $pkh
            return
        else
            echo "Invalid public key hash. Please enter a valid 56-character hexadecimal string."
        fi
    done
}

select_utxos() {
    local container=$1
    local address=$2
    local amount_ada=${3:-0}
    
    local utxos

    echo "Querying UTXOs for address $address in container $container" >&2
    utxos=$(docker exec -i "$container" cardano-cli query utxo --socket-path /ipc/node.socket --address "$address" --$CARDANO_NETWORK_WITH_MAGIC)

    local selected_utxos=()
    local total_lovelace_in_list=0
    local tx_in_list=""

    echo "Select UTXOs to use as inputs:" >&2

    # Prepare the numbered list of UTXOs
    local utxo_list=()
    local count=1
    while IFS= read -r line; do
        if [ $count -gt 2 ]; then
            utxo_list+=("$line")
            echo "$((count-2))) $line" >&2
        fi
        count=$((count+1))
    done <<< "$utxos"

    while true; do
        read -p "Enter UTXO number (0 to finish): " utxo_number
        if [[ $utxo_number -eq 0 ]]; then
            if [[ $total_lovelace_in_list -ge $amount_ada ]]; then
                break
            else
                echo "Total lovelace in selected UTXOs ($total_lovelace_in_list) is less than the required amount ($amount_ada). Please select more UTXOs." >&2
            fi
        fi
        if [[ $utxo_number -gt 0 && $utxo_number -le ${#utxo_list[@]} ]]; then
            local utxo="${utxo_list[$((utxo_number-1))]}"
            local tx_hash=$(echo "$utxo" | awk '{print $1}')
            local tx_ix=$(echo "$utxo" | awk '{print $2}')
            local tx_amount=$(echo "$utxo" | awk '{print $3}')
            local utxo_id="$tx_hash#$tx_ix"
            
            # Check if the UTXO is already selected
            if [[ " ${selected_utxos[*]} " == *"$utxo_id"* ]]; then
                echo "UTXO $utxo_id is already selected. Please choose a different UTXO." >&2
            else
                tx_in_list+=" $utxo_id"
                selected_utxos+=("$utxo_id")
                total_lovelace_in_list=$((total_lovelace_in_list + tx_amount))
                echo "Selected UTXO: $utxo_id with $tx_amount lovelace" >&2
            fi
        else
            echo "Invalid UTXO number, please try again." >&2
        fi
    done

    echo "Selected UTXOs:" >&2
    for utxo in "${selected_utxos[@]}"; do
        echo "$utxo" >&2
    done
    echo "Total Lovelace in list: $total_lovelace_in_list" >&2

    echo "$tx_in_list|$total_lovelace_in_list"
}

build_and_submit_transaction() {
    local container=$1
    local walletPath=$2
    local tx_in_list=$3
    local tx_out_list=$4
    local change_address=$5

    echo "Creating the transaction..."
    if ! docker exec -i "$container" cardano-cli transaction build \
            --babbage-era \
            --socket-path /ipc/node.socket \
            --$CARDANO_NETWORK_WITH_MAGIC \
            $tx_in_list $tx_out_list \
            --change-address "$change_address" \
            --out-file "/tmp/tx.body"; then
        echo "Error: Failed to build the transaction. Check the input parameters and script." >&2
        return 0  # Indicate failure
    fi

    echo "Signing the transaction..."
    wallet_skey_path="$walletPath/$(basename $walletPath).skey"
    docker cp "$wallet_skey_path" "$container:/tmp/wallet.skey"
    if ! docker exec -i "$container" cardano-cli transaction sign \
            --tx-body-file "/tmp/tx.body" \
            --signing-key-file "/tmp/wallet.skey" \
            --$CARDANO_NETWORK_WITH_MAGIC \
            --out-file "/tmp/tx.signed"; then
        echo "Error: Failed to sign the transaction. Check wallet and key permissions." >&2
        docker exec -i "$container" rm /tmp/wallet.skey  # Clean up
        return 0
    fi

    docker exec -i "$container" rm /tmp/wallet.skey

    echo "Submitting the transaction..."
    if ! docker exec -i "$container" cardano-cli transaction submit \
            --$CARDANO_NETWORK_WITH_MAGIC \
            --socket-path /ipc/node.socket \
            --tx-file "/tmp/tx.signed"; then
        echo "Error: Failed to submit the transaction. Check network connection and transaction details." >&2
        return 0
    fi

    echo "Transaction submitted successfully!"
}
