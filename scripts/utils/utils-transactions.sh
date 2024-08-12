
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
    local params=$1
    echo "Deploying smart contract... $WORKSPACE_ROOT_DIR_ABSOLUTE"
    cabal run "${PROJECT_NAME}Deploy" "$WORKSPACE_ROOT_DIR_ABSOLUTE" $params
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

    echo "Select UTXOs" >&2

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

select_single_utxo() {
    local container=$1
    local address=$2
    
    local utxos

    echo "Querying UTXOs for address $address in container $container" >&2
    utxos=$(docker exec -i "$container" cardano-cli query utxo --socket-path /ipc/node.socket --address "$address" --$CARDANO_NETWORK_WITH_MAGIC)

    local selected_utxo=""
    local tx_in_list=""

    echo "Select a UTXO:" >&2

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

    # Check if there are no UTXOs available
    if [ ${#utxo_list[@]} -eq 0 ]; then
        echo "No UTXOs available for address $address" >&2
        return 1
    fi

    while true; do
        read -p "Enter UTXO number: " utxo_number
        if [[ $utxo_number -gt 0 && $utxo_number -le ${#utxo_list[@]} ]]; then
            local utxo="${utxo_list[$((utxo_number-1))]}"
            local tx_hash=$(echo "$utxo" | awk '{print $1}')
            local tx_ix=$(echo "$utxo" | awk '{print $2}')
            local utxo_id="$tx_hash#$tx_ix"
            
            tx_in_list="$utxo_id"
            selected_utxo="$utxo_id"
            echo "Selected UTXO: $utxo_id" >&2
            break
        else
            echo "Invalid UTXO number, please try again." >&2
        fi
    done

    echo "Selected UTXO:" >&2
    echo "$selected_utxo" >&2

    echo "$tx_in_list"
}

select_collateral_utxo() {
    local container=$1
    local address=$2

    # echo "Querying UTXOs for address $address in container $container" >&2
    utxos=$(docker exec -i "$container" cardano-cli query utxo --socket-path /ipc/node.socket --address "$address" --$CARDANO_NETWORK_WITH_MAGIC)

    local collateral_utxo=""
    local tx_in_list=""

    echo "Selecting a UTXO with more than 5 ADA and no other tokens for collateral:" >&2

    # Prepare the list of UTXOs
    local utxo_list=()
    local count=1
    while IFS= read -r line; do
        if [ $count -gt 2 ]; then
            utxo_list+=("$line")
        fi
        count=$((count+1))
    done <<< "$utxos"

    # Find a suitable UTXO for collateral
    for utxo in "${utxo_list[@]}"; do
        local tx_hash=$(echo "$utxo" | awk '{print $1}')
        local tx_ix=$(echo "$utxo" | awk '{print $2}')
        local tx_amount=$(echo "$utxo" | awk '{print $3}')  # Get tx_amount directly
        local rest=$(echo "$utxo" | awk '{for (i=4; i<=NF; i++) printf $i " "; print ""}')

        # echo "tx_hash: $tx_hash" >&2
        # echo "tx_ix: $tx_ix" >&2
        # echo "tx_amount: $tx_amount" >&2
        # echo "rest: $rest" >&2
        
        if [[ $tx_amount -gt 4999999 ]]; then  # Check amount first
            # Improved token check
            if [[ ! "$rest" =~ [0-9]+\ [0-9a-fA-F]+\.[0-9a-zA-Z]+ ]]; then 
                local utxo_id="$tx_hash#$tx_ix"
                collateral_utxo="$utxo_id"
                echo "Selected UTXO for collateral: $utxo_id with $tx_amount lovelace" >&2
                break
            fi
        fi
    done

    if [[ -z "$collateral_utxo" ]]; then
        echo "No suitable UTXO found for collateral. Please ensure there is a UTXO with more than 5 ADA and no other tokens." >&2
        return 1
    fi

    echo "$collateral_utxo"
}


build_and_submit_transaction() {
    local container=$1
    local walletPath=$2
    local tx_in_list=$3
    local tx_out_list=$4
    local change_address=$5
    local mint_params=$6

    echo "Creating the transaction..."
    # echo  $tx_in_list $tx_out_list $mint_params

    ## PROTOCOL PARAMETERS FILE
    mkdir -p "$WORKSPACE_ROOT_DIR_ABSOLUTE/configs/cardano-node/$CARDANO_NETWORK"
    ppFile="$WORKSPACE_ROOT_DIR_ABSOLUTE/configs/cardano-node/$CARDANO_NETWORK/protocol-parameters.json"
    if [[ -f "$ppFile" ]]
    then
        # echo "Protocol File: $ppFile"
        docker cp -q "$ppFile" "$container:/tmp/protocol-parameters.json" 
        # echo "Existe... desea actualizarlo (y/n)?"
        # read -n 1 -s opcion
        # if [[ $opcion = "y" ]]; then 
        #     cardano-cli query protocol-parameters --out-file $ppFile --$CARDANO_NETWORK_WITH_MAGIC 
        # fi
    else
        # echo "Getting Protocol File: $ppFile..."
        docker exec -i "$container" cardano-cli query protocol-parameters --socket-path /ipc/node.socket --$CARDANO_NETWORK_WITH_MAGIC --out-file "/tmp/protocol-parameters.json"
        docker cp -q "$container:/tmp/protocol-parameters.json" "$ppFile"
    fi

    ## SLOT TIP
    tipSlot=$(docker exec -i "$container" cardano-cli query tip --socket-path /ipc/node.socket --$CARDANO_NETWORK_WITH_MAGIC | jq -r .slot)
    echo "tipSlot: $tipSlot"

    ## WALLET SIGNER
    wallet_skey_path="$walletPath/$(basename $walletPath).skey"
    docker cp -q "$wallet_skey_path" "$container:/tmp/wallet.skey"

    # wallet_vkey_path="$walletPath/$(basename $walletPath).vkey"
    # docker cp -q "$wallet_vkey_path" "$container:/tmp/wallet.vkey"
    # wallet_pkh=$(docker exec -i "$container" cardano-cli address key-hash --payment-verification-key-file "/tmp/wallet.vkey")
    wallet_pkh=$(cat "$walletPath/$(basename $walletPath).pkh")
    # echo "wallet_pkh:"  $wallet_pkh

    ## BUILD TRANSACTION

    # --required-signer-hash $wallet_pkh \
    # --required-signer="/tmp/wallet.skey" \
    # --protocol-params-file \"$ppFile\" \

    cmd="docker exec -i \"$container\" cardano-cli transaction build \
            --babbage-era \
            --socket-path /ipc/node.socket \
            --$CARDANO_NETWORK_WITH_MAGIC \
            $tx_in_list $tx_out_list $mint_params \
            --change-address \"$change_address\" \
            --required-signer-hash $wallet_pkh \
            --invalid-before ${tipSlot} \
            --out-file \"/tmp/tx.body\""


# cardano-cli transaction build  \
#     --babbage-era  \
#     --testnet-magic 1  \
#     --tx-in 4f909998b77f71d4a75eaa8ed5f800d1a97a419ecee70ed2179b1589ba409fc8#0   \
#     --tx-in-script-file CheckDateValidator/2024-08-08-02-21/paramCheckAfterDeadlineValidator.plutus  \
#     --tx-in-inline-datum-present  \
#     --tx-in-redeemer-file redeemer.json  \
#     --tx-in-collateral 1df0a401b4821cc9e4481ef697ec111e6ba7a54cc0509ce87afd14d1506c0af8#3  \
#     --required-signer-hash 336f16c12c359e9188d0006e4a914b0b011449dc376286360d0edecc  \
#     --change-address addr_test1qqek79kp9s6eayvg6qqxuj53fv9sz9zfmsmk9p3kp58danpj089g4nmyect2j4x4tatv08qsg09pkj84n0aj5lq6pkvs2cnthz
#     --invalid-before 1  \
#     --out-file tx.body

    # Initialize sw_debug with a default value, 1 ==  false
    sw_debug=${sw_debug:-1}
    
    if [ "$sw_debug" -eq 0 ]; then
        echo "Command: $cmd"
    fi

    if ! eval $cmd; then
        echo "Error: Failed to build the transaction. Check the input parameters and script." >&2
        return 0  # Indicate failure
    fi

    # docker cp -q "$container:/tmp/tx.body" "/tmp/tx.body" 
    # cat /tmp/tx.body

    ## SIGN TRANSACTION

    echo "Signing the transaction..."
   
    if ! docker exec -i "$container" cardano-cli transaction sign \
            --tx-body-file "/tmp/tx.body" \
            --signing-key-file "/tmp/wallet.skey" \
            --$CARDANO_NETWORK_WITH_MAGIC \
            --out-file "/tmp/tx.signed"; then
        echo "Error: Failed to sign the transaction. Check wallet and key permissions." >&2
        docker exec -i "$container" rm /tmp/wallet.skey  # Clean up
        return 0
    fi
    # docker exec -i "$container" rm /tmp/wallet.skey

    # docker cp -q "$container:/tmp/tx.signed" "/tmp/tx.signed" 
    # cat /tmp/tx.signed

    ## SUBMIT TRANSACTION

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


create_collateral_tx() {
    echo "Creating collateral transaction..."

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
    tx_out_list=" --tx-out $wallet_address+5000000 --tx-out $wallet_address+5000000 --tx-out $wallet_address+5000000 --tx-out $wallet_address+5000000" 

    build_and_submit_transaction "$selected_node_container" "$selected_wallet" "$tx_in_list" "$tx_out_list" "$wallet_address" 

    read -p "Press Enter to continue..."

}

create_minting_tx() {
    local redeemer="$1"
    echo "Creating minting transaction..."
    create_generic_minting_tx "mint" "$redeemer"
    read -p "Press Enter to continue..."
}

create_burning_tx() {
    local redeemer="$1"
    echo "Creating burning transaction..."
    create_generic_minting_tx "burn" "$redeemer"
    read -p "Press Enter to continue..."
}

create_generic_minting_tx() {
    local action="$1"
    local redeemer="$2"
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
            read -p "Enter amount of tokens to $action (default 1): " token_amount
            if [[ -z "$token_amount" ]]; then
                token_amount=1
            fi

            # Validate token amount (only positive numbers allowed)
            if ! [[ "$token_amount" =~ ^[0-9]+$ ]]; then
                echo "Invalid amount. Please enter a positive number."
            else
                # Set amount to negative for burning
                if [[ "$action" == "burn" ]]; then
                    token_amount=$(( -token_amount ))
                fi
                break
            fi
        done

        tokens+=("$token_name_hex")
        amounts+=("$token_amount")
    done

    # Handle redeemer if provided
    redeemer_json_file="/tmp/redeemer.json"
    if (declare -f sw_use_redeemer > /dev/null && sw_use_redeemer) || [[ -n "$redeemer" ]]; then
        generate_redeemer_json "$redeemer" "$redeemer_json_file"
        docker cp -q "$redeemer_json_file" "$selected_node_container:$redeemer_json_file"
        # echo "Redeemer - $redeemer - JSON: $(cat $redeemer_json_file)"
    fi

    tx_in_script_file="$selected_scripts/$selected_policy.plutus"
    docker cp -q "$tx_in_script_file" "$selected_node_container:/tmp/policy.plutus"
    pid=$(docker exec -i "$selected_node_container" cardano-cli transaction policyid --script-file /tmp/policy.plutus)

    mint_params=""
    mint_values=""
    for i in "${!tokens[@]}"; do
        token_name_hex="${tokens[$i]}"
        token_amount="${amounts[$i]}"
        mint_value="$token_amount $pid.$token_name_hex"
        mint_values+=" + $mint_value"
        if (declare -f sw_use_redeemer > /dev/null && sw_use_redeemer) || [[ -n "$redeemer" ]]; then
            mint_params+=" --mint \"$mint_value\" --mint-script-file /tmp/policy.plutus --mint-redeemer-file /tmp/redeemer.json"
        else
            mint_params+=" --mint \"$mint_value\" --mint-script-file /tmp/policy.plutus --mint-redeemer-value {}"
        fi
    done

    if [[ "$action" == "mint" ]]; then
        tx_out_list=" --tx-out \"$wallet_address + 2000000 lovelace + $mint_values\""
    fi
    
    build_and_submit_transaction "$selected_node_container" "$selected_wallet" "$tx_in_list" "$tx_out_list" "$wallet_address" "$mint_params"
}


create_vesting_tx() {
    local datum="$1"

    echo "Creating vesting transaction..."
    select_contract

    # Handle datum if provided
    datum_json_file="/tmp/datum.json"
    if (declare -f sw_use_datum > /dev/null && sw_use_datum) || [[ -n "$datum" ]]; then
        generate_datum_json "$datum" "$datum_json_file"
        docker cp -q "$datum_json_file" "$selected_node_container:$datum_json_file"
        # echo "Datum - $datum - JSON: $(cat $datum_json_file)"
    fi

    amount_ada=$(select_amount_ada)

    wallet_address=$(cat "$selected_wallet/$(basename $selected_wallet).addr")
    echo "Choosing UTXOs from wallet to use as inputs..."
    select_utxos_output=$(select_utxos "$selected_node_container" "$wallet_address" $amount_ada)
    IFS='|' read -r wallet_tx_in_list total_lovelace_in_list <<< "$select_utxos_output"

    tx_in_list=""
    if [[ -n "$wallet_tx_in_list" ]]; then
        IFS=' ' read -ra wallet_tx_ins <<< "$wallet_tx_in_list"
        for wallet_tx_in in "${wallet_tx_ins[@]}"; do
            tx_in_list+=" --tx-in $wallet_tx_in"
        done
    fi
    
    if (declare -f sw_use_datum > /dev/null && sw_use_datum) || [[ -n "$datum" ]]; then
        tx_out_list="--tx-out $script_address+$amount_ada --tx-out-inline-datum-file /tmp/datum.json"
    else
        tx_out_list="--tx-out $script_address+$amount_ada --tx-out-inline-datum-value {}"
    fi

    build_and_submit_transaction "$selected_node_container" "$selected_wallet" "$tx_in_list" "$tx_out_list" "$wallet_address"

    read -p "Press Enter to continue..."
}

create_claiming_tx() {
    local redeemer="$1"
    echo "Creating claiming transaction..."
    select_contract

    wallet_address=$(cat "$selected_wallet/$(basename $selected_wallet).addr")
    echo "Choosing UTXOs from wallet to use as inputs..."
    select_utxos_output=$(select_utxos "$selected_node_container" "$wallet_address" 1)
    IFS='|' read -r wallet_tx_in_list total_lovelace_in_list <<< "$select_utxos_output"

    echo "Choosing UTXOs from script address to consume..."
    select_utxos_output=$(select_utxos "$selected_node_container" "$script_address" 1)
    IFS='|' read -r script_tx_in_list total_lovelace_in_script <<< "$select_utxos_output"
    
    tx_in_script_file="$selected_scripts/$selected_validator.plutus"
    docker cp -q "$tx_in_script_file" "$selected_node_container:/tmp/validator.plutus"

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

    # Handle redeemer if provided
    redeemer_json_file="/tmp/redeemer.json"
    if (declare -f sw_use_redeemer > /dev/null && sw_use_redeemer) || [[ -n "$redeemer" ]]; then
        generate_redeemer_json "$redeemer" "$redeemer_json_file"
        docker cp -q "$redeemer_json_file" "$selected_node_container:$redeemer_json_file"
        # echo "Redeemer - $redeemer - JSON: $(cat $redeemer_json_file)"
    fi

    # Handle script tx-in list
    if [[ -n "$script_tx_in_list" ]]; then
        IFS=' ' read -ra script_tx_ins <<< "$script_tx_in_list"
        for script_tx_in in "${script_tx_ins[@]}"; do
            if (declare -f sw_use_redeemer > /dev/null && sw_use_redeemer) || [[ -n "$redeemer" ]]; then
                tx_in_list+=" --tx-in $script_tx_in --tx-in-script-file /tmp/validator.plutus --tx-in-inline-datum-present --tx-in-redeemer-file /tmp/redeemer.json $tx_in_collateral"
            else
                tx_in_list+=" --tx-in $script_tx_in --tx-in-script-file /tmp/validator.plutus --tx-in-inline-datum-present --tx-in-redeemer-value {} $tx_in_collateral"
            fi
        done

        tx_out_list=" --tx-out \"$wallet_address + $total_lovelace_in_script\""
    
    fi
    
    build_and_submit_transaction "$selected_node_container" "$selected_wallet" "$tx_in_list" "$tx_out_list" "$wallet_address"
    
    read -p "Press Enter to continue..."

}