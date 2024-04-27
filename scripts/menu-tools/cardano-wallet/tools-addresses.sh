#!/bin/bash




pkhToAddress() {
    local container=$1
    
    CARDANO_NETWORK=$(docker exec -it "$container" printenv CARDANO_NETWORK | tr -d '\r')
    if [[ "$CARDANO_NETWORK" == "preprod" || "$CARDANO_NETWORK" == "preview" ]]; then
        local network=0 # Use 0 for testnet, 1 for mainnet
    else
        local network=1 # Use 0 for testnet, 1 for mainnet
    fi
    
    echo "CARDANO_NETWORK: $CARDANO_NETWORK"
    
    # Define paths
    EXECUTABLE="${WORKSPACE_ROOT_DIR_ABSOLUTE}/bin/cardano-address-tools"
    
    # Check if the executable exists
    if [[ ! -f "$EXECUTABLE" ]]; then
        echo "Executable not found: $EXECUTABLE"
        exit 1
    fi
    
    read -p "Enter Wallet Payment Public Key Hash [default: abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e]: " payment_key
    payment_key=${payment_key:-abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e}
    read -p "Enter Wallet Stake Public Key Hash: " stake_key
    
    # Check if stake key is present
    if [[ -z "$stake_key" ]]; then
        # Stake key is absent, pass only the payment key
        address=$("$EXECUTABLE" $network "$payment_key")
    else
        # Stake key is present, pass both keys
        address=$("$EXECUTABLE" $network "$payment_key" "$stake_key")
    fi
    
    # Check for empty addresses or errors
    if [[ -z "$address" ]]; then
        echo "Error generating address for $payment_key, $stake_key"
        address=""  # Set address to empty to indicate an error in the output
    fi
    
    echo "Address: $address"
    
}


addresses_tools() {
    
    local container=$1
    
    while true; do
        
        echo "----"
        echo "MENU - Selected container: $selected_container - Addresses Tools"
        echo "----"
        echo "1) PKH To Address"
        echo "0) Return Main Menu"
        read -p "Enter your choice or 0 to exit: " tool_choice
        echo "----"
        
        case $tool_choice in
            1) pkhToAddress "$selected_container"
                read -p "Press Enter to continue..."
            ;;
            0) break 1 # Breaks out of both the inner loop and the container selection loop
            ;;
            *)
                echo "Invalid choice, please select a valid option."
                read -p "Press Enter to continue..."
                
            ;;
        esac
    done
}
