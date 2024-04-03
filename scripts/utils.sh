#!/bin/bash

install_package() {
    local package_name="$1"
    echo "Checking if $package_name is installed..."
    
    if ! command -v "$package_name" &> /dev/null; then
        echo "$package_name is not installed. Attempting to install $package_name..."
        # Detect package manager and install the package
        if command -v apt-get &> /dev/null; then
            echo "Using apt-get to install $package_name..."
            sudo apt-get update && sudo apt-get install -y "$package_name"
            elif command -v dnf &> /dev/null; then
            echo "Using dnf to install $package_name..."
            sudo dnf install -y "$package_name"
            elif command -v brew &> /dev/null; then
            echo "Using brew to install $package_name..."
            brew install "$package_name"
            elif command -v pacman &> /dev/null; then
            echo "Using pacman to install $package_name..."
            sudo pacman -Syu "$package_name" --noconfirm
        else
            echo "Package manager not detected. Please install $package_name manually."
            return 1 # Return failure
        fi
    else
        echo "$package_name is already installed."
    fi
}

verify_snapshot_integrity() {
    local snapshot_path="$1"
    echo "Verifying integrity of the snapshot: $snapshot_path..."
    if lz4 -t "$snapshot_path" &>/dev/null; then
        echo "Snapshot verification successful."
        return 0 # Return success
    else
        echo "Snapshot verification failed."
        return 1 # Return failure
    fi
}

determine_network_with_magic() {
    local cardano_network="$1"  # CARDANO_NETWORK as the first argument
    local config_dir_absolute="$2"  # CONFIG_DIR_ABSOLUTE as the second argument
    
    local cardano_shelley="$config_dir_absolute/cardano-node/$cardano_network/shelley-genesis.json"
    
    if [[ -f "$cardano_shelley" ]]; then
        if [[ "$cardano_network" == "mainnet" ]]; then
            CARDANO_NETWORK_WITH_MAGIC="mainnet"
        else
            local network_magic=$(jq -r '.networkMagic' "$cardano_shelley")
            CARDANO_NETWORK_WITH_MAGIC="testnet-magic $network_magic"
        fi
        echo "Network with magic: $CARDANO_NETWORK_WITH_MAGIC"
        export CARDANO_NETWORK_WITH_MAGIC
    else
        echo "Shelley genesis file not found at $cardano_shelley"
        exit 1
    fi
}


delete_docker_volume() {
    local volume_name=$1  # The name of the volume to delete
    
    echo "Checking if the Docker volume '$volume_name' exists..."
    volume_exists=$(docker volume ls -q | grep "$volume_name" || true)
    if [ ! -z "$volume_exists" ]; then
        echo "A Docker volume for '$volume_name' exists."
        read -p "Do you want to delete the existing volume? This will result in data loss. [y/N]: " confirm_action
        if [[ "$confirm_action" =~ ^[Yy]$ ]]; then
            echo "Identifying any containers using the volume '$volume_name'..."
            containers_using_volume=$(docker ps -a --filter volume="$volume_name" -q)
            if [ ! -z "$containers_using_volume" ]; then
                echo "Stopping and removing containers that use the volume..."
                docker stop $containers_using_volume
                docker rm $containers_using_volume
            fi
            
            echo "Deleting the Docker volume '$volume_name'..."
            docker volume rm "$volume_name"
            echo "The volume '$volume_name' has been deleted. A new database/data will be created upon starting the Docker Compose setup."
        else
            echo "Proceeding without deleting the Docker volume. Ensure the existing database/data matches your configuration."
        fi
        # else
        #     echo "No volume named '$volume_name' found. Skipping deletion."
    fi
}

check_node_resources() {
    echo "Checking for Cardano Node resources..."
    
    read -p "Enter CARDANO_NODE_VERSION [default: 8.9.0]: " CARDANO_NODE_VERSION
    CARDANO_NODE_VERSION=${CARDANO_NODE_VERSION:-"8.9.0"}
    export CARDANO_NODE_VERSION
    
    while :; do
        read -p "Enter CARDANO_NETWORK [options: preprod, mainnet] (default: preprod): " CARDANO_NETWORK
        CARDANO_NETWORK=${CARDANO_NETWORK:-preprod}
        
        if [[ "$CARDANO_NETWORK" == "preprod" || "$CARDANO_NETWORK" == "mainnet" ]]; then
            export CARDANO_NETWORK
            break
        else
            echo "Invalid network. Please enter 'preprod' or 'mainnet'."
        fi
    done
    
    if ! docker ps | grep -q "cardano-node-container-${CARDANO_NODE_VERSION:-"8.9.0"}-${CARDANO_NETWORK:-mainnet}"; then
        echo "Cardano node container is not running. Please start the Cardano Node Version: ${CARDANO_NODE_VERSION:-"8.9.0"} and Network: ${CARDANO_NETWORK:-mainnet} first."
        exit 1
    fi
    
    # Check if the cardano-network is available
    if ! docker network ls | grep -q "cardano-node-network-${CARDANO_NODE_VERSION:-"8.9.0"}-${CARDANO_NETWORK:-mainnet}"; then
        echo "Cardano network not found. Please ensure the Cardano Node Version: ${CARDANO_NODE_VERSION:-"8.9.0"} and Network: ${CARDANO_NETWORK:-mainnet} is set up."
        exit 1
    fi
    
    # Check if the node-ipc volume exists
    if ! docker volume ls | grep -q "cardano-node-node-ipc-${CARDANO_NODE_VERSION:-"8.9.0"}-${CARDANO_NETWORK:-mainnet}"; then
        echo "node-ipc volume not found. Please ensure the Cardano Node Version: ${CARDANO_NODE_VERSION:-"8.9.0"} and Network: ${CARDANO_NETWORK:-mainnet} is set up."
        exit 1
    fi
    
    echo "Cardano Node resources verified."
}


# Centralized function to fetch and select a container
select_container() {
    local container_type="$1"  # e.g., 'cardano-wallet-container'
    
    echo "----"
    echo "Fetching list of $container_type containers..."
    containers=()
    while IFS= read -r line; do
        containers+=("$line")
    done < <(docker ps --format "{{.Names}}" | grep "$container_type")
    
    if [ ${#containers[@]} -eq 0 ]; then
        echo "No $container_type containers found. Please ensure they are running."
        return 1  # Return with error status to signal no containers found
    fi
    
    while true; do
        
        echo "----"
        echo "Available $container_type Containers (0 to exit):"
        for i in "${!containers[@]}"; do
            echo "$((i+1))) ${containers[i]}"
        done
        read -p "Select a container [1-${#containers[@]}] or 0 to exit: " container_choice
        echo "----"
        if [ "$container_choice" -eq 0 ]; then
            return 1  # Return with error status to signal user requested exit
            elif [[ "$container_choice" =~ ^[0-9]+$ ]] && [ "$container_choice" -ge 1 ] && [ "$container_choice" -le ${#containers[@]} ]; then
            selected_container="${containers[$container_choice-1]}"
            echo "Selected container: $selected_container"
            return 0  # Return success status
        else
            echo "Invalid selection. Please try again."
            read -p "Press Enter to continue..."
        fi
        
    done
}