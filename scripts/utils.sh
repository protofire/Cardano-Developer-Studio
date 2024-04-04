#!/bin/bash

set -e

check_bash_version() {
    local MIN_BASH_VERSION="$1" # Define the minimum required version of Bash
    local bash_version="${BASH_VERSION%%(*}" # Extract the version number before the parenthesis
    
    # Now use the compare_versions function to compare the extracted version with the minimum required version
    if compare_versions "$bash_version" "$MIN_BASH_VERSION"; then
        echo "Bash version $bash_version is sufficient."
    else
        echo "Bash version $bash_version is too old. Please upgrade to Bash version $MIN_BASH_VERSION or newer."
        return 1 # Return failure
    fi
}

check_docker_version() {
    local MIN_DOCKER_VERSION="$1" # Specify the minimum Docker version required
    if ! command -v docker &> /dev/null; then
        echo "Docker is not installed."
        return 1 # Return failure
    fi

    # Extract the version string. This adjustment accounts for various version formats.
    local version
    version=$(docker --version | grep -oP 'Docker version \K[^,]+' | cut -d'-' -f1) # Gets '19.03.12' for example

    # Now use the compare_versions function to check if the installed version meets the requirement
    if compare_versions $version $MIN_DOCKER_VERSION; then
        echo "Docker version $version is sufficient."
    else
        echo "Docker version $version is too old. Please upgrade to Docker version $MIN_DOCKER_VERSION or newer."
        return 1 # Return failure
    fi
}

check_docker_compose_version() { 
    local MIN_COMPOSE_VERSION="$1"
    if ! command -v docker-compose &> /dev/null; then
        echo "Docker Compose is not installed."
        return 1 # Return failure
    fi

    # Extract the version string properly
    local version
    version=$(docker-compose --version | grep -oP 'version \K[^,]+' | cut -d'-' -f1 | sed 's/^v//') # Removes 'v' at the start

    if compare_versions $version $MIN_COMPOSE_VERSION; then
        echo "Docker Compose version $version is sufficient."
    else
        echo "Docker Compose version $version is too old. Please upgrade to Docker Compose version $MIN_COMPOSE_VERSION or newer."
        return 1 # Return failure
    fi
}

compare_versions() {
    local version_a=(${1//./ }) # Split version by '.' into array
    local version_b=(${2//./ })
    
    # Compare each part of the version numbers
    for ((i=0; i<${#version_a[@]}; i++)); do
        if [[ -z ${version_b[i]} ]]; then
            # If b is shorter than a, and we're already here, a is greater
            return 0
        fi
        if ((10#${version_a[i]} > 10#${version_b[i]})); then
            return 0
        elif ((10#${version_a[i]} < 10#${version_b[i]})); then
            return 1
        fi
    done
    
    # If we're here, the versions are equal or b is longer than a
    if [[ ${#version_a[@]} -lt ${#version_b[@]} ]]; then
        return 1
    fi
    
    return 0
}   

check_package_manager() {
    if command -v brew &> /dev/null; then
        echo "Homebrew is installed."
    elif command -v apt-get &> /dev/null; then
        echo "apt-get is available."
    elif command -v dnf &> /dev/null; then
        echo "dnf is available."
    elif command -v pacman &> /dev/null; then
        echo "pacman is available."
    else
        echo "No recognized package manager found. Please ensure you have a package manager such as brew, apt-get, dnf, or pacman."
        return 1 # Return failure
    fi
}


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


check_node_resources() {
    echo "----"
    echo "Setting up Cardano Node environment..."
    echo "----"
    
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
    
    echo "----"
    
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
    if ! docker volume ls | grep -q "cardano-node-ipc-${CARDANO_NODE_VERSION:-"8.9.0"}-${CARDANO_NETWORK:-mainnet}"; then
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
        read -p "Press Enter to continue..."
        return 1  # Return with error status to signal no containers found
    fi
    
    while true; do
        
        echo "----"
        echo "Available $container_type Containers (0 to Return Main Menu):"
        for i in "${!containers[@]}"; do
            echo "$((i+1))) ${containers[i]}"
        done
        read -p "Select a container [1-${#containers[@]}] or 0 to Return Main Menu: " container_choice
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



# Function to delete container and ask about deleting its volumes
delete_container_and_optionally_volumes() {
    local container_name="$1"
    
    read -p "Do you  want to delete the container ($container_name) and its volumes? [y/N]: " confirm_delete
    if [[ "$confirm_delete" =~ ^[Yy]$ ]]; then
        
        echo "Preparing to delete container: $container_name"
        
        # Get all volumes attached to the container
        local volumes=$(docker inspect --format='{{range .Mounts}}{{.Name}}{{"\n"}}{{end}}' "$container_name")
        
        # Stop and remove the container
        docker stop "$container_name"
        docker rm "$container_name"
        echo "Container $container_name has been removed."
        
        # Ask about deleting each volume if it is not used by another container
        for volume in $volumes; do
            delete_volume_if_unused "$volume"
        done
        
    fi
}

delete_dbsync_container_and_associated_postgres() {
    local dbsync_container="$1"
    
    # Extract the POSTGRES_HOST environment variable value from the Cardano DB Sync container
    local POSTGRES_VERSION=$(docker inspect --format='{{range .Config.Env}}{{println .}}{{end}}' "$dbsync_container" | grep POSTGRES_VERSION= | cut -d'=' -f2)
    local CARDANO_NODE_VERSION=$(docker inspect --format='{{range .Config.Env}}{{println .}}{{end}}' "$dbsync_container" | grep CARDANO_NODE_VERSION= | cut -d'=' -f2)
    local CARDANO_NETWORK=$(docker inspect --format='{{range .Config.Env}}{{println .}}{{end}}' "$dbsync_container" | grep CARDANO_NETWORK= | cut -d'=' -f2)
    
    local postgres_container="postgres-container-${POSTGRES_VERSION:-"14.10-alpine"}-${CARDANO_NODE_VERSION:-"8.9.0"}-${CARDANO_NETWORK:-mainnet}"
    
    echo "Cardano DB Sync container: $dbsync_container"
    echo "PostgreSQL container: $postgres_container"
    
    delete_container_and_optionally_volumes "$dbsync_container"
    delete_container_and_optionally_volumes "$postgres_container"
}

delete_wallet_container_and_associated_icarus() {
    local wallet_container="$1"
    
    # Extract the POSTGRES_HOST environment variable value from the Cardano DB Sync container
    local ICARUS_VERSION=$(docker inspect --format='{{range .Config.Env}}{{println .}}{{end}}' "$wallet_container" | grep ICARUS_VERSION= | cut -d'=' -f2)
    local CARDANO_NODE_VERSION=$(docker inspect --format='{{range .Config.Env}}{{println .}}{{end}}' "$wallet_container" | grep CARDANO_NODE_VERSION= | cut -d'=' -f2)
    local CARDANO_NETWORK=$(docker inspect --format='{{range .Config.Env}}{{println .}}{{end}}' "$wallet_container" | grep CARDANO_NETWORK= | cut -d'=' -f2)
    
    local icarus_container="icarus-container-${ICARUS_VERSION:-v2023-04-14}-${CARDANO_NODE_VERSION:-"8.9.0"}-${CARDANO_NETWORK:-mainnet}"
    
    echo "Cardano Wallet container: $wallet_container"
    echo "Icarus container: $icarus_container"
    
    delete_container_and_optionally_volumes "$wallet_container"
    delete_container_and_optionally_volumes "$icarus_container"
}


# Function to safely delete volumes if not used by any other container
delete_volume_if_unused() {
    local volume_name="$1"
    # Check if the volume is attached to any running container
    local attached_containers=$(docker ps -q --filter "volume=$volume_name")
    if [[ -z "$attached_containers" ]]; then
        read -p "Do you want to delete the unused volume '$volume_name'? [y/N]: " confirm_volume_delete
        if [[ "$confirm_volume_delete" =~ ^[Yy]$ ]]; then
            echo "Deleting volume '$volume_name'..."
            docker volume rm "$volume_name"
            echo "Volume '$volume_name' deleted."
        else
            echo "Volume '$volume_name' is kept."
        fi
    else
        echo "Volume '$volume_name' is in use by other containers and will not be deleted."
    fi
}

force_delete_docker_volume() {
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
