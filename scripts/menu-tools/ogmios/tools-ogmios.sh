#!/bin/bash

source "$(dirname "${BASH_SOURCE[0]}")/../../utils/utils.sh"

setWorkspaceDir

# Specific function for container selection
select_ogmios_container() {
    if ! select_container 'ogmios'; then
        return 1
    fi
}


health_check() {
    local container=$1

    echo "Fetching..."

    OGMIOS_PORT=$(docker exec -it "$container" printenv OGMIOS_PORT | tr -d '\r')
    echo "Container: ${container}"
    echo "OGMIOS_PORT: ${OGMIOS_PORT}"
    if [ -f /.dockerenv ]; then
        # echo "Running inside a Docker container."
        BASE_URL="http://host.docker.internal:${OGMIOS_PORT}"
    else
        # echo "Running on the host."
        BASE_URL="http://localhost:${OGMIOS_PORT}"
    fi
    response=$(curl -s "${BASE_URL}/health")
    
    echo "$response"
}

query_network_block_height() {
    local container=$1
    echo "Fetching..."
    OGMIOS_PORT=$(docker exec -it "$container" printenv OGMIOS_PORT | tr -d '\r')
    echo "Container: ${container}"
    echo "OGMIOS_PORT: ${OGMIOS_PORT}"

    if [ -f /.dockerenv ]; then
        # echo "Running inside a Docker container."
        BASE_URL="http://host.docker.internal:${OGMIOS_PORT}"
    else
        # echo "Running on the host."
        BASE_URL="http://localhost:${OGMIOS_PORT}"
    fi

    response=$(curl -s -X POST -H "Content-Type: application/json" -d '{"jsonrpc":"2.0","method":"queryNetwork/blockHeight","id":null}' "${BASE_URL}")
    echo "$response"
}

# Function to interact with the Cardano node container
cardano_ogmios_tools() {
    while select_ogmios_container; do
        while true; do
            echo "----"
            echo "MENU - Selected container: $selected_container"
            echo "----"
            echo "1) Ogmios Version"
            echo "2) Health check"
            echo "3) Network/BlockHeight"
            echo "4) Docker Logs"
            echo "5) Delete this Container and Optionally Its Volumes"
            echo "0) Return Main Menu"
            read -p "Enter your choice or 0 to exit: " tool_choice
            
            case $tool_choice in
                1)
                    docker exec -it "$selected_container" ogmios --version
                    read -p "Press Enter to continue..."
                ;;
                
                2)
                    health_check "$selected_container"
                    read -p "Press Enter to continue..."
                ;;

                3)
                    query_network_block_height "$selected_container"
                    read -p "Press Enter to continue..."
                ;;
                
                3)
                    monitor_logs "$selected_container"
                    read -p "Press Enter to continue..."
                ;;
                
                4)
                    delete_container_and_optionally_volumes "$selected_container"
                    break 2 # Breaks out of the current loop and the container selection loop
                    read -p "Press Enter to continue..."
                ;;
                0)
                    break 2 # Breaks out of the current loop and the container selection loop
                ;;
                *)
                    echo "Invalid choice, please select a valid option."
                    read -p "Press Enter to continue..."
                ;;
            esac
        done
    done
}