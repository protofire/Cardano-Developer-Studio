#!/bin/bash

source "$(dirname "${BASH_SOURCE[0]}")/../../utils/utils.sh"

setWorkspaceDir

# Specific function for container selection
select_kupo_container() {
    if ! select_container 'kupo'; then
        return 1
    fi
}

# Function to list all patterns
show_patterns() {
    local container=$1
    echo "Listing all Patterns..."
    KUPO_PORT=$(docker exec -it "$container" printenv KUPO_PORT | tr -d '\r')
    echo "Container: ${container}"
    echo "KUPO_PORT: ${KUPO_PORT}"
    if [ -f /.dockerenv ]; then
        # echo "Running inside a Docker container."
        BASE_URL="http://host.docker.internal:${KUPO_PORT}"
    else
        # echo "Running on the host."
        BASE_URL="http://localhost:${KUPO_PORT}"
    fi
    response=$(curl -s "${BASE_URL}/patterns")
    echo "Patterns: $response"
}


# Function to list all matches
show_matches() {
    local container=$1
    echo "Listing all Matches..."
    KUPO_PORT=$(docker exec -it "$container" printenv KUPO_PORT | tr -d '\r')
    echo "Container: ${container}"
    echo "KUPO_PORT: ${KUPO_PORT}"
    if [ -f /.dockerenv ]; then
        # echo "Running inside a Docker container."
        BASE_URL="http://host.docker.internal:${KUPO_PORT}"
    else
        # echo "Running on the host."
        BASE_URL="http://localhost:${KUPO_PORT}"
    fi
    # response=$(curl -s "${BASE_URL}/matches/*/*?unspent")
    response=$(curl -s "${BASE_URL}/matches")
    
    echo "Matches: $response"
}


# Function to interact with the Cardano node container
cardano_kupo_tools() {
    while select_kupo_container; do
        while true; do
            echo "----"
            echo "MENU - Selected container: $selected_container"
            echo "----"
            echo "1) Kupo Version"
            echo "2) Show Patterns"
            echo "3) Show Matches"
            echo "4) Docker Logs"
            echo "5) Delete this Container and Optionally Its Volumes"
            echo "0) Return Main Menu"
            read -p "Enter your choice or 0 to exit: " tool_choice
            
            case $tool_choice in
                1)
                    docker exec -it "$selected_container" kupo --version
                    read -p "Press Enter to continue..."
                ;;
                2)
                    show_patterns "$selected_container"
                    read -p "Press Enter to continue..."
                ;;
                3)
                    show_matches "$selected_container"
                    read -p "Press Enter to continue..."
                ;;
                4)
                    monitor_logs "$selected_container"
                    read -p "Press Enter to continue..."
                ;;
                5)
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