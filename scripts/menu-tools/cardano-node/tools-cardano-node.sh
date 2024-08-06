#!/bin/bash

source "$(dirname "${BASH_SOURCE[0]}")/../../utils/utils.sh"

setWorkspaceDir

# Specific function for node container selection
select_node_container() {
    if ! select_container 'cardano-node-container'; then
        return 1  # Return 1 to signal to break out of the loop
    fi
}

# Function to interact with the Cardano node container
cardano_node_tools() {
    while select_node_container; do
        while true; do
            echo "----"
            echo "MENU - Selected container: $selected_container"
            echo "----"
            echo "1) Cardano Node Version"
            echo "2) Run Cardano Node"
            echo "3) Cardano CLI Query Tip"
            echo "4) Docker Logs"
            echo "5) Delete this Container and Optionally Its Volumes"
            echo "0) Return Main Menu"
            read -p "Enter your choice or 0 to exit: " tool_choice
            
            case $tool_choice in
                1)
                    docker exec -it "$selected_container" cardano-node --version
                    read -p "Press Enter to continue..."
                ;;
                
                2)
                    # Check if node.socket file exists in the container
                    if docker exec "$selected_container" test -e /ipc/node.socket; then
                        echo "Error: node.socket does exist in $selected_container. The node is already running."
                    else
                        # File does not exist,
                        echo "Initializing Cardano node in $selected_container..."
                        docker exec -d  "$selected_container" bash init-cardano-node.sh
                        echo "Cardano node initialization started in the background."
                    fi
                    read -p "Press Enter to continue..."
                ;;
                
                3)
                    # Check if node.socket file exists in the container
                    if docker exec "$selected_container" test -e /ipc/node.socket; then
                        # Fetch environment variable and run the cardano-cli query command
                        CARDANO_NETWORK_WITH_MAGIC=$(docker exec -it "$selected_container" printenv CARDANO_NETWORK_WITH_MAGIC | tr -d '\r')
                        docker exec -it "$selected_container" cardano-cli query tip --socket-path /ipc/node.socket --$CARDANO_NETWORK_WITH_MAGIC
                    else
                        # File does not exist, print error and suggestion to start the node
                        echo "Error: node.socket does not exist in $selected_container. Please start the node first."
                    fi
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