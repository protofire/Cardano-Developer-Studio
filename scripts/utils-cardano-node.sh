#!/bin/bash

source "$(dirname "${BASH_SOURCE[0]}")/utils.sh"


# Specific function for node container selection
select_node_container() {
    if ! select_container 'cardano-node-container'; then
        return 1
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
            echo "2) Cardano CLI Query Tip"
            echo "3) Exit to container selection"
            read -p "Enter your choice or 3 to exit: " tool_choice
            
            case $tool_choice in
                1)
                    docker exec -it "$selected_container" cardano-node --version
                    ;;
                2)
                    CARDANO_NETWORK_WITH_MAGIC=$(docker exec -it "$selected_container" printenv CARDANO_NETWORK_WITH_MAGIC | tr -d '\r')
                    docker exec -it "$selected_container" cardano-cli query tip --socket-path /ipc/node.socket --$CARDANO_NETWORK_WITH_MAGIC
                    ;;
                3)
                    break  # Breaks out of both the inner loop and the container selection loop
                    ;;
                *)
                    echo "Invalid choice, please select a valid option."
                    ;;
            esac
            read -p "Press Enter to continue..."
        done
    done
}