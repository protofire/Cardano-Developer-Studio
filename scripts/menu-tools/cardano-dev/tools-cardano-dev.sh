#!/bin/bash

source "$(dirname "${BASH_SOURCE[0]}")/../../utils/utils.sh"


setWorkspaceDir

# Specific function for container selection
select_dev_container() {
    if ! select_container 'cardano-dev'; then
        return 1
    fi
}

# Function to interact with the Cardano Dev container
cardano_dev_tools() {
    while select_dev_container; do
        while true; do
            echo "----"
            echo "MENU - Selected container: $selected_container"
            echo "----"
            echo "1) Cabal Build All"
            echo "2) Cabal Test All"
            echo "3) Run example CLI"
            echo "4) Docker Logs"
            echo "5) Delete this Container and Optionally Its Volumes"
            echo "0) Return Main Menu"
            read -p "Enter your choice or 0 to exit: " tool_choice
            
            case $tool_choice in
                1)
                    docker exec -it "$selected_container" cabal build all
                    read -p "Press Enter to continue..."
                ;;
                
                2)
                    docker exec -it "$selected_container" cabal test all
                    read -p "Press Enter to continue..."
                ;;

                3)
                    # source "$WORKSPACE_ROOT_DIR_ABSOLUTE/examples/Validators/CheckDate/scripts/cli.sh"
                    docker exec -it "$selected_container" bash -c "cd ~/workspace && bash ./Validators/CheckDate/scripts/cli.sh"
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