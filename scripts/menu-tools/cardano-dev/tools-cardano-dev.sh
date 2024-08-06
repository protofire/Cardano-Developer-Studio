#!/bin/bash

source "$(dirname "${BASH_SOURCE[0]}")/../../utils/utils.sh"

setWorkspaceDir

# Specific function for container selection
select_dev_container() {
    if [[ -z "$INSIDE_DEV_CONTAINER" ]]; then
        if ! select_container 'cardano-dev'; then
            return 1
        fi
    else
        selected_container='cardano-dev'
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
            # TODO: add all the rest of the CLI
            echo "3) Run Validators/CheckDate/CheckDateValidator CLI"
            echo "4) Run Policys/RedeemerNFT CLI"
            echo "5) Docker Logs"
            echo "6) Delete this Container and Optionally Its Volumes"
            echo "0) Return Main Menu"
            read -p "Enter your choice or 0 to exit: " tool_choice
            
            case $tool_choice in
                1)
                    if [[ -z "$INSIDE_DEV_CONTAINER" ]]; then
                        docker exec -it "$selected_container" cabal build all
                    else
                        cabal build all
                    fi
                    read -p "Press Enter to continue..."
                ;;
                2)
                    if [[ -z "$INSIDE_DEV_CONTAINER" ]]; then
                        docker exec -it "$selected_container" cabal test all
                    else
                        cabal test all
                    fi
                    read -p "Press Enter to continue..."
                ;;

                3)
                    if [[ -z "$INSIDE_DEV_CONTAINER" ]]; then
                        docker exec -it "$selected_container" bash -c "export INSIDE_DEV_CONTAINER=1 && cd ~/workspace && bash ./Validators/CheckDate/scripts/cli.sh"
                    else
                        bash "$WORKSPACE_ROOT_DIR_ABSOLUTE/Validators/CheckDate/scripts/cli.sh"
                    fi
                    
                    read -p "Press Enter to continue..."
                ;;
                4)
                    if [[ -z "$INSIDE_DEV_CONTAINER" ]]; then
                        docker exec -it "$selected_container" bash -c "export INSIDE_DEV_CONTAINER=1 && cd ~/workspace && bash ./Policys/RedeemerNFT/scripts/cli.sh"
                    else
                        bash "$WORKSPACE_ROOT_DIR_ABSOLUTE/Policys/RedeemerNFT/scripts/cli.sh"
                    fi
                    
                    read -p "Press Enter to continue..."
                ;;
                5)
                    monitor_logs "$selected_container"
                    read -p "Press Enter to continue..."
                ;;
                
                6)
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