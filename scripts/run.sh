#!/bin/bash

# Exit on any error
set -e

# Determine the directory where run.sh resides
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

# Change to the script's directory
cd "$SCRIPT_DIR"

# Function to set execute permissions on necessary scripts
set_script_permissions() {
    # Ensure compose.sh is executable
    chmod +x "$SCRIPT_DIR/compose.sh"
}

# Declare a global variable for main_choice
declare -g main_choice

# Function to display the main menu and capture the choice
show_main_menu() {
    echo "Main Menu - Choose an option:"
    echo "1) Docker Compose Workflow"
    echo "2) Other Tool [Placeholder]"
    echo "3) Exit"
    read -p "Enter choice [1-3]: " main_choice
}

# Function to handle Docker Compose Workflow
docker_compose_workflow() {
    # Ensure scripts have execute permissions
    set_script_permissions

    # Use SCRIPT_DIR to reference compose.sh relative to run.sh's location
    "$SCRIPT_DIR/compose.sh"
}

# Main script logic
while true; do
    show_main_menu

    case $main_choice in
        1)
            docker_compose_workflow
            ;;
        2)
            # Placeholder for other tools
            echo "Other tools (Placeholder)"
            ;;
        3)
            echo "Exiting."
            exit 0
            ;;
        *)
            echo "Invalid choice, please select a valid option."
            ;;
    esac
done
