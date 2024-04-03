#!/bin/bash

source "$(dirname "${BASH_SOURCE[0]}")/utils.sh"

# Function to select a Cardano DB Sync container and navigate between options
select_dbsync_container() {
    if ! select_container 'cardano-dbsync-container'; then
        return 1  # Propagate the selection failure
    fi
}

# Executes a given SQL query using psql
execute_query() {
    local container=$1
    local query="$2"
    # Adjust the command to use the selected container and correct Postgres credentials
    POSTGRES_DB=$(docker exec "$container" printenv POSTGRES_DB | tr -d '\r')
    POSTGRES_PORT=$(docker exec "$container" printenv POSTGRES_PORT | tr -d '\r')
    POSTGRES_USER=$(docker exec "$container" printenv POSTGRES_USER | tr -d '\r')
    POSTGRES_PASSWORD=$(docker exec "$container" printenv POSTGRES_PASSWORD | tr -d '\r')
    POSTGRES_HOST=$(docker exec "$container" printenv POSTGRES_HOST | tr -d '\r')  # Assuming this is set to 'postgres' in your Docker Compose
    
    # Execute the query using psql. Note the use of the POSTGRES_HOST environment variable for the host.
    # Using 'docker exec' without -it since this is an automated script and we don't need interactive TTY
    docker exec "$container" psql -h "$POSTGRES_HOST" -p "$POSTGRES_PORT" -U "$POSTGRES_USER" -d "$POSTGRES_DB" -c "$query"
}

# DB Sync tools menu with container selection
cardano_dbsync_tools() {
    while select_dbsync_container; do
        while true; do
            echo "----"
            echo "MENU - Selected container: $selected_container"
            echo "----"
            echo "1) Sync progress of db-sync"
            echo "2) Retrieving most recent block"
            echo "3) Slot number of the most recent block"
            echo "4) Current total on-chain supply of Ada"
            echo "5) Delete this Container and Optionally Its Volumes"
            echo "6) Return Main Menu"
            read -p "Enter your choice or 6 to exit: " tool_choice
            
            case $tool_choice in
                1)
                    query="SELECT 100 * (EXTRACT(EPOCH FROM (MAX(time) AT TIME ZONE 'UTC')) - EXTRACT(EPOCH FROM (MIN(time) AT TIME ZONE 'UTC'))) / (EXTRACT(EPOCH FROM (NOW() AT TIME ZONE 'UTC')) - EXTRACT(EPOCH FROM (MIN(time) AT TIME ZONE 'UTC'))) AS sync_percent FROM block;"
                    execute_query "$selected_container" "$query"
                    read -p "Press Enter to continue..."
                ;;
                2)
                    query="SELECT * FROM block ORDER BY time DESC LIMIT 1;"
                    execute_query "$selected_container" "$query"
                    read -p "Press Enter to continue..."
                ;;
                3)
                    query="SELECT slot_no FROM block WHERE block_no IS NOT NULL ORDER BY block_no DESC LIMIT 1;"
                    execute_query "$selected_container" "$query"
                    read -p "Press Enter to continue..."
                ;;
                4)
                    query="SELECT SUM(value) / 1000000 AS current_supply FROM tx_out AS tx_outer WHERE NOT EXISTS (SELECT tx_out.id FROM tx_out INNER JOIN tx_in ON tx_out.tx_id = tx_in.tx_out_id AND tx_out.index = tx_in.tx_out_index WHERE tx_outer.id = tx_out.id);"
                    execute_query "$selected_container" "$query"
                    read -p "Press Enter to continue..."
                ;;
                5)
                    delete_dbsync_container_and_associated_postgres "$selected_container"
                    read -p "Press Enter to continue..."
                    break 2 # Breaks out of the current loop and the container selection loop
                ;;
                6)
                    break  2 # Breaks out of both the inner loop and the container selection loop
                ;;
                *)
                    echo "Invalid choice, please select a valid option."
                    read -p "Press Enter to continue..."
                ;;
            esac
        done
    done
}