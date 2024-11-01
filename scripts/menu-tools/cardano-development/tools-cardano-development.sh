#!/bin/bash

source "$(dirname "${BASH_SOURCE[0]}")/../../utils/utils.sh"

setWorkspaceDir

# Specific function for container selection
select_development_container() {
  if [[ -z "$INSIDE_DEV_CONTAINER" ]]; then
    if ! select_container 'cardano-development' 1 0; then
      return 1
    fi
  else
    selected_container='cardano-development'
    do_local_execution=0
  fi
}

# Function to interact with the Cardano Dev container
cardano_dev_tools() {
  while select_development_container; do
    while true; do
      echo "----"
      echo "MENU - Selected container: $selected_container"
      echo "----"
      echo "1) Cabal Build All"
      echo "2) Cabal Test All"
      echo "3) Run Example Contract CLI"
      if [[ $do_local_execution == 1 || !(-z $INSIDE_DEV_CONTAINER) ]]; then
        echo "4) Docker Logs"
        echo "5) Delete this Container and Optionally Its Volumes"
      fi
      echo "0) Return Main Menu"
      read -p "Enter your choice or 0 to exit: " tool_choice

      case $tool_choice in
        1)
          if [[ $do_local_execution == 1  ]]; then
            docker exec -it "$selected_container" cabal build all
          else
            if [[ -z $INSIDE_DEV_CONTAINER ]]; then
              cd $WORKSPACE_ROOT_DIR_ABSOLUTE/cardano-smart-contracts-examples
            fi
            cabal build all
            if [[ -z $INSIDE_DEV_CONTAINER ]]; then
              cd -
            fi
          fi
          read -p "Press Enter to continue..."
          ;;
        2)
          if [[ $do_local_execution == 1 ]]; then
            docker exec -it "$selected_container" cabal test all
          else
            if [[ -z $INSIDE_DEV_CONTAINER ]]; then
              cd $WORKSPACE_ROOT_DIR_ABSOLUTE/cardano-smart-contracts-examples
            fi
            cabal test all
            if [[ -z $INSIDE_DEV_CONTAINER  ]]; then
              cd -
            fi
          fi
          read -p "Press Enter to continue..."
          ;;
        3) 
          contract_type_selector
          read -p "Press Enter to continue..."
          ;;
        4)
          if [[ $do_local_execution == 1 || !(-z $INSIDE_DEV_CONTAINER) ]]; then
            monitor_logs "$selected_container"
            read -p "Press Enter to continue..."
          else
            echo "Invalid choice, please select a valid option."
            read -p "Press Enter to continue..."
          fi
          ;;
        5)
          if [[ $do_local_execution == 1 || !(-z $INSIDE_DEV_CONTAINER) ]]; then
            delete_container_and_optionally_volumes "$selected_container"
            break 2 # Breaks out of the current loop and the container selection loop
            read -p "Press Enter to continue..."
          else
            echo "Invalid choice, please select a valid option."
            read -p "Press Enter to continue..."
          fi
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

contract_type_selector (){
  echo "----"
  echo "MENU - Selected Type of Contract"
  echo "----"
  echo "1) Policies Contract Examples CLI"
  echo "2) Validators Contract Examples CLI"
  read -p "Enter your choice or 0 to exit: " contract_choice

  case $contract_choice in
    1)
      policy_selector
      ;;
    2)
      validator_selector
      ;;
    0)
      ;;
    *)
      echo "Invalid choice, please select a valid option."
      contract_type_selector
      ;;
  esac
}

policy_selector() {
  echo "----"
  echo "MENU - Selected Contract Example"
  echo "----"

  echo "1) Always False CLI"
  echo "2) Always True CLI"
  echo "3) Check Date CLI"
  echo "4) Check Signature CLI"
  echo "5) Mint/Burn FT CLI"
  echo "6) Mint/Burn NFT CLI"
  read -p "Enter your choice or 0 to exit: " policies_choice

  case $policies_choice in
    1) policy_script="AlwaysFalse" ;;
    2) policy_script="AlwaysTrue" ;;
    3) policy_script="CheckDate" ;;
    4) policy_script="CheckSignature" ;;
    5) policy_script="RedeemerFT" ;;
    6) policy_script="RedeemerNFT" ;;
    0) return ;;  # Exit
    *) echo "Invalid choice, please select a valid option." ; policy_selector ; return ;;
  esac

  run_policy_script "$policy_script"
}

run_policy_script() {
  local script_name=$1

  if [[ $do_local_execution == 1 ]]; then
    docker exec -it "$selected_container" bash -c "export INSIDE_DEV_CONTAINER=1 && cd ~/workspace && bash ./Policies/$script_name/scripts/cli.sh"
  else
    if [[ -z $INSIDE_DEV_CONTAINER ]]; then
      bash "$WORKSPACE_ROOT_DIR_ABSOLUTE/cardano-smart-contracts-examples/Policies/$script_name/scripts/cli.sh"
    else
      bash "$WORKSPACE_ROOT_DIR_ABSOLUTE/Policies/$script_name/scripts/cli.sh"
    fi
  fi
}

execute_validator_script() {
  local validator_name=$1

  if [[ $do_local_execution == 1 ]]; then
    docker exec -it "$selected_container" bash -c "export INSIDE_DEV_CONTAINER=1 && cd ~/workspace && bash ./Validators/$validator_name/scripts/cli.sh"
  else
    if [[ -z $INSIDE_DEV_CONTAINER ]]; then
      bash "$WORKSPACE_ROOT_DIR_ABSOLUTE/cardano-smart-contracts-examples/Validators/$validator_name/scripts/cli.sh"
    else
      bash "$WORKSPACE_ROOT_DIR_ABSOLUTE/Validators/$validator_name/scripts/cli.sh"
    fi
  fi
}

validator_selector() {
  echo "----"
  echo "MENU - Selected Validators Example"
  echo "----"
  echo "1) Always False CLI"
  echo "2) Always True CLI"
  echo "3) Check Date CLI"
  echo "4) Check Signature CLI"
  read -p "Enter your choice or 0 to exit: " validators_choice
  
  case $validators_choice in
    1) execute_validator_script "AlwaysFalse" ;;
    2) execute_validator_script "AlwaysTrue" ;;
    3) execute_validator_script "CheckDate" ;;
    4) execute_validator_script "CheckSignature" ;;
    0) ;;
    *)
      echo "Invalid choice, please select a valid option."
      validator_selector
      ;;
  esac
}

