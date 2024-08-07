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
      echo "3) Run Example Contract CLI"
      echo "4) Docker Logs"
      echo "5) Delete this Container and Optionally Its Volumes"
      echo "0) Return Main Menu"
      read -p "Enter your choice or 0 to exit: " tool_choice

      case $tool_choice in
        1)
          if [[ "$INSIDE_DEV_CONTAINER" ]]; then
            docker exec -it "$selected_container" cabal build all
          else
            cd $WORKSPACE_ROOT_DIR_ABSOLUTE/examples
            cabal build all
            cd -
          fi
          read -p "Press Enter to continue..."
          ;;
        2)
          if [[ "$INSIDE_DEV_CONTAINER" ]]; then
            docker exec -it "$selected_container" cabal test all
          else
            cd $WORKSPACE_ROOT_DIR_ABSOLUTE/examples

            cabal test all
            cd -
          fi
          read -p "Press Enter to continue..."
          ;;
        3) 
          contract_type_selector
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

contract_type_selector (){
  echo "----"
  echo "MENU - Selected Type of Contract"
  echo "----"
  echo "1) Validators Contract Examples CLI"
  echo "2) Policys Contract Examples CLI"
  read -p "Enter your choice or 0 to exit: " contract_choice

  case $contract_choice in
    1)
      validator_selector
      ;;
    2)
      policy_selector
      ;;
    0)
      ;;
    *)
      echo "Invalid choice, please select a valid option."
      contract_type_selector
      ;;
  esac
}

validator_selector(){
  echo "----"
  echo "MENU - Selected Validators Example"
  echo "----"
  echo "1) Allways True CLI"
  echo "2) Allways False CLI"
  echo "3) Check Date CLI"
  echo "4) Check Signature CLI"
  read -p "Enter your choice or 0 to exit: " validators_choice
  case $validators_choice in
    1)
      if [[ "$INSIDE_DEV_CONTAINER" ]]; then
        docker exec -it "$selected_container" bash -c "export INSIDE_DEV_CONTAINER=1 && cd ~/workspace && bash ./Validators/AllwaysTrue/scripts/cli.sh"
      else
        bash "$WORKSPACE_ROOT_DIR_ABSOLUTE/examples/Validators/CheckDate/AllwaysTrue/cli.sh"
      fi       
      ;;
    2)
      if [[ $INSIDE_DEV_CONTAINER == 1 ]]; then
        docker exec -it "$selected_container" bash -c "export INSIDE_DEV_CONTAINER=1 && cd ~/workspace && bash ./Validators/AllwaysFalse/scripts/cli.sh"
      else
        bash "$WORKSPACE_ROOT_DIR_ABSOLUTE/Validators/CheckDate/AllwaysFalse/cli.sh"
      fi       
      ;;
    3)
      if [[ "$INSIDE_DEV_CONTAINER" ]]; then
        docker exec -it "$selected_container" bash -c "export INSIDE_DEV_CONTAINER=1 && cd ~/workspace && bash ./Validators/CheckDate/scripts/cli.sh"
      else
        bash "$WORKSPACE_ROOT_DIR_ABSOLUTE/examples/examples/Validators/CheckDate/scripts/cli.sh"
      fi       
      ;;
    4)
      if [[ "$INSIDE_DEV_CONTAINER" ]]; then
        docker exec -it "$selected_container" bash -c "export INSIDE_DEV_CONTAINER=1 && cd ~/workspace && bash ./Validators/CheckSignature/scripts/cli.sh"
      else
        bash "$WORKSPACE_ROOT_DIR_ABSOLUTE/examples/Validators/CheckSignature/scripts/cli.sh"
      fi       
      ;;
    0)
      ;;
    *)
      echo "Invalid choice, please select a valid option."
      validator_selector
      ;;
  esac
}

policy_selector(){
  echo "----"
  echo "MENU - Selected Contract Example"
  echo "----"

  echo "1) Allways True CLI"
  echo "2) Allways False CLI"
  echo "3) Check Date CLI"
  echo "4) Check Signature CLI"
  echo "5) Mint/Burn FT CLI"
  echo "6) Mint/Burn NFT CLI"
  read -p "Enter your choice or 0 to exit: " policys_choice
  case $policys_choice in
    1)
      if [[ "$INSIDE_DEV_CONTAINER" ]]; then
        docker exec -it "$selected_container" bash -c "export INSIDE_DEV_CONTAINER=1 && cd ~/workspace && bash ./Validators/AllwaysTrue/scripts/cli.sh"
      else
        bash "$WORKSPACE_ROOT_DIR_ABSOLUTE/examples/Validators/CheckDate/AllwaysTrue/cli.sh"
      fi       
      ;;
    2)
      if [[ "$INSIDE_DEV_CONTAINER" ]]; then
        docker exec -it "$selected_container" bash -c "export INSIDE_DEV_CONTAINER=1 && cd ~/workspace && bash ./Validators/AllwaysFalse/scripts/cli.sh"
      else
        bash "$WORKSPACE_ROOT_DIR_ABSOLUTE/examples/Validators/CheckDate/AllwaysFalse/cli.sh"
      fi       
      ;;
    3)
      if [[ "$INSIDE_DEV_CONTAINER" ]]; then
        docker exec -it "$selected_container" bash -c "export INSIDE_DEV_CONTAINER=1 && cd ~/workspace && bash ./Validators/CheckDate/scripts/cli.sh"
      else
        bash "$WORKSPACE_ROOT_DIR_ABSOLUTE/examples/Validators/CheckDate/scripts/cli.sh"
      fi       
      ;;
    4)
      if [[ "$INSIDE_DEV_CONTAINER" ]]; then
        docker exec -it "$selected_container" bash -c "export INSIDE_DEV_CONTAINER=1 && cd ~/workspace && bash ./Validators/CheckSignature/scripts/cli.sh"
      else
        bash "$WORKSPACE_ROOT_DIR_ABSOLUTE/examples/Validators/CheckSignature/scripts/cli.sh"
      fi       
      ;;
    5)
      if [[ "$INSIDE_DEV_CONTAINER" ]]; then
        docker exec -it "$selected_container" bash -c "export INSIDE_DEV_CONTAINER=1 && cd ~/workspace && bash ./Validators/RedeemerFT/scripts/cli.sh"
      else
        bash "$WORKSPACE_ROOT_DIR_ABSOLUTE/examples/Validators/RedeemerFT/scripts/cli.sh"
      fi       
      ;;
    6)
      if [[ "$INSIDE_DEV_CONTAINER" ]]; then
        docker exec -it "$selected_container" bash -c "export INSIDE_DEV_CONTAINER=1 && cd ~/workspace && bash ./Validators/RedeemerNFT/scripts/cli.sh"
      else
        bash "$WORKSPACE_ROOT_DIR_ABSOLUTE/examples/Validators/RedeemerNFT/scripts/cli.sh"
      fi       
      ;;
    0)
      ;;
    *)
      echo "Invalid choice, please select a valid option."
      policy_selector
      ;;
  esac
}
