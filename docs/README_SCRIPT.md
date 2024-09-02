### Table of Contents
- [Table of Contents](#table-of-contents)
- [Introduction](#introduction)
- [Usage](#usage)
- [Main Menu](#main-menu)
- [Docker Compose Menu](#docker-compose-menu)
- [Cardano Node Tools Menu](#cardano-node-tools-menu)
- [Cardano Wallet Tools Menu](#cardano-wallet-tools-menu)
- [Cardano DB Sync Tools Menu](#cardano-db-sync-tools-menu)
- [Cardano Development Tools Menu](#cardano-development-tools-menu)
  - [Validators Contract Examples CLI](#validators-contract-examples-cli)
  - [Policies Contract Examples CLI](#policies-contract-examples-cli)
- [Note to Users](#note-to-users)
  - [Initial Requirements Checks by the Toolbox CLI](#initial-requirements-checks-by-the-toolbox-cli)
    - [What the Toolbox CLI Checks](#what-the-toolbox-cli-checks)

### Introduction

To streamline the setup and execution of the Cardano Developer Studio tools, we provide the **Toolbox CLI**, a versatile command-line tool designed to simplify the management of Cardano development tasks. This intuitive CLI, located in the `scripts` directory as `run.sh`, enables developers to easily navigate, configure, and execute various tools and components required for Cardano development.

### Usage

- **Open a terminal** and navigate to the root directory of the Cardano Developer Studio project.
- **Execute the Toolbox CLI** by running the following command:

  ```
  ./scripts/run.sh
  ```

This will launch the Toolbox CLI, presenting you with various options to manage and utilize the tools provided by the Cardano Developer Studio.

### Main Menu

The main menu provides the following options:

- `1) Docker Compose Workflow`: Initiates the Docker Compose workflow, allowing you to select and run various components such as the Cardano Node, Cardano Wallet, and more.
- `2) Cardano Node Tools`: Access and manage tools related to the Cardano Node.
- `3) Cardano Wallet Tools`: Manage wallets and perform operations such as generating mnemonics, creating wallets, and more.
- `4) Cardano DB Sync Tools`: Interact with the Cardano DB Sync, executing queries and retrieving blockchain data.
- `5) Ogmios Tools`: Utilize tools related to Ogmios, a lightweight bridge between Cardano and your applications.
- `6) Kupo Tools`: Manage Kupo, a scalable Cardano indexer, through dedicated tools.
- `7) Cardano Development Tools`: Access tools for developing and testing Plutus smart contracts.
- `0) Exit`: Exits the Toolbox CLI.

### Docker Compose Menu

After selecting the Docker Compose Workflow, you will see options to run specific components:

- `1) Cardano Node`: Starts the Cardano Node container with the selected configuration.
- `2) Cardano Wallet`: Initiates the Cardano Wallet container.
- `3) Cardano DB Sync`: Starts the Cardano DB Sync container.
- `4) Ogmios and Kupo`: Launches the Ogmios and Kupo containers, providing a lightweight bridge and a scalable Cardano indexer.
- `5) Cardano Development`: Starts the development environment for building and testing Plutus smart contracts.
- `0) Return Main Menu`: Returns to the main menu.

For each selection, you will be prompted to enter environment variables such as `CARDANO_NODE_VERSION`, `CARDANO_NETWORK`, and `CARDANO_NODE_PORT`. Default values are provided, but you may customize them as needed.

### Cardano Node Tools Menu

Selecting option `2` from the main menu, you can:

- `1) Cardano Node Version`: Display the version of the Cardano Node.
- `2) Cardano c Query Tip`: Query the current tip of the blockchain.
- `3) Delete this Container and Optionally Its Volumes`: Remove the selected container and its dependencies.
- `4) Return Main Menu`: Returns to the main menu.

### Cardano Wallet Tools Menu

Selecting option `3` from the main menu, the wallet tools menu allows you to:

- `1) Generate mnemonic`: Create a new recovery phrase.
- `2) Create wallet`: Generate a new wallet with a created mnemonic.
- `3) Export Wallet to File`: Export the current wallet data to a file.
- `4) List wallets ids and names`: Display all wallet IDs and their associated names.
- `5) List wallets utxos`: Show the unspent transaction outputs (UTXOs) of the wallets.
- `6) Addresses Tools`: Access tools for managing wallet addresses.
   - `1) PKH To Address`: Convert a public key hash (PKH) to a Cardano address.
   - `0) Return Main Menu`: Return to the main menu.
- `7) Fetch network information`: Retrieve current network information.
- `8) Docker Logs`: View the logs of the Docker container.
- `9) Delete this Container and Optionally Its Volumes`: Remove the selected container and optionally its associated volumes.
- `0) Return Main Menu`: Return to the main menu.

### Cardano DB Sync Tools Menu

By choosing option `4` from the main menu, the DB Sync tools menu provides:

- `1) Sync progress of db-sync`: Check the synchronization progress of DB Sync.
- `2) Retrieving most recent block`: Fetch the most recent block from the blockchain.
- `3) Slot number of the most recent block`: Query the slot number of the latest block.
- `4) Current total on-chain supply of Ada`: Calculate the current total supply of ADA.
- `5) Delete this Container and Optionally Its Volumes`: Remove the selected container and its dependencies.
- `6) Return Main Menu`: Returns to the main menu.

### Cardano Development Tools Menu

Selecting option `7` from the main menu, you can interact with the Cardano Development Container:

- `1) Cabal Build All`: Build all components using Cabal inside the selected container.
- `2) Cabal Test All`: Run all tests using Cabal inside the selected container.
- `3) Run Example Contract CLI`: Execute example contract scripts from the CLI.
- `4) Docker Logs`: View logs from the selected container.
- `5) Delete this Container and Optionally Its Volumes`: Delete the selected container and optionally its volumes.
- `0) Return Main Menu`: Returns to the main menu.

After selecting the type of contract example to run, you will be presented with specific options:

#### Validators Contract Examples CLI
- `1) Always True CLI`: Run the Always True contract example.
- `2) Always False CLI`: Run the Always False contract example.
- `3) Check Date CLI`: Run the Check Date contract example.
- `4) Check Signature CLI`: Run the Check Signature contract example.

#### Policies Contract Examples CLI
- `1) Always True CLI`: Run the Always True policy example.
- `2) Always False CLI`: Run the Always False policy example.
- `3) Check Date CLI`: Run the Check Date policy example.
- `4) Check Signature CLI`: Run the Check Signature policy example.
- `5) Mint/Burn FT CLI`: Run the Mint/Burn FT policy example.
- `6) Mint/Burn NFT CLI`: Run the Mint/Burn NFT policy example.

For each selection, you can choose to run the scripts inside the container or in the local environment, depending on your setup.

### Note to Users

- Before running the Toolbox CLI, ensure Docker is installed and running on your system. Follow the installation instructions provided in the previous sections for your respective operating system.
- The Toolbox CLI handles the intricacies of configuring and starting the Docker Compose services based on your selections. It automatically sets necessary environment variables and permissions to ensure a smooth setup experience.

#### Initial Requirements Checks by the Toolbox CLI

The Toolbox CLI starts by performing several checks to ensure the environment meets all necessary requirements for successful execution. These checks include verifying the presence of a package manager, installing required commands, and confirming that the versions of Bash, Docker, and Docker Compose are sufficient.

##### What the Toolbox CLI Checks

1. Package Manager: The Toolbox CLI checks if a recognized package manager is available on the system. This is crucial for installing other required packages.
   
2. Required Commands: The Toolbox CLI automatically installs the following essential commands if they are not already present:
   - jq
   - lz4
   - curl
   - grep
   - sed
   - gawk
   - cut

3. Software Versions: The Toolbox CLI verifies that the installed versions of Bash, Docker, and Docker Compose meet the minimum requirements:
   - Bash version 4.0 or newer
   - Docker version 19.03 or newer
   - Docker Compose version 1.25 or newer