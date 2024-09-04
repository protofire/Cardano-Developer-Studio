# Smart Contracts Examples and Helpers Library

## Overview

This repository provides a comprehensive suite of tools and examples for
developing and testing Plutus smart contracts. It includes the Helpers Library
to streamline development, along with a variety of example contracts and their
corresponding tests. 

The tools and examples are designed to assist developers in
understanding, building, and deploying Plutus smart contracts effectively.

## Table of Contents

- [Smart Contracts Examples and Helpers Library](#smart-contracts-examples-and-helpers-library)
  - [Overview](#overview)
  - [Table of Contents](#table-of-contents)
  - [Helpers Library](#helpers-library)
    - [Step-by-Step Guide: Using the Helpers Library](#step-by-step-guide-using-the-helpers-library)
  - [Smart Contract Examples](#smart-contract-examples)
    - [Policy AlwaysFalse](#policy-alwaysfalse)
    - [Policy AlwaysTrue](#policy-alwaystrue)
    - [Policy CheckDate](#policy-checkdate)
    - [Policy CheckSignature](#policy-checksignature)
    - [Policy RedeemerFT](#policy-redeemerft)
    - [Policy RedeemerNFT](#policy-redeemernft)
    - [Validator AlwaysFalse](#validator-alwaysfalse)
    - [Validator AlwaysTrue](#validator-alwaystrue)
    - [Validator CheckDate](#validator-checkdate)
    - [Validator CheckSignature](#validator-checksignature)
  - [Choosing your Environment](#choosing-your-environment)
    - [Available Environments](#available-environments)
    - [Core Requirements](#core-requirements)
    - [Docker Compose](#docker-compose)
    - [Dev Container](#dev-container)
    - [Local Execution](#local-execution)
      - [Set Up Local Environment](#set-up-local-environment)
        - [Set environment variables](#set-environment-variables)
        - [Install necessary packages](#install-necessary-packages)
        - [Download in Temp fodler](#download-in-temp-fodler)
        - [Clone and install secp256k1](#clone-and-install-secp256k1)
          - [Clone and install libsodium](#clone-and-install-libsodium)
        - [Download ghcup](#download-ghcup)
        - [Install GHC, Cabal, and HLS](#install-ghc-cabal-and-hls)
        - [Update cabal](#update-cabal)
        - [Download and install stylish-haskell](#download-and-install-stylish-haskell)
  - [Working with the Examples](#working-with-the-examples)
    - [Running the Example CLI](#running-the-example-cli)
      - [Build and test the examples](#build-and-test-the-examples)
      - [Deploy the compiled Plutus smart contracts](#deploy-the-compiled-plutus-smart-contracts)
      - [Make transactions with the examples](#make-transactions-with-the-examples)
    - [Using a Terminal](#using-a-terminal)

## Helpers Library

The Helpers Library, located in the directory `./cardano-smart-contracts-examples/Helpers` is a set of
utility functions designed to simplify and streamline the development of smart
contracts in Plutus. These functions cover common tasks and operations needed
throughout the lifecycle of a smart contract, from input validation to building
complex transactions. The library is divided into modules focusing on different
aspects of development:

- **On-Chain Helpers:** Functions that run on-chain, assisting with validations
  and the logic of smart contracts.
- **Off-Chain Helpers:** Utilities that facilitate building transactions and
  communicating with the blockchain from an off-chain environment.
- **Evaluation Helpers:** Tools for evaluating and simulating the execution of
  smart contracts, enabling thorough testing before deployment.

### Step-by-Step Guide: Using the Helpers Library

To use the Helpers Library in your Haskell project, follow these steps:

1. **Add the Helpers Library as a dependency in your Cabal file and cabal
   project**:

   ```cabal
   -- *.cabal

   build-depends: cardano-helpers
   ```

   ```cabal
   -- *.project

   packages:
       Helpers
   ```

2. **Import the Helpers Library in your Haskell code**:
   ```haskell
   import qualified Helpers.OnChain                   as OnChainHelpers -- OnChain is just possibilities
   import qualified Helpers.OffChain                  as OffChainHelpers
   import qualified Helpers.OffChainEval              as OffChainEval
   ```

## Smart Contract Examples

The directories `./cardano-smart-contracts-examples/Policies` and `./cardano-smart-contracts-examples/Validators` 
contains a collection of examples for Plutus minting policies and validator contracts, along with their corresponding tests.
Each contract example is organized into its own directory with the following
structure:

- **`src/`**: Contains the Haskell source code for the validator contract or
  minting policy.
- **`test/`**: Contains the Haskell source code for the tests related to the
  contract or policy.
- **`.cabal`**: The Cabal configuration file for building and testing the
  project.

### Policy AlwaysFalse

- See README file: [Policy AlwaysFalse](./Policies/AlwaysFalse/README.md)

### Policy AlwaysTrue

- See README file: [Policy AlwaysTrue](./Policies/AlwaysTrue/README.md)

### Policy CheckDate

- See README file: [Policy CheckDate](./Policies/CheckDate/README.md)

### Policy CheckSignature

- See README file: [Policy CheckSignature](./Policies/CheckSignature/README.md)

### Policy RedeemerFT

- See README file: [Policy RedeemerFT](./Policies/RedeemerFT/README.md)

### Policy RedeemerNFT

- See README file: [Policy RedeemerNFT](./Policies/RedeemerNFT/README.md)

### Validator AlwaysFalse

- See README file: [Validator AlwaysFalse](./Validators/AlwaysFalse/README.md)

### Validator AlwaysTrue

- See README file: [Validator AlwaysTrue](./Validators/AlwaysTrue/README.md)

### Validator CheckDate

- See README file: [Validator CheckDate](./Validators/CheckDate/README.md)

### Validator CheckSignature

- See README file:
  [Validator CheckSignature](./Validators/CheckSignature/README.md)

## Choosing your Environment

We have set up multiple working environments to accommodate different development preferences and setups. 
Regardless of the environment you choose, the core requirement is to use Cabal to compile the projects.

### Available Environments

- **Docker Compose**: A containerized setup that includes all necessary dependencies.
- **Dev Container**: A pre-configured development environment that can be used with Visual Studio Code.
- **Local Environment**: A step-by-step guide to install all required tools on your local machine.

### Core Requirements

Regardless of your chosen environment, you'll need:

GHC 8.10.7  
Cabal 3.10.3

These are already set up in the Docker and Dev Container environments. If you're using a local setup, make sure to install these versions.

### Docker Compose

You must have the **Cardano Development Container** created and running.

Follow this instructions to set-up:

- [COMPOSE](../docs/README_SCRIPT.md#docker-compose-menu)
- [USE](../docs/README_SCRIPT.md#cardano-development-tools-menu)

Once the container is created you can work from a **terminal** outside or within
the container and run the **Toolbox CLI**. See
[Running the Example CLI](#running-the-example-cli)

But you also can perform commands in the terminal within the container:
[Using a Terminal](#using-a-terminal)

### Dev Container

The `devcontainer/` folder within the cardano-smart-contracts-examples directory provides a ready-to-use
development environment. This folder contains all the necessary configurations
for using Visual Studio Code's Remote - Containers extension. This setup offers
a consistent and isolated development environment for your Plutus smart contract
projects.

To use the Dev Container:

1. **Open the examples folder in Visual Studio Code:**

   ```
   code ./cardano-smart-contracts-examples
   ```

2. **Use the Dev Container:**
   - Follow the instructions in
     [Using the Development Container in VS Code](../docs/README_VSCODE.md) to
     start the container.
   - Once inside the Dev Container, you can work with the examples directly from
     the **terminal** ([Using a Terminal](#using-a-terminal)), or use the
     integrated **Toolbox CLI** to manage tasks like compiling, testing,
     deploying, and making transactions with the examples as described in the
     [Running the Example CLI](#running-the-example-cli) using the
     `7) Cardano Development Tools` option.

### Local Execution

If you prefer to work locally without containers:

1. **Install all dependencies locally** as detailed in the
   [Set Up Local Environment](#set-up-local-environment) section.
2. **Use the terminal** to compile, test, and deploy the examples. Follow the
   steps provided in the [Using a Terminal](#using-a-terminal)

#### Set Up Local Environment

##### Set environment variables

```
export USER=$(whoami)
export HOME=/home/${USER}
export TEMPDir=/tmp

#x86_64 | arm64
export BUILDARCH=x86_64
export IOKH_LIBSECP251_GIT_REV=ac83be33d0956faf6b7f61a60ab524ef7d6a473a
export IOHK_LIBSODIUM_GIT_REV=66f017f16633f2060db25e17c170c2afa0f2a8a1
export CABAL_VERSION=3.6.2.0
export GHC_VERSION=8.10.7
export HLS_VERSION=1.7.0.0
```

##### Install necessary packages

```
sudo add-apt-repository ppa:rmescandon/yq -y
sudo apt-get update -y
sudo apt-get install -y \
    curl \
    xz-utils \
    automake \
    build-essential \
    g++ \
    git \
    jq \
    libicu-dev \
    libffi-dev \
    libgmp-dev \
    libncursesw5 \
    libpq-dev \
    libssl-dev \
    libsystemd-dev \
    libtinfo-dev \
    libtool \
    make \
    pkg-config \
    tmux \
    wget \
    zlib1g-dev \
    libreadline-dev \
    llvm \
    libnuma-dev \
    software-properties-common \
    sudo \
    vim \
    apt-file \
    liblzma-dev \
    lsof \
    grep \
    coreutils \
    yq
```

##### Download in Temp fodler

```
cd $TEMPDir
```

##### Clone and install secp256k1

```
git clone https://github.com/bitcoin-core/secp256k1
cd secp256k1
git fetch --all --tags
git checkout ${IOKH_LIBSECP251_GIT_REV}
./autogen.sh
./configure --prefix=/usr --enable-module-schnorrsig --enable-experimental
make
sudo make install
cd ..
rm -rf secp256k1
```

###### Clone and install libsodium

```
git clone https://github.com/input-output-hk/libsodium.git
cd libsodium
git fetch --all --tags
git checkout ${IOHK_LIBSODIUM_GIT_REV}
./autogen.sh
./configure --prefix=/usr
make
sudo make install
cd ..
rm -rf libsodium
```

##### Download ghcup

```
wget --secure-protocol=TLSv1_2 https://downloads.haskell.org/~ghcup/${BUILDARCH}-linux-ghcup
chmod +x ${BUILDARCH}-linux-ghcup
mv ${BUILDARCH}-linux-ghcup ${HOME}/.ghcup/bin/ghcup
```

##### Install GHC, Cabal, and HLS

```
sudo -u ${USER} -H bash -c "
    mkdir -p ${HOME}/.ghcup/bin
    export PATH=${PATH}:${HOME}/.ghcup/bin
    ghcup config set cache true
    ghcup install ghc ${GHC_VERSION}
    ghcup install cabal ${CABL_VERSION}
    ghcup set ghc ${GHC_VERSION}
    ghcup install hls ${HLS_VERSION}
    ghcup config set cache false
    ghcup gc --cache
"
```

##### Update cabal

```
sudo -u ${USER} -H bash -c "
    export PATH=${PATH}:${HOME}/.ghcup/bin:${HOME}/.cabal/bin
    cabal update
"
```

##### Download and install stylish-haskell

```
wget https://github.com/rober-m/stylish-haskell/releases/download/v0.14.3.0/x86_64-linux-stylish-haskell
chmod +x x86_64-linux-stylish-haskell
sudo mv x86_64-linux-stylish-haskell /usr/bin/stylish-haskell
```

## Working with the Examples

Once your environment is set up, you can work with the Smart Contract examples (build, deploy, and test) using Cabal commands or run the **Toolbox CLI**. 
The process is the same regardless of your chosen environment.

### Running the Example CLI

Once you have a running **Cardano Development Container** or a local machine
ready, follow this steps to run the **Toolbox CLI** and follow the
menus on the terminal.

#### Build and test the examples

To build and test the examples, follow these steps:

1. **Run the Toolbox CLI**:

   ```bash
   ./script/run.sh
   ```

2. **Choose the "Cardano Development Tools" option**:

   ```
   ----
   Main Menu - Choose an option:
   ----
   1) Docker Compose Workflow
   2) Cardano Node Tools
   3) Cardano Wallet Tools
   4) Cardano DB Sync Tools
   5) Ogmios Tools
   6) Kupo Tools
   7) Cardano Development Tools
   0) Exit
   Enter your choice or 0 to exit: 7
   ```

3. **Choose the a Cardano Development Container or use the local execution**:

   ```
   Fetching list of cardano-development containers (including stopped)...

   Available cardano-development Containers (0 to Return Main Menu)
   Default: Local machine
   1) cardano-development-cardano-development-1 - Up 17 seconds
   Select a container [0-1]:
   ```

   For this example, I select run it i my local machine.

4. **Choose the "Cabal Build All" option**:

   ```cabal
   ----
   MENU - Selected container: Local Machine
   ----
   1) Cabal Build All
   2) Cabal Test All
   3) Run Example Contract CLI
   0) Return Main Menu
   Enter your choice or 0 to exit: 1
   ```

5. **Choose the "Cabal Test All" option**:
   ```cabal
   ----
   MENU - Selected container: Local Machine
   ----
   1) Cabal Build All
   2) Cabal Test All
   3) Run Example Contract CLI
   0) Return Main Menu
   Enter your choice or 0 to exit: 2
   ```
   If you seen a green OK in every test, everything's gonna be alright.

#### Deploy the compiled Plutus smart contracts

To test the examples, follow these steps:

1. **Run the Toolbox CLI**:

   ```bash
   ./script/run.sh
   ```

2. **Choose the "Cardano Development Tools" option**:

   ```
   ----
   Main Menu - Choose an option:
   ----
   1) Docker Compose Workflow
   2) Cardano Node Tools
   3) Cardano Wallet Tools
   4) Cardano DB Sync Tools
   5) Ogmios Tools
   6) Kupo Tools
   7) Cardano Development Tools
   0) Exit
   Enter your choice or 0 to exit: 7
   ```

3. **Choose the a Cardano Development Container or use the local execution**:

   ```
   Fetching list of cardano-development containers (including stopped)...

   Available cardano-development Containers (0 to Return Main Menu)
   Default: Local machine
   1) cardano-development-cardano-development-1 - Up 17 seconds
   Select a container [0-1]:
   ```

   For this example, I select run it i my local machine.

4. **Choose the "Run Example Contract CLI" option**:

   ```cabal
   ----
   MENU - Selected container: Local Machine
   ----
   1) Cabal Build All
   2) Cabal Test All
   3) Run Example Contract CLI
   0) Return Main Menu
   Enter your choice or 0 to exit: 3
   ```

5. **Choose the desire contract type to deploy**:

   ```
   ----
   MENU - Selected Type of Contract
   ----
   1) Policies Contract Examples CLI
   2) Validators Contract Examples CLI
   Enter your choice or 0 to exit: 2
   ```

   For this example, I select validators type.

6. **Choose the desire contract to deploy**:
   ```
   ----
   MENU - Selected Validators Example
   ----
   1) Always False CLI
   2) Always True CLI
   3) Check Date CLI
   4) Check Signature CLI
   Enter your choice or 0 to exit: 2
   ```
   For this example, I select Always True.
7. **Choose "deploy" option**:

   ```
   ----
   Main Menu - AlwaysTrueValidator - Choose an option:
   ----
   1) Test
   2) Deploy
   3) Create Transactions
   0) Exit
   Enter your choice or 0 to exit: 2
   ```

   Then will ask you for the folder name of the deploy. Finally, the deploy will
   be find in:

   - **`Cardano-Developer-Studio/export/{Contract Name}Validator/{Folder Name}/`**

     - **`{Contract Name}-HEX.addr`**: The JSON snippet you provided represents
       a data structure typically used in Cardano or similar blockchain
       platforms to describe an address's credentials.
     - **`{Contract Name}-Mainnet.addr`**: Is a Bech32-encoded Cardano address
       of the contract for Mainnet.
     - **`{Contract Name}-Testnet.addr`**: Is a Bech32-encoded Cardano address
       of the contract for Testnet.
     - **`{Contract Name}.hash`**: This byte sequence could be used in scripts
       or smart contracts, where it might represent an identifier, a reference
       to a particular key or script, or other data that has been encoded into a
       compact format.
     - **`{Contract Name}.plutus`**:The JSON object you provided is a
       representation of a Plutus V2 script in Cardano, encoded in CBOR (Concise
       Binary Object Representation) format.

   - **`Cardano-Developer-Studio/export/{Contract Name}Policy/{Folder Name}/`**
     - **`{Contract Name}.plutus`**:The JSON object you provided is a
       representation of a Plutus V2 script in Cardano, encoded in CBOR (Concise
       Binary Object Representation) format.
     - **`{Contract Name}.symbol`**:The JSON object you provided is a
       representation of a unique identifier associated with a native token or a
       specific type of asset on the blockchain generated for this policy
       contract.

#### Make transactions with the examples

To test the examples, follow these steps:

1. **Run the Toolbox CLI**:

   ```bash
   ./script/run.sh
   ```

2. **Choose the "Cardano Development Tools" option**:

   ```
   ----
   Main Menu - Choose an option:
   ----
   1) Docker Compose Workflow
   2) Cardano Node Tools
   3) Cardano Wallet Tools
   4) Cardano DB Sync Tools
   5) Ogmios Tools
   6) Kupo Tools
   7) Cardano Development Tools
   0) Exit
   Enter your choice or 0 to exit: 7
   ```

3. **Choose the a Cardano Development Container or use the local execution**:

   ```
   Fetching list of cardano-development containers (including stopped)...

   Available cardano-development Containers (0 to Return Main Menu)
   Default: Local machine
   1) cardano-development-cardano-development-1 - Up 17 seconds
   Select a container [0-1]:
   ```

   For this example, I select run it i my local machine.

4. **Choose the "Run Example Contract CLI" option**:

   ```cabal
   ----
   MENU - Selected container: Local Machine
   ----
   1) Cabal Build All
   2) Cabal Test All
   3) Run Example Contract CLI
   0) Return Main Menu
   Enter your choice or 0 to exit: 3
   ```

5. **Choose the desire contract type to use**:

   ```
   ----
   MENU - Selected Type of Contract
   ----
   1) Policies Contract Examples CLI
   2) Validators Contract Examples CLI
   Enter your choice or 0 to exit: 2
   ```

   For this example, I select validators type.

6. **Choose the desire contract to use**:
   ```
   ----
   MENU - Selected Validators Example
   ----
   1) Always False CLI
   2) Always True CLI
   3) Check Date CLI
   4) Check Signature CLI
   Enter your choice or 0 to exit: 2
   ```
   For this example, I select Always True.
7. **Choose "Create Transactions" option**:
   ```
   ----
   Main Menu - AlwaysTrueValidator - Choose an option:
   ----
   1) Test
   2) Deploy
   3) Create Transactions
   0) Exit
   Enter your choice or 0 to exit: 3
   ```
8. **Choose "Create Transactions" option**:

   ```
   ----
   Transaction Menu - AlwaysTrueValidator - Choose an option:
   ----
   1) Select Container with Node - Selected:
   2) Select Smart Contract Files - Selected:
   3) Select Wallet Files - Selected:
   4) UTXOs in Wallet
   5) UTXOs in Smart Contracts
   6) Create UTXO for Collateral in Wallet
   7) Create Vesting Transaction
   8) Create Claiming Transaction
   0) Return to Main Menu
   Enter your choice or 0 to return:
   ```

   In this instance, you will need to have a Node container running, along with
   a wallet and the smart contract deployed beforehand.

9. **Select the node container, the smart contract and wallet files**: For the
   node selection:

   ```
   Enter your choice or 0 to return: 1
   Selecting container with node...
   ----
   Fetching list of cardano-node-container containers (running only)...
   ----
   Available cardano-node-container Containers (0 to Return Main Menu)
   1) cardano-node-container-9.0.0-preprod - Up 19 hours (healthy)
   Select a container [0-1]:
   ```

   For the smart contract selection:

   ```
   Enter your choice or 0 to return: 2
   Selecting smart contract files...
   ----
   Folders in '(DEVELOPER_STUDIO_ROOT)/export/AlwaysTrueValidator':
   ----
   1) 2024-08-09-20-11
   2) 2024-08-09-20-46
   0) None (Exit)
   Select a folder [1-2] or 0 to exit:
   ```

   For the smart contract selection:

   ```
   Enter your choice or 0 to return: 3
   Selecting wallet files...
   ----
   Folders in '(DEVELOPER_STUDIO_ROOT)/.priv/wallets':
   ----
   1) test1
   2) test2
   0) None (Exit)
   Select a folder [1-2] or 0 to exit:
   ```

10. **Try it by your self**:

```
Transaction Menu - AlwaysTrueValidator - Choose an option:
----
1) Select Container with Node - Selected: cardano-node-container-9.0.0-preprod
2) Select Smart Contract Files - Selected: 2024-08-09-20-11
3) Select Wallet Files - Selected: test1
4) UTXOs in Wallet
5) UTXOs in Smart Contracts
6) Create UTXO for Collateral in Wallet
7) Create Vesting Transaction
8) Create Claiming Transaction
0) Return to Main Menu
Enter your choice or 0 to return:
```

Explore by your self the options in this menu. In it you can Vest and Claim the
contracts and consults por the UTxOs in it.

Note: In policies case, the options 6) and 7) are changed for:

```
6) Create Minting Transaction
7) Create Burning Transaction
```

### Using a Terminal

Once you have a local machine ready with all required dependences (or within a
**Cardano Development Container**), follow these steps:

1. **Build the project**:

   ```bash
   cabal build
   ```

2. **Run the tests**:
   ```bash
   cabal test
   ```

If you want to build and test all the examples in the `examples` directory, you
can use the following commands:

- **Build all examples**:

  ```bash
  cabal build all
  ```

- **Run all tests**:
  ```bash
  cabal test all
  ```

