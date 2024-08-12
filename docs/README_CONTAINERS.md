### Cardano Node

To interact with the Cardano Node container, you can open a shell within the container using Visual Studio Code (VS Code).

Install the "Remote - Containers" extension in VS Code.

Open the project folder in VS Code.

Click on the green "><" button in the bottom-left corner of VS Code and select "Attach to Running Container".

Choose the Cardano Node container from the list.


Once inside the container, you can run various commands, such as: 

**Run the following command to check the version:** 

```
cardano-node --version
```

**To query the blockchain tip, use:**

```
cardano-cli query tip --socket-path /ipc/node.socket --$CARDANO_NETWORK_WITH_MAGIC
```
### Cardano Wallet

To interact with the Cardano Wallet container, follow the same steps as mentioned for the Cardano Node container to open a shell within the container using VS Code.

Inside the container, you can perform actions like generating a recovery phrase:

```
cardano-wallet recovery-phrase generate --size 15
```

To create a wallet using the generated recovery phrase, you can use cURL or Postman to make an API request:

```
curl -X POST http://localhost:8090/v2/wallets \
  -H "Content-Type: application/json" \
  -d '{
        "name": "Your Wallet Name",
        "mnemonic_sentence": ["salute", "ritual", "length", "dress", "seminar", "adult", "hybrid", "travel", "ridge", "satisfy", "style", "vocal", "permit", "foster", "pizza"],
        "passphrase": "your-secure-passphrase"
      }'
```

Refer to the API documentation for more details:

[Cardano Wallet API Documentation](https://cardano-foundation.github.io/cardano-wallet/api/edge/)

#### Importing API Collections into Postman

To facilitate the testing and interaction with the Cardano Wallet Backend API, we've provided a collection of API endpoints in a Postman collection file named `cardano-wallet-API.json`. Follow these steps to import the collection into Postman and start using the APIs:

Open Postman: Launch the Postman application on your computer.

**Import the Collection:**

Click on the Import button, which can be found at the top left corner of the Postman interface.
In the dialog that appears, go to the File tab and click on Upload Files.
Browse to the location of the `cardano-wallet-API.json` file located in `configs/cardano-wallet` folder of the project, select it, and click Open to start the import process.

**Using the Collection:**

After the import is complete, the Cardano Wallet Backend API collection will appear in the Collections sidebar on the left.

Ensure you set the base URL to `http://localhost:8090/v2` in your Postman environment settings to match your local Cardano Wallet Backend instance, or update it according to your specific setup.

Expand the collection to view the API requests it contains. You can click on any request to load it into the active tab.Before sending a request, make sure to adjust any necessary parameters or request bodies as per your testing needs. Hit the Send button to execute the request and view the response from the Cardano Wallet Backend.

This collection provides a comprehensive set of API calls, enabling you to interact with the Cardano Wallet Backend for various operations such as wallet management, transaction creation, and blockchain queries. Using Postman collections can significantly streamline your workflow and facilitate efficient testing and integration development.

**Example API queries:**

http://localhost:8090/v2/network/information

http://localhost:8090/v2/wallets

### Add test founds to a wallet using Faucet
1) Obtain a Testnet Address

   If you don’t already have a Cardano testnet address, you’ll need to create one. 
   
   You can create one using the `run.sh` script. Using `Cardano Wallet Tools`, you can select the option 2 or 3 to create one.

      - If you use the option ``3) Export Wallet to File``, you can find the address in `.priv/{wallet name}.addr`.
      - If you use the option ``2) Create wallet``, you can find the address in getting the wallet id using option `4) List wallets ids and names` and then using it in option `5) List wallets utxos`.

   Or you can use a Cardano wallet like Daedalus Testnet or Yoroi Testnet. Follow the wallet's instructions to generate a new address.

2) Access the Faucet

   Visit the Cardano Testnet Faucet page: [Cardano Testnet Faucet](https://docs.cardano.org/cardano-testnets/tools/faucet/)
3) Request Test ADA 

   On the Faucet page:

   Enter Your Testnet Address: Paste the testnet address you obtained in Step 1 into the designated field on the Faucet page.

   Request Funds: Click the button to request funds. The Faucet will send a small amount of test ADA to the provided address.

4) Confirm the Transaction

   After requesting funds:

   Check the Status: The Faucet may display a transaction ID (TxID) or a success message indicating that the transaction has been processed.

   Verify in Wallet: Open your testnet wallet and check your balance. The test ADA should appear shortly, depending on the network’s speed.

### Cardano DB Sync

Refer to the documentation for more details:

[Cardano DB Sync Documentation](https://docs.cardano.org/cardano-components/cardano-db-sync/about-db-sync/)

[Cardano DB Sync GitHub](https://github.com/IntersectMBO/cardano-db-sync)

**Basic SQL queries to run against the synced database:**

- Sync progress of db-sync:
  
```
select
   100 * (extract (epoch from (max (time) at time zone 'UTC')) - extract (epoch from (min (time) at time zone 'UTC')))
      / (extract (epoch from (now () at time zone 'UTC')) - extract (epoch from (min (time) at time zone 'UTC')))
  as sync_percent
  from block ;
```

- Retrieving most recent block:

```
SELECT * FROM block ORDER BY time DESC LIMIT 1;
```

- Slot number of the most recent block:

```
select slot_no from block where block_no is not null
    order by block_no desc limit 1 ;
```

- Current total on-chain supply of Ada (queryTotalSupply):

This just queries the UTxO set for unspent transaction outputs. It does not include staking rewards that have have not yet been withdrawn. Before being withdrawn rewards exist in ledger state and not on-chain.

Note: 1 ADA == 1,000,000 Lovelace

```
select sum (value) / 1000000 as current_supply from tx_out as tx_outer where
    not exists
      ( select tx_out.id from tx_out inner join tx_in
          on tx_out.tx_id = tx_in.tx_out_id and tx_out.index = tx_in.tx_out_index
          where tx_outer.id = tx_out.id
      ) ;

```

- Retrieving transactions in a block:

```
SELECT * FROM tx WHERE block_id = (SELECT id FROM block WHERE block_no = <block_number>);
```

Replace <block_number> with the actual block number you want to query transactions for. This query retrieves all the transactions associated with a specific block.

- Retrieving transaction details:
  
```
SELECT * FROM tx WHERE hash = '<transaction_hash>';
```

Replace <transaction_hash> with the actual transaction hash you want to retrieve details for. This query returns the details of a specific transaction.

- Querying UTxO (Unspent Transaction Output) information:

```
SELECT * FROM utxo WHERE tx_id = (SELECT id FROM tx WHERE hash = '<transaction_hash>');
```

Replace <transaction_hash> with the actual transaction hash you want to query UTxOs for. This query retrieves the UTxOs associated with a specific transaction.

- Retrieving address balance:

```
SELECT SUM(value) AS balance FROM utxo WHERE address = '<address>';
```

Replace address with the actual Cardano address you want to retrieve the balance for. This query calculates the total balance of an address by summing the values of its UTxOs.

- Querying stake distribution:

This query retrieves the total stake for each stake pool.

```
select pool_id, sum (amount) from epoch_stake group by pool_id ;
```

### Plutus Development Toolkit

This project includes a comprehensive suite of tools, libraries, and examples to assist in Plutus smart contract development.

#### Helpers Library

The Helpers Library provides a set of utility functions designed to simplify and streamline Plutus development. These functions cover common tasks and operations that are frequently needed when working with Plutus smart contracts.

#### Example Suite

The Example Suite includes various example Plutus contracts to demonstrate different functionalities and use cases. These examples serve as practical guides to help you understand how to implement and deploy Plutus contracts.

#### Testing Functions

The project also includes functions for testing resource usage and transaction sizes. These functions help ensure that your contracts are efficient and comply with the constraints of the Plutus platform.

### Step-by-Step Guide to Using the Helpers Library

To use the Helpers Library in your Haskell project, follow these steps:

1. **Add the Helpers Library as a dependency in your Cabal file and cabal project**:
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
### Step-by-Step Guide to deploy the examples

To test the examples, follow these steps:
1. **Run the main script**:
   ```bash
   ./script/run.sh
   ```

2. **Choose the "Smart Contract Plutus Development Tools" option**:
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
      7) Smart Contract Plutus Development Tools
      0) Exit
      Enter your choice or 0 to exit: 7
      ```

3. **Choose the a Cardano Development Container or use the local execution**:
   ``` 
   ----
   Fetching list of cardano-development containers (including stopped)...
   ----
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
   Then will ask you for the folder name of the deploy.
   Finally, the deploy will be find in:

   - **`Cardano-Developer-Studio/export/{Contract Name}Validator/{Folder Name}/`**
      - **`{Contract Name}-HEX.addr`**:
The JSON snippet you provided represents a data structure typically used in Cardano or similar blockchain platforms to describe an address's credentials. 
      - **`{Contract Name}-Mainnet.addr`**: Is a Bech32-encoded Cardano address of the contract for Mainnet.
      - **`{Contract Name}-Testnet.addr`**: Is a Bech32-encoded Cardano address of the contract for Testnet.
      - **`{Contract Name}.hash`**: This byte sequence could be used in scripts or smart contracts, where it might represent an identifier, a reference to a particular key or script, or other data that has been encoded into a compact format.
      - **`{Contract Name}.plutus`**:The JSON object you provided is a representation of a Plutus V2 script in Cardano, encoded in CBOR (Concise Binary Object Representation) format. 
   
   - **`Cardano-Developer-Studio/export/{Contract Name}Policy/{Folder Name}/`**
      - **`{Contract Name}.plutus`**:The JSON object you provided is a representation of a Plutus V2 script in Cardano, encoded in CBOR (Concise Binary Object Representation) format.
      - **`{Contract Name}.symbol`**:The JSON object you provided is a representation of a unique identifier associated with a native token or a specific type of asset on the blockchain generated for this policy contract.

### Step-by-Step Guide to build and test the examples

To build and test the examples, follow these steps:
1. **Run the main script**:
   ```bash
   ./script/run.sh
   ```

2. **Choose the "Smart Contract Plutus Development Tools" option**:
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
      7) Smart Contract Plutus Development Tools
      0) Exit
      Enter your choice or 0 to exit: 7
      ```

3. **Choose the a Cardano Development Container or use the local execution**:
   ``` 
   ----
   Fetching list of cardano-development containers (including stopped)...
   ----
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

### Step-by-Step Guide to make transactions with the examples

To test the examples, follow these steps:
1. **Run the main script**:
   ```bash
   ./script/run.sh
   ```

2. **Choose the "Smart Contract Plutus Development Tools" option**:
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
      7) Smart Contract Plutus Development Tools
      0) Exit
      Enter your choice or 0 to exit: 7
      ```

3. **Choose the a Cardano Development Container or use the local execution**:
   ``` 
   ----
   Fetching list of cardano-development containers (including stopped)...
   ----
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
   In this instance, you will need to have a Node container running, along with a wallet and the smart contract deployed beforehand.

9. **Select the node container, the smart contract and wallet files**:
   For the node selection:
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
   Folders in '/home/francio/Documentos/Protofire/Cardano-Developer-Studio/export/AlwaysTrueValidator':
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
   Folders in '/home/francio/Documentos/Protofire/Cardano-Developer-Studio/.priv/wallets':
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
   Explore by your self the options in this menu. In it you can Vest and Claim the contracts and consults por the UTxOs in it.

   Note: In policies case, the options 6) and 7) are changed for:
   ```
   6) Create Minting Transaction
   7) Create Burning Transaction
   ```
### Using Different Environments

You can use different environments to work with the examples:

#### Local execution
Once you have all the dependencies installed, which you can find in the [examples README](./examples/README.md), follow the steps in the [Step-by-Step Guide to Build and Test the Examples section](#step-by-step-guide-to-build-and-test-the-examples).

#### Using Example Container

This container can be used as a standard Docker container or through Visual Studio Code Integration.

1. **Open the example folder in VSCode**:
   ```bash
   code ./example
   ```

   Details on this usage are explained in [Using the Development Container in VS Code](#using-the-development-container-in-vs-code).

This container installs all dependencies required for building, testing, and using the examples and helper libraries. You can follow the [Getting Started section](./examples/README.md?tab=readme-ov-file#getting-started) from here.

#### Using Developer Container

Similar to the [Example Container](#using-example-container), the main developer container can be used in the same way, as explained in the [Using the Development Container in VS Code section](#using-the-development-container-in-vs-code).

Inside this container, you can use [the main script](#running-the-main-script) and deploy the development container as described in the [Docker Compose Menu](#docker-compose-menu) using the `7) Smart Contract Plutus Development` option.

From there, you can explore the `./examples/` directory, [deploy](#step-by-step-guide-to-deploy-the-examples), [build and test](#step-by-step-guide-to-build-and-test-the-examples) the contract examples.

Additionally, you can use it as a playground for [making transactions to mint tokens, burn tokens, and manage vesting and claim contracts](#step-by-step-guide-to-make-transactions-with-the-examples).