
### Table of Contents
- [Table of Contents](#table-of-contents)
- [Cardano Node Container](#cardano-node-container)
- [Cardano Wallet Server Container](#cardano-wallet-server-container)
  - [Importing API Collections into Postman](#importing-api-collections-into-postman)
  - [Add test founds to a wallet using Faucet](#add-test-founds-to-a-wallet-using-faucet)
- [Cardano DB Sync Container](#cardano-db-sync-container)
- [Cardano Development Container](#cardano-development-container)

### Cardano Node Container

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
### Cardano Wallet Server Container

To interact with the Cardano Wallet Server container, you can open a shell within the container using Visual Studio Code (VS Code).

Install the "Remote - Containers" extension in VS Code.

Open the project folder in VS Code.

Click on the green "><" button in the bottom-left corner of VS Code and select "Attach to Running Container".

Choose the Cardano Wallet Server container from the list.

Once inside the container, you can run various commands, such as: 

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

#### Add test founds to a wallet using Faucet
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

### Cardano DB Sync Container

To interact with the Cardano DB Sync container, you can open a shell within the container using Visual Studio Code (VS Code).

Install the "Remote - Containers" extension in VS Code.

Open the project folder in VS Code.

Click on the green "><" button in the bottom-left corner of VS Code and select "Attach to Running Container".

Choose the Cardano DB Sync container from the list.

Once inside the container, you can run various commands.

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

### Cardano Development Container
This container includes a comprehensive suite of tools, libraries, and examples to assist in Plutus smart contract development. It is equipped with `cabal` and `GHC`, which are essential for compiling and testing Haskell code, making it an ideal environment for Plutus development.

To interact with the Cardano Development Container, you can open a shell within the container using Visual Studio Code (VS Code).

1. Install the "Remote - Containers" extension in VS Code.
2. Open the project folder in VS Code.
3. Click on the green "><" button in the bottom-left corner of VS Code and select "Attach to Running Container".
4. Choose the Cardano Development Container from the list.

Once inside the container, you can run various commands to build and test your smart contracts using `cabal` and `GHC`.

You can read more in [Smart Contracts Examples and Helpers Library](../cardano-smart-contracts-examples/README.md)

**Basic Cabal Commands:**

- Compile all examples:
  
  ```
  cabal build all
  ```

- Test all examples:
  
  ```
  cabal test all
  ```