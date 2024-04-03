# Welcome to Cardano Developer Studio

This repository is the home of the Cardano Developer Studio, an all-in-one suite designed to streamline the development process for Cardano blockchain developers. Our goal is to provide an accessible, comprehensive set of tools and resources to support developers in building dApps and smart contracts efficiently on the Cardano platform.

## Features

- Docker Compose configurations for easy setup and integration
- Script tool with menus to navigate and configure and run differents tools

## What is included

### Cardano Node

Knowing how to operate a Cardano Node is fundamental for developers because it serves as your gateway to the Cardano blockchain. With it, you can create and submit transactions, query the blockchain state, and participate in consensus. For dApp development, it provides the infrastructure needed to interact with the blockchain, ensuring your applications are well-integrated within the Cardano ecosystem. Additionally, understanding the node's workings is crucial for optimizing network participation, whether for personal projects or running a stake pool.

### Cardano Wallet

The wallet is more than just a way to store cryptocurrency; it's a critical component for any application needing to handle ADA transactions or manage staking operations. For developers, integrating wallet functionalities into your applications can significantly enhance user experience by providing secure and seamless transaction capabilities. Moreover, understanding wallet operations, such as key management and wallet restoration, is vital for building robust and secure applications on the Cardano platform.

### Cardano DB Sync

Data is king in the modern world, and Cardano DB Sync allows developers to harness blockchain data efficiently. Whether you're building financial tools, exploring chain analytics, or creating user-friendly interfaces for interacting with the blockchain, DB Sync provides the data backbone for these applications. By enabling complex queries and easy access to historical data, it allows for deep insights into blockchain operations and user transactions. This can inform business decisions, provide analytics services, or enhance application functionalities with rich data features.

### Script Menus and Tools

To facilitate interaction with Cardano Node, Wallet, and DB Sync, we have provided scripts that include menus for easy navigation and tool execution. 

## Why Developers Need to Experiment with These Tools

Experimentation leads to innovation. By getting hands-on experience with the Cardano Node, Wallet, and DB Sync, developers can push the boundaries of what's possible within the Cardano ecosystem. It's not just about building applications; it's about understanding the intricacies of blockchain technology and leveraging that knowledge to create solutions that are secure, efficient, and user-friendly.

For instance, experimenting with the Cardano Node can help developers optimize transaction fees, understand block propagation, and secure their applications against common blockchain threats. Learning the ins and outs of the Cardano Wallet can lead to the development of new wallet features, improved user security practices, and the integration of ADA payments into e-commerce platforms. And with Cardano DB Sync, the possibilities for blockchain data analytics, reporting tools, and real-time monitoring services are endless.

## Getting Started

Detailed instructions on setting up your development environment and using the tools provided will be added to this README.

## Installing Docker

### Windows

**Docker Desktop for Windows:**

- Visit the Docker Hub at https://www.docker.com/products/docker-desktop and download the installer for Windows.
- Run the installer and follow the instructions to install Docker Desktop on Windows.
- After installation, Docker will start automatically. You might need to log out and log back in or reboot your computer to complete the installation.

### Mac

**Docker Desktop for Mac:**

- Visit the Docker Hub at https://www.docker.com/products/docker-desktop and download the installer for Mac.
- Open the `.dmg` file and drag Docker to the Applications folder.
- Run Docker from the Applications folder. Docker will request your password to install a helper tool.
- After installation, Docker will start automatically.

### Ubuntu

**Install using the repository:**

Update your package index and install packages to allow `apt` to use a repository over HTTPS:
```
sudo apt-get update
sudo apt-get install \
  ca-certificates \
  curl \
  gnupg \
  lsb-release
```

Add Docker’s official GPG key:
```
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg
```

Use the following command to set up the stable repository:
```
echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
```

Update `apt` package index, and install the latest version of Docker Engine and containerd:
```
sudo apt-get update
sudo apt-get install docker-ce docker-ce-cli containerd.io
```

Verify that Docker Engine is installed correctly by running the hello-world image:
```
sudo docker run hello-world
```

### Note to Users

The installation steps provided above are a general guide. Due to the rapid development of Docker and the differences between system configurations, it's recommended to refer to the official Docker documentation for the most accurate and detailed instructions:

- Docker Documentation: https://docs.docker.com/get-docker/

This ensures that users follow the latest guidelines and troubleshooting steps directly from Docker, accommodating any recent changes or system-specific requirements.

## Running Bash Scripts on Windows

To run bash scripts on Windows, you will need to use a Unix-like environment. The most common approach is to install Git Bash or enable the Windows Subsystem for Linux (WSL).

### Git Bash:

- Download and install Git for Windows from https://git-scm.com/download/win.
- During installation, ensure you select the option to use Git and optional Unix tools from the Windows Command Prompt.
- After installation, you can right-click in any folder in Windows Explorer and select "Git Bash Here" to open a Bash terminal in that directory.

### Windows Subsystem for Linux (WSL):

- Open PowerShell as Administrator and run: `wsl --install`.
- Follow the instructions to complete the installation of your preferred Linux distribution from the Microsoft Store.
- Once installed, you can access Linux terminals directly from Windows.

For detailed instructions on setting up WSL, visit the Microsoft documentation: https://docs.microsoft.com/en-us/windows/wsl/install.

## Running Bash Scripts on Mac

MacOS comes with a built-in Terminal application that supports running bash scripts out-of-the-box.

- Open the Terminal app from your Applications folder or by using Spotlight search (`Cmd + Space`, then type "Terminal").
- Navigate to the directory containing your bash script using the `cd` command.
- To run your script, type `bash script_name.sh`, replacing "script_name.sh" with the name of your script.

Ensure you have the necessary permissions to execute the script. If not, run `chmod +x script_name.sh` before executing it.

## Running the main script

To streamline the setup and execution of the Cardano Developer Studio tools, we provide a utility script, `run.sh`, located in the `scripts` directory. This script simplifies the management of Docker Compose workflows, allowing for an intuitive selection of different configurations for development purposes.

### How to Use `run.sh`

- **Open a terminal** and navigate to the root directory of the Cardano Developer Studio project.
- **Execute the script** by running the following command:

```
bash scripts/run.sh
```

## Main Menu

The main menu will provide you with the following options:

- `1) Docker Compose Workflow`: Initiates the Docker Compose workflow, allowing you to select which components to run (e.g., Cardano Node, Cardano Wallet).
- `2) Cardano Node Testing and Tools`: Access tools related to the Cardano Node.
- `3) Cardano Wallet Testing and Tools`: Manage wallets and perform wallet-related operations.
- `4) Cardano DB Sync Tools`: Execute queries and interact with the Cardano DB Sync.
- `5) Other Tool [Placeholder]`: Reserved for future tools and utilities.
- `6) Exit`: Exits the script.
  
### Docker Compose Menu

After selecting the Docker Compose Workflow, you will be presented with another menu to choose the specific component you wish to run:
- `1) Cardano Node`: Starts the Cardano Node container with the configured environment.
- `2) Cardano Wallet`: Initiates the Cardano Wallet container setup.
- `3) Cardano DB Sync`: Begins the Cardano DB Sync container.
- `4) Exit`: Returns to the main menu.

For each selection, you will be prompted to enter environment variables such as `CARDANO_NODE_VERSION`, `CARDANO_NETWORK`, `CARDANO_NODE_DB_PATH`, and `CARDANO_NODE_PORT`. Default values are provided, but you may customize them as needed.

### Cardano Node Tools Menu

Selecting option `2` from the main menu, you can:

- `1) Cardano Node Version`: Display the version of the Cardano Node.
- `2) Cardano CLI Query Tip`: Query the current tip of the blockchain.
- `3) Exit to container selection`: Return to the container selection menu.

### Cardano Wallet Tools Menu

Selecting option `3` from the main menu, the wallet tools menu allows you to:

- `1) Generate mnemonic`: Create a new recovery phrase.
- `2) Generate mnemonic and create wallet`: Create a new wallet with a generated mnemonic.
- `3) List wallets`: Display all created wallets.
- `4) Fetch network information`: Get current network information.
- `5) Exit to container selection`: Return to the container selection menu.

### Cardano DB Sync Tools Menu

By choosing option `4` from the main menu, the DB Sync tools menu provides:

- `1) Sync progress of db-sync`: Check the synchronization progress of DB Sync.
- `2) Retrieving most recent block`: Fetch the most recent block from the blockchain.
- `3) Slot number of the most recent block`: Query the slot number of the latest block.
- `4) Current total on-chain supply of Ada`: Calculate the current total supply of ADA.
- `5) Exit to container selection`: Return to the container selection menu.

### Note:

- Before running the `run.sh` script, ensure Docker is installed and running on your system. Follow the installation instructions provided in the previous sections for your respective operating system.
- The `compose.sh` script, invoked by `run.sh`, handles the intricacies of configuring and starting the Docker Compose services based on your selections. It automatically sets necessary environment variables and permissions to ensure a smooth setup experience.

## Using Docker Containers

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

##### Import the Collection:

Click on the Import button, which can be found at the top left corner of the Postman interface.
In the dialog that appears, go to the File tab and click on Upload Files.
Browse to the location of the `cardano-wallet-API.json` file located in `configs/cardano-wallet` folder of the project, select it, and click Open to start the import process.

##### Using the Collection:

After the import is complete, the Cardano Wallet Backend API collection will appear in the Collections sidebar on the left.

Ensure you set the base URL to `http://localhost:8090/v2` in your Postman environment settings to match your local Cardano Wallet Backend instance, or update it according to your specific setup.

Expand the collection to view the API requests it contains. You can click on any request to load it into the active tab.Before sending a request, make sure to adjust any necessary parameters or request bodies as per your testing needs. Hit the Send button to execute the request and view the response from the Cardano Wallet Backend.

This collection provides a comprehensive set of API calls, enabling you to interact with the Cardano Wallet Backend for various operations such as wallet management, transaction creation, and blockchain queries. Using Postman collections can significantly streamline your workflow and facilitate efficient testing and integration development.

**Example API queries:**

http://localhost:8090/v2/network/information

http://localhost:8090/v2/wallets


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

Replace <address> with the actual Cardano address you want to retrieve the balance for. This query calculates the total balance of an address by summing the values of its UTxOs.

- Querying stake distribution:

This query retrieves the total stake for each stake pool.

```
select pool_id, sum (amount) from epoch_stake group by pool_id ;
```

## Contribution

Contributions to the Cardano Developer Studio are welcome. Whether you're looking to fix bugs, add new features, or improve documentation, your help is appreciated. Please see our contribution guidelines for more information.

## License

This project is licensed under the GNU General Public License v3.0 - see the [LICENSE](LICENSE) file for details.


## Acknowledgements

We express our deepest gratitude to the Cardano community for their unwavering support and valuable contributions to this project. This work is part of a funded project through Cardano Catalyst, a community-driven innovation platform. For more details on the proposal and its progress, please visit our proposal page on [IdeaScale](https://cardano.ideascale.com/c/idea/110047).
