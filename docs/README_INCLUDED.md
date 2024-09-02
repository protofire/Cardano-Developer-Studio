### Table of Contents
- [Table of Contents](#table-of-contents)
- [Cardano Node](#cardano-node)
- [Cardano Wallet](#cardano-wallet)
- [Cardano DB Sync](#cardano-db-sync)
- [Smart Contracts Examples](#smart-contracts-examples)
- [Haskell Helpers Library](#haskell-helpers-library)
- [Testing Suite](#testing-suite)
- [Toolbox CLI](#toolbox-cli)
- [Dockerized Tools Ready for Use](#dockerized-tools-ready-for-use)
- [Integrated Web3 Frontend Template](#integrated-web3-frontend-template)
- [CI/CD Pipeline and Docker Integration](#cicd-pipeline-and-docker-integration)

### Cardano Node

Operating a Cardano Node is essential for developers as it serves as the entry point to the Cardano blockchain. It enables you to create and submit transactions, query blockchain state, and engage in consensus. For dApp development, the Cardano Node provides the foundational infrastructure necessary for blockchain interaction, ensuring that applications are seamlessly integrated into the Cardano ecosystem. Understanding node operations is also key for optimizing network participation, whether for personal use or stake pool management.

See [Github](https://github.com/intersectmbo/cardano-node/)

### Cardano Wallet

More than just a cryptocurrency storage, the Cardano Wallet is a critical tool for managing ADA transactions and staking operations. Integrating wallet functionalities into applications enhances user experience by offering secure and straightforward transaction capabilities. For developers, grasping wallet operations such as key management and wallet restoration is crucial for building secure and robust applications on Cardano.

See [Github](https://github.com/cardano-foundation/cardano-wallet/releases)

### Cardano DB Sync

Cardano DB Sync enables developers to access and utilize blockchain data effectively. This tool is crucial for applications requiring detailed blockchain analytics, financial tools, or interfaces that interact with blockchain data. DB Sync facilitates complex queries and provides access to historical data, offering deep insights into blockchain activities and user transactions. This capability supports better business decisions, comprehensive analytics services, and enriched application functionalities.

See [Github](https://github.com/intersectmbo/cardano-db-sync/pkgs/container/cardano-db-sync)

### Smart Contracts Examples

The **Smart Contracts Examples** feature a collection of practical Plutus smart contract examples that demonstrate various functionalities. Each example provides a detailed guide on implementing specific use cases, helping developers understand, deploy, and interact with smart contracts on the Cardano blockchain.

- **Validator Examples**: Showcases contracts that always return true or false, perform date and signature validation, and more.
- **Policy Examples**: Includes examples for minting policies, covering both fungible and non-fungible tokens.

### Haskell Helpers Library

The **Haskell Helpers Library** provides a set of utility functions to streamline the development of smart contracts in Plutus. These utilities cover common tasks throughout the smart contract lifecycle, such as input validation and building complex transactions. The library is organized into modules that focus on different development aspects:

- **On-Chain Helpers**: Functions that execute on-chain, supporting smart contract validation and logic.
- **Off-Chain Helpers**: Tools for constructing transactions and managing blockchain interactions from off-chain environments.
- **Evaluation Helpers**: Utilities for simulating and testing the execution of smart contracts, ensuring they function correctly before deployment.

### Testing Suite

Our **Testing Suite** ensures that each smart contract example adheres to Plutus constraints, particularly in terms of resource usage and transaction sizes. This suite is essential for optimizing contracts and preventing issues during blockchain interactions. Each smart contract example comes with a set of tests implemented using the Tasty testing framework, providing comprehensive coverage to verify contract behavior.

- **Integrated Tasty Tests**: Each smart contract example includes Tasty-based tests, verifying its functionality against expected behaviors. These tests help detect issues early and confirm that contracts perform as intended.
- **Resource Usage and Transaction Size**: The tests also focus on assessing the resource consumption and transaction sizes of contracts, ensuring compliance with Cardano's operational constraints. This preemptively addresses potential problems when deploying or interacting with contracts on the blockchain.

### Toolbox CLI

The **Toolbox CLI** is an intuitive, menu-driven command-line interface designed to simplify the use of various development tools. It provides easy navigation and execution of tasks related to the Cardano Node, Wallet, DB Sync, and other utilities, all directly from the terminal.

### Dockerized Tools Ready for Use

All tools are fully dockerized for convenience and scalability. Whether using Visual Studio Code or another IDE, Docker provides a consistent and isolated development environment. Our Docker setup includes containers for:

- **Cardano Node**: Interact directly with the Cardano blockchain.
- **DB Sync**: Access and analyze blockchain data.
- **Wallet Server**: Manage ADA transactions and staking.
- **Kupo**: A lightweight Cardano chain indexer.
- **Ogmios**: A WebSocket server for Cardano node integration.
- **Cardano Development Container**: Pre-configured for compiling Haskell code with Cabal and GHC, ideal for smart contract development.

### Integrated Web3 Frontend Template

Included is a comprehensive dApp frontend template that integrates seamlessly with the Cardano blockchain, enabling developers to rapidly build and deploy decentralized applications.

### CI/CD Pipeline and Docker Integration

Utilize our GitHub Actions-based CI/CD pipelines for automated testing, building, and deployment of smart contracts and frontend applications. With Docker support, easily create production-ready images, enabling consistent and scalable deployment across environments.
