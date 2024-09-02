# FrontEnd Application 

This project is an example web application that connects to the Cardano blockchain using the Blockfrost API. It is designed to help developers understand how to integrate Blockfrost into their own applications and interact with Plutus smart contracts.

## Overview

The frontend is developed using React and communicates with the Cardano blockchain through Blockfrost. It enables users to interact with smart contracts by providing an intuitive interface for sending transactions, checking contract states, and viewing blockchain data.

## Acknowledgements

This project is inspired by and reuses a significant amount of code from the Plutus Pioneer Program by Input Output HK (IOHK). We want to express our gratitude to the IOHK team for their invaluable contributions to the Cardano ecosystem and for making their code available to the community. All credit for the original concepts and implementations goes to the Plutus Pioneer Program and its contributors.

https://github.com/input-output-hk/plutus-pioneer-program

## Prerequisites

Before setting up the frontend, ensure that you have the following installed:

- Node.js (version 14 or higher)
- npm or Yarn package manager
- A Blockfrost API key
- Compatible wallet extension (Nami or Eternl)
- Testnet ADA for transactions

### Installing a Compatible Wallet Extension

To interact with the Cardano blockchain, you'll need a compatible wallet extension. We recommend either Nami or Eternl:

1. **Nami Wallet**:
   - Visit the [Nami Wallet Chrome Extension page](https://chromewebstore.google.com/detail/nami/lpfcbjknijpeeillifnkikgncikgfhdo)
   - Click "Add to Chrome" and follow the installation prompts
   - Once installed, click the extension icon and follow the setup instructions to create or import a wallet

2. **Eternl Wallet**:
   - Go to the [Eternl Wallet Chrome Extension page](https://chromewebstore.google.com/detail/eternl/kmhcihpebfmpgmihbkipmjlmmioameka)
   - Click "Add to Chrome" and complete the installation process
   - After installation, open the extension and follow the prompts to set up your wallet

After installation, ensure your wallet is set to the Cardano testnet network for this example application.

### Obtaining Testnet ADA

To interact with smart contracts and make transactions on the testnet, you'll need some test ADA. You can obtain this for free from the Cardano testnet faucet:

1. Visit the [Cardano Testnet Faucet](https://docs.cardano.org/cardano-testnets/tools/faucet/)
2. Select the appropriate testnet (e.g., "Preprod" or "Preview")
3. Enter your wallet's testnet address
4. Complete the CAPTCHA and submit the request
5. You should receive the test ADA in your wallet within a few minutes

Remember, testnet ADA has no real-world value and is only for testing purposes.

## What is Blockfrost?

Blockfrost is a robust API that allows developers to interact with the Cardano blockchain. It provides access to various blockchain data, enabling you to query information about blocks, transactions, addresses, and more.

To use Blockfrost, you'll need to generate an API key, which will authenticate your requests to the Blockfrost service.

### Generating a Blockfrost API Key

To use this example application, you'll need to generate a Blockfrost API key for the `preview` environment:

1. Visit the [Blockfrost Dashboard](https://blockfrost.io/dashboard).
2. Create an account if you haven't already.
3. Navigate to the "Projects" section.
4. Create a new project and select the `preview` network.
5. Copy the API key provided by Blockfrost.

## Key Features

- **User-Friendly Interface**: Simplifies interaction with Plutus smart contracts.
- **Blockfrost Integration**: Seamlessly connects to the Cardano blockchain using the Blockfrost API.
- **Transaction Management**: Allows users to create, sign, and submit transactions directly from the frontend.
- **Lucid Library Integration**: Demonstrates how to interact with Plutus smart contracts using the Lucid library.
- **Wallet Compatibility**: Works with popular Cardano wallet extensions like Nami and Eternl.

## Setup Instructions

To run the example application, follow these steps:

1. **Navigate to the project directory**:

   ```bash
   cd examples/Off-chain/
   ```

2. **Install the required dependencies**:

   ```bash
   npm install
   ```

   or if you're using Yarn:

   ```bash
   yarn install
   ```

3. **Configure the Blockfrost API key**:
   - Copy `.env.example` located in the `examples/Off-chain/` directory to `.env.local`

   ```bash
   cp .env.example .env.local
   ```
   - Open the `.env.local` file 
   - Find the line `BLOCKFROST_PREVIEW=YOUR_PREVIEW_KEY_HERE`.
   - Paste your Blockfrost API key after the equals sign, like so:

4. **Run the development server**:

   ```bash
   npm run dev
   ```

   or with Yarn:

   ```bash
   yarn dev
   ```

5. **Access the web application**:
   - Open your web browser and go to [http://localhost:3000](http://localhost:3000).

Now you can interact with the Cardano blockchain through the example application.

## Project Structure

```
/examples/Off-chain
├── src/
│   ├── components/
│   ├── pages/
│   ├── utils/
│   ├── App.js
│   └── index.js
├── public/
├── .env
└── package.json
```

## Usage

1. Ensure your wallet extension (Nami or Eternl) is installed and set to the testnet network.
2. Connect your wallet using the provided interface in the application.
3. View your ADA balance and other blockchain data.
4. Interact with deployed Plutus smart contracts.
5. Create, sign, and submit transactions using your testnet ADA.

## Additional Configuration

- **Customizing the UI**: Modify the React components in the `src` directory to customize the look and feel of the frontend.
- **Extending Functionality**: Add new features or integrate additional APIs to extend the functionality of the frontend as needed.
- **Wallet Integration**: The application is set up to work with Nami and Eternl wallets. If you want to add support for other wallets, you'll need to modify the wallet connection logic in the relevant components.

## Development

- To customize the UI, modify React components in `src/components/`.
- Add new pages in `src/pages/` for additional functionality.
- Use `src/utils/` for helper functions and API calls.
- When testing transactions, always use small amounts of testnet ADA to avoid any potential issues.

## Troubleshooting

- Ensure your Blockfrost API key is correctly set in the `.env` file.
- Verify that the wallet extension (Nami or Eternl) is properly installed and set up in your browser.
- Check that your wallet is connected to the correct testnet network.
- Confirm you have sufficient testnet ADA in your wallet for transactions.
- Check the browser console for any error messages if you encounter issues.
- If transactions are failing, double-check the smart contract addresses and parameters.

## Additional Resources

- [Cardano Developer Portal](https://developers.cardano.org/)
- [Blockfrost Documentation](https://docs.blockfrost.io/)
- [Lucid Library Documentation](https://lucid.spacebudz.io/)
- [Nami Wallet GitHub](https://github.com/berry-pool/nami)
- [Nami Wallet Extension](https://chromewebstore.google.com/detail/nami/lpfcbjknijpeeillifnkikgncikgfhdo)
- [Eternl Wallet Extension](https://chromewebstore.google.com/detail/eternl/kmhcihpebfmpgmihbkipmjlmmioameka)
- [Cardano Testnet Faucet](https://docs.cardano.org/cardano-testnets/tools/faucet/)

## Contributing

Contributions to improve this example are welcome! Please feel free to submit issues, fork the repository and send pull requests.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

For any questions or support, please open an issue in the GitHub repository.


