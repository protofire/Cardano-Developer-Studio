# Cardano Web3 Frontend Template

## Overview

This project is an example web application that connects to the Cardano blockchain. It is designed to help developers understand how to develop their own applications and interact with Plutus smart contracts.

The frontend is developed using React and communicates with the Cardano blockchain through Blockfrost. It provides an intuitive interface for sending transactions, checking contract states, and viewing blockchain data.

## Table of Contents

- [Cardano Web3 Frontend Template](#cardano-web3-frontend-template)
  - [Overview](#overview)
  - [Table of Contents](#table-of-contents)
  - [Quick Start](#quick-start)
  - [Detailed Usage Guide](#detailed-usage-guide)
    - [Using the Template within Developer Studio](#using-the-template-within-developer-studio)
    - [Using the Template as a Standalone Project](#using-the-template-as-a-standalone-project)
  - [Prerequisites](#prerequisites)
  - [Key Features](#key-features)
  - [What is Blockfrost?](#what-is-blockfrost)
    - [Generating a Blockfrost API Key](#generating-a-blockfrost-api-key)
  - [Wallet Integration](#wallet-integration)
    - [Installing a Compatible Wallet Extension](#installing-a-compatible-wallet-extension)
    - [Obtaining Testnet ADA](#obtaining-testnet-ada)
  - [Project Structure](#project-structure)
  - [Usage](#usage)
  - [Development](#development)
  - [CI/CD Pipeline](#cicd-pipeline)
  - [Docker Integration](#docker-integration)
  - [Deploying Your Application](#deploying-your-application)
  - [Troubleshooting](#troubleshooting)
  - [Additional Resources](#additional-resources)
  - [Contributing](#contributing)
  - [License](#license)

## Quick Start

1. Clone the repository:
   ```
   git clone https://github.com/protofire/Cardano-Developer-Studio.git
   cd Cardano-Developer-Studio/cardano-web3-frontend-template
   ```

2. Install dependencies:
   ```
   npm install
   ```
   or if you're using Yarn:
   ```
   yarn install
   ```

3. Set up your Blockfrost API key in `.env.local` (see [Generating a Blockfrost API Key](#generating-a-blockfrost-api-key))
   - Copy `.env.example` to `.env.local`:
     ```
     cp .env.example .env.local
     ```
   - Open the `.env.local` file
   - Find the line `BLOCKFROST_PREVIEW=YOUR_PREVIEW_KEY_HERE`.
   - Replace `YOUR_PREVIEW_KEY_HERE` with your Blockfrost API key.

4. Run the development server:
   ```
   npm run dev
   ```
   or with Yarn:
   ```
   yarn dev
   ```

5. Open [http://localhost:3000](http://localhost:3000) in your browser

## Detailed Usage Guide

### Using the Template within Developer Studio

1. Navigate to the GitHub repository: [https://github.com/protofire/Cardano-Developer-Studio](https://github.com/protofire/Cardano-Developer-Studio)
2. Clone the repository:
   ```
   git clone https://github.com/protofire/Cardano-Developer-Studio.git
   cd Cardano-Developer-Studio/cardano-web3-frontend-template
   ```
3. Follow the [Quick Start](#quick-start) guide from step 2

Note: The CI/CD pipeline is configured to trigger only when changes are made in the `cardano-web3-frontend-template` folder.

### Using the Template as a Standalone Project

1. Navigate to the GitHub repository: [https://github.com/protofire/Cardano-Developer-Studio](https://github.com/protofire/Cardano-Developer-Studio)
2. Click on the "Use this template" button
3. Choose a name for your new repository and create it
4. Clone your new repository:
   ```
   git clone https://github.com/your-username/your-new-repo.git
   cd your-new-repo
   ```
5. If you want to keep only the frontend template:
   ```
   mv cardano-web3-frontend-template/* .
   mv cardano-web3-frontend-template/.* .
   rm -rf cardano-web3-frontend-template
   ```
6. Update the CI/CD configuration:
   - Edit `.github/workflows/ci.yml` and `.github/workflows/cd.yml`
   - Remove the `paths` filter and the `working-directory` setting
7. Follow the [Quick Start](#quick-start) guide from step 2

## Prerequisites

- Node.js (version 14 or higher)
- npm or Yarn package manager
- A Blockfrost API key
- Compatible wallet extension (e.g., Nami, Eternl, Flint, Yoroi, Typhon, Nufi)
- Testnet ADA for transactions
- Docker (for containerized deployment)
- Docker account (for pushing and pulling Docker images)
- Git (for version control and CI/CD integration)
- GitHub account (for repository management and CI/CD)

## Key Features

- **User-Friendly Interface**: Simplifies interaction with Plutus smart contracts.
- **Blockfrost Integration**: Seamlessly connects to the Cardano blockchain using the Blockfrost API.
- **Transaction Management**: Allows users to create, sign, and submit transactions directly from the frontend.
- **Lucid Library Integration**: Demonstrates how to interact with Plutus smart contracts using the Lucid library.
- **Multi-Wallet Compatibility**: Works with multiple popular Cardano wallet extensions.
- **CI/CD Integration**: Automated build, test, and deployment processes using GitHub Actions.
- **Docker Support**: Production-ready Docker images for easy deployment and scaling.

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

## Wallet Integration

This template supports multiple Cardano wallets. The wallet connector component allows users to connect their preferred wallet or install a new one if it's not already available. Supported wallets include:

- Eternl
- Nami
- Flint
- Yoroi
- Typhon
- Nufi

Users can easily connect their wallet or install a new one directly from the application interface.

### Installing a Compatible Wallet Extension

To interact with the Cardano blockchain, you'll need a compatible wallet extension. Some recomendatios are:

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

## Project Structure

```
/cardano-web3-frontend-template/
├── src/
│   ├── components/
│   ├── pages/
│   ├── utils/
│   ├── App.js
│   └── index.js
├── public/
├── .github/
│   └── workflows/
│       ├── ci.yml
│       └── cd.yml
├── Dockerfile
├── docker-compose.yml
├── .env
└── package.json
```

## Usage

1. Ensure your chosen wallet extension is installed and set to the testnet network.
2. Open the web application at [http://localhost:3000](http://localhost:3000).
3. Use the wallet connector to connect your preferred wallet.
4. Once connected, you can view your ADA balance and other blockchain data.
5. Interact with deployed Plutus smart contracts through the user interface.
6. Create, sign, and submit transactions using your testnet ADA.

## Development

- Customize the UI by modifying React components in `src/components/`
- Add new pages in `src/pages/`
- Use `src/utils/` for helper functions and API calls
- When testing transactions, always use small amounts of testnet ADA

## CI/CD Pipeline

This project uses GitHub Actions for Continuous Integration and Continuous Deployment (CI/CD).

1. CI Pipeline (`ci.yml`):
   - Triggered on push or pull request to the main branch
   - Runs tests and builds the project
   - For the Developer Studio repo, only triggers on changes in the `cardano-web3-frontend-template` folder

2. CD Pipeline (`cd.yml`):
   - Triggered when a new release (tag) is created
   - Builds and pushes a Docker image to Docker Hub
   - For the Developer Studio repo, only builds the `cardano-web3-frontend-template` folder

To set up CI/CD:

1. Navigate to your repository on GitHub
2. Go to "Settings" > "Secrets and variables" > "Actions"
3. Add the following secrets:
   - `DOCKERHUB_USERNAME`: Your Docker Hub username
   - `DOCKERHUB_TOKEN`: Your Docker Hub access token
   - `BLOCKFROST_PROJECT_ID`: Your Blockfrost project ID

4. If using the template as a standalone project, update the YAML files as described in the [Using the Template as a Standalone Project](#using-the-template-as-a-standalone-project) section

## Docker Integration

1. Create a Docker Hub account if you haven't already: [https://hub.docker.com/](https://hub.docker.com/)
2. Create a new repository on Docker Hub for your project
3. Update the `cd.yml` file with your Docker Hub repository name

To build and push your Docker image manually:

1. Build the Docker image:
   ```
   docker build -t your-dockerhub-username/your-repo-name:latest .
   ```
2. Push the image to Docker Hub:
   ```
   docker push your-dockerhub-username/your-repo-name:latest
   ```

## Deploying Your Application

1. Ensure your CI/CD pipeline is set up correctly (see [CI/CD Pipeline](#cicd-pipeline) section)
2. Make changes to your code and push to the `main` branch
3. The CI pipeline will automatically run tests
4. If tests pass and you're ready to deploy, create a new release:
   ```
   git tag v1.0.0
   git push origin v1.0.0
   ```
5. The CD pipeline will build a new Docker image and push it to Docker Hub
6. You can now pull this image on your deployment server:
   ```
   docker pull your-dockerhub-username/your-repo-name:latest
   ```
7. Run the container:
   ```
   docker run -p 3000:3000 your-dockerhub-username/your-repo-name:latest
   ```
   
## Troubleshooting

- Ensure your Blockfrost API key is correctly set in the `.env.local` file
- Verify that your chosen wallet extension is properly installed and set up
- Check that your wallet is connected to the correct testnet network
- Confirm you have sufficient testnet ADA in your wallet for transactions
- Check the browser console for any error messages
- For CI/CD issues, check the GitHub Actions logs for detailed error messages
- Ensure all required secrets are properly set in your GitHub repository settings
- Verify that your Docker Hub credentials are correct and you have permission to push to the specified repository

## Additional Resources

- [Cardano Developer Portal](https://developers.cardano.org/)
- [Blockfrost Documentation](https://docs.blockfrost.io/)
- [Lucid Library Documentation](https://lucid.spacebudz.io/)
- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [Docker Documentation](https://docs.docker.com/)
- [Nami Wallet GitHub](https://github.com/berry-pool/nami)
- [Nami Wallet Extension](https://chromewebstore.google.com/detail/nami/lpfcbjknijpeeillifnkikgncikgfhdo)
- [Eternl Wallet Extension](https://chromewebstore.google.com/detail/eternl/kmhcihpebfmpgmihbkipmjlmmioameka)
- [Cardano Testnet Faucet](https://docs.cardano.org/cardano-testnets/tools/faucet/)

## Contributing

Contributions to improve this example are welcome! Please feel free to submit issues, fork the repository and send pull requests.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

For any questions or support, please open an issue in the GitHub repository.