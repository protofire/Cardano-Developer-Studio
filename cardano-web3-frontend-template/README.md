# Cardano Web3 Frontend Template

## Overview

This project is an example web application that connects to the Cardano blockchain. It is designed to help developers understand how to develop their own applications and interact with Plutus smart contracts.

The frontend is developed using React and communicates with the Cardano blockchain through Blockfrost. It provides an intuitive interface for sending transactions, checking contract states, and viewing blockchain data.

## Table of Contents

- [Cardano Web3 Frontend Template](#cardano-web3-frontend-template)
  - [Overview](#overview)
  - [Table of Contents](#table-of-contents)
  - [Quick Start](#quick-start)
  - [Prerequisites](#prerequisites)
    - [Account Setup](#account-setup)
  - [Key Features](#key-features)
  - [What is Blockfrost?](#what-is-blockfrost)
    - [Generating a Blockfrost API Key](#generating-a-blockfrost-api-key)
  - [Wallet Integration](#wallet-integration)
    - [Installing a Compatible Wallet Extension](#installing-a-compatible-wallet-extension)
    - [Obtaining Testnet ADA](#obtaining-testnet-ada)
  - [Project Structure](#project-structure)
  - [Website Usage](#website-usage)
  - [Development](#development)
  - [CI/CD Pipeline](#cicd-pipeline)
    - [Continuous Integration (CI)](#continuous-integration-ci)
    - [Continuous Deployment (CD)](#continuous-deployment-cd)
  - [Detailed Usage Guide](#detailed-usage-guide)
    - [Using the Template within Developer Studio](#using-the-template-within-developer-studio)
    - [Using the Template as a Standalone Project](#using-the-template-as-a-standalone-project)
      - [Step 1: Clone the Cardano Developer Studio Repository](#step-1-clone-the-cardano-developer-studio-repository)
      - [Step 2: Copy the Template to a New Directory](#step-2-copy-the-template-to-a-new-directory)
      - [Step 3: Initialize a New Git Repository](#step-3-initialize-a-new-git-repository)
      - [Step 4: Set Up Project](#step-4-set-up-project)
    - [Set Up CI/CD for the Standalone Project](#set-up-cicd-for-the-standalone-project)
      - [Step 1: Verify Workflow Files](#step-1-verify-workflow-files)
      - [Step 2: Configure Docker Hub and GitHub Secrets](#step-2-configure-docker-hub-and-github-secrets)
    - [Step 3: Triggering CI Workflow](#step-3-triggering-ci-workflow)
    - [Step 4: Triggering CD Workflow](#step-4-triggering-cd-workflow)
    - [Step 5: Manual Docker Image Creation (Optional)](#step-5-manual-docker-image-creation-optional)
    - [Step 6: Using the Docker Image](#step-6-using-the-docker-image)
  - [Troubleshooting](#troubleshooting)
  - [Additional Resources](#additional-resources)
  - [Contributing](#contributing)
  - [License](#license)

## Quick Start

This quick start guide assumes you're using the template within the Cardano Developer Studio repository. For standalone usage, see the [Detailed Usage Guide](#detailed-usage-guide).

1. Clone the Cardano Developer Studio repository:

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

4. Run the development server:

   ```
   npm run dev
   ```

   or with Yarn:

   ```
   yarn dev
   ```

5. Open [http://localhost:3000](http://localhost:3000) in your browser.

6. See [Website Usage](#website-usage) 

Note: The CI/CD pipeline is not active when using the template within the Cardano Developer Studio repository.

## Prerequisites

- Node.js (version 14 or higher)
- npm or Yarn package manager
- A Blockfrost API key
- Compatible wallet extension (e.g., Nami, Eternl, Flint, Yoroi, Typhon, Nufi)
- Testnet ADA for transactions
- Docker (for containerized deployment when using as a standalone project)
- Git (for version control)
- GitHub account (for repository management and CI/CD when using as a standalone project)
- Docker Hub account (for storing Docker images)

### Account Setup

1. **GitHub Account**: Required for version control and CI/CD workflows. If you don't have one, create it [here](https://github.com/).
2. **Docker Hub Account**: Required for storing and managing Docker images. If you don't have one, create it [here](https://hub.docker.com/).

## Key Features

- **User-Friendly Interface**: Simplifies interaction with Plutus smart contracts.
- **Blockfrost Integration**: Seamlessly connects to the Cardano blockchain using the Blockfrost API.
- **Transaction Management**: Allows users to create, sign and submit transactions directly from the frontend.
- **Lucid Library Integration**: Demonstrates how to interact with Plutus smart contracts using the Lucid library.
- **Multi-Wallet Compatibility**: Works with multiple popular Cardano wallet extensions.
- **CI/CD Integration**: Automated build, test, and deployment processes using GitHub Actions (when used as a standalone project).
- **Docker Support**: Production-ready Docker images for easy deployment and scaling (when used as a standalone project).

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
6. Create a `.env.local` file in the project root:

   ```
   cp .env.example .env.local
   ```

7. Add your Blockfrost API key:
   - Open the `.env.local` file.
   - Find the line `BLOCKFROST_PREVIEW=YOUR_PREVIEW_KEY_HERE`.
   - Replace `YOUR_PREVIEW_KEY_HERE` with your Blockfrost API key.

## Wallet Integration

This template supports multiple Cardano wallets. The wallet connector component allows users to connect their preferred wallet or install a new one if it's not already available. Supported wallets include:

- Eternl
- Nami
- Flint
- Yoroi
- Typhon
- Nufi

The application provides an "Install" button for each wallet, which directs users to the appropriate extension page for easy installation.

### Installing a Compatible Wallet Extension

To interact with the Cardano blockchain, you'll need a compatible wallet extension. Some recommendations are:

1. **Nami Wallet**:
   - Visit the [Nami Wallet Chrome Extension page](https://chromewebstore.google.com/detail/nami/lpfcbjknijpeeillifnkikgncikgfhdo).
   - Click "Add to Chrome" and follow the installation prompts.
   - Once installed, click the extension icon and follow the setup instructions to create or import a wallet.

2. **Eternl Wallet**:
   - Go to the [Eternl Wallet Chrome Extension page](https://chromewebstore.google.com/detail/eternl/kmhcihpebfmpgmihbkipmjlmmioameka).
   - Click "Add to Chrome" and complete the installation process.
   - After installation, open the extension and follow the prompts to set up your wallet.

After installation, ensure your wallet is set to the Cardano testnet network for this example application.

### Obtaining Testnet ADA

To interact with smart contracts and make transactions on the testnet, you'll need some test ADA. You can obtain this for free from the Cardano testnet faucet:

1. Visit the [Cardano Testnet Faucet](https://docs.cardano.org/cardano-testnets/tools/faucet/).
2. Select the appropriate testnet (e.g., "Preprod" or "Preview").
3. Enter your wallet's testnet address.
4. Complete the CAPTCHA and submit the request.
5. You should receive the test ADA in your wallet within a few minutes.

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
├── .env.example
├── .env.local (you need to create this)
└── package.json
```

## Website Usage

1. Ensure your chosen wallet extension is installed and set to the testnet network.
2. Open the web application at [http://localhost:3000](http://localhost:3000).
3. Use the wallet connector to connect your preferred wallet. If you don't have a compatible wallet installed, use the provided "Install" buttons to set one up.
4. Once connected, you can view your ADA balance and other blockchain data.
5. Interact with deployed Plutus smart contracts through the user interface.
6. Create, sign, and submit transactions using your testnet ADA.

## Development

- Customize the UI by modifying React components in `src/components/`.
- Add new pages in `src/pages/`.
- Use `src/utils/` for helper functions and API calls.
- When testing transactions, always use small amounts of testnet ADA.

## CI/CD Pipeline

Note: The CI/CD pipeline is only functional when this template is used as a standalone project, not within the Cardano Developer Studio repository.

This project uses GitHub Actions for Continuous Integration and Continuous Deployment (CI/CD) when set up as a standalone repository.

### Continuous Integration (CI)

The CI workflow (`ci.yml`) automates the testing and building process:

- Triggered on every push to the main branch and on pull requests.
- Sets up the Node.js environment.
- Installs project dependencies.
- Runs automated tests.
- Builds the project to verify successful compilation.

Benefits of CI:
- Early detection of integration issues.
- Maintains a consistently deployable codebase.
- Provides quick feedback to developers about their changes.

### Continuous Deployment (CD)

The CD workflow (`cd.yml`) automates the deployment process:

- Triggered when a new release is created (i.e., when a new tag is pushed).
- Builds a Docker image of your application.
- Pushes the Docker image to Docker Hub.

Benefits of CD:
- Automates the deployment process, reducing manual errors.
- Ensures consistent deployment across different environments.
- Enables rapid and frequent releases.

## Detailed Usage Guide

### Using the Template within Developer Studio

1. Follow the [Quick Start](#quick-start) guide.
2. The template is ready for development and testing within the Cardano Developer Studio environment.
3. Note that the CI/CD workflows will not run automatically in this setup.

### Using the Template as a Standalone Project

To use this template with CI/CD capabilities:

#### Step 1: Clone the Cardano Developer Studio Repository

1. Go to the GitHub repository for Cardano Developer Studio: [https://github.com/protofire/Cardano-Developer-Studio](https://github.com/protofire/Cardano-Developer-Studio).
2. Click on the green "Code" button and copy the repository URL.
3. Clone the repository to your local machine:
   ```
   git clone https://github.com/protofire/Cardano-Developer-Studio.git
   ```
4. Change to the project directory:
   ```
   cd Cardano-Developer-Studio
   ```

#### Step 2: Copy the Template to a New Directory

1. Navigate to the `cardano-web3-frontend-template` folder:
   ```
   cd cardano-web3-frontend-template
   ```
2. Create a new directory outside of the Cardano Developer Studio project:
   ```
   mkdir ../../my-frontend-project
   cp -r ./* ../../my-frontend-project
   cp -r ./.* ../../my-frontend-project
   cd ../../my-frontend-project
   ```

#### Step 3: Initialize a New Git Repository

1. Initialize a new Git repository:
   ```
   git init
   ```
2. Create a new repository on GitHub via the website.
3. Add the remote origin to your new repository:
   ```
   git remote add origin https://github.com/your-username/your-new-repo.git
   ```
4. Commit and push your changes:
   ```
   git add .
   git commit -m "Initial commit"
   git push -u origin main
   ```

#### Step 4: Set Up Project

Follow the [Quick Start](#quick-start) guide from step 2 to set up and run your project locally.

### Set Up CI/CD for the Standalone Project

#### Step 1: Verify Workflow Files

Ensure that the `.github/workflows` directory is at the root of your new repository, containing your CI/CD workflow files (`ci.yml` and `cd.yml`).

#### Step 2: Configure Docker Hub and GitHub Secrets

1. Create a Docker Hub account if you haven't already. See [Account Setup](#account-setup)
2. Create a new repository on Docker Hub for your project (e.g., `my-frontend-project`)
3. Go to your GitHub repository settings > Secrets and variables > Actions.
4. Add the following secrets:
   - `DOCKERHUB_USERNAME`: Your Docker Hub username.
   - `DOCKERHUB_TOKEN`: Your Docker Hub access token.
   - `DOCKERHUB_REPO`: Your Docker Hub repository name.

### Step 3: Triggering CI Workflow

The Continuous Integration (CI) workflow is triggered automatically on:
- Every push to the `main` branch
- Every pull request targeting the `main` branch

It performs the following tasks:
1. Sets up the Node.js environment
2. Installs project dependencies
3. Runs tests
4. Builds the project

To view CI results:
1. Go to your GitHub repository
2. Click on the "Actions" tab
3. You'll see a list of workflow runs. Click on a specific run to view details.

### Step 4: Triggering CD Workflow

To trigger the Continuous Deployment (CD) workflow:

1. Create a new release:
   ```
   git tag v1.0.0
   git push origin v1.0.0
   ```
   Or use the GitHub website:
   - Go to your repository > "Releases" > "Draft a new release"
   - Create a new tag (e.g., `v1.0.0`)

2. This action will trigger the CD pipeline, building and pushing the Docker image to Docker Hub.

### Step 5: Manual Docker Image Creation (Optional)

To manually create and push a Docker image:

1. Log in to Docker Hub:
   ```
   docker login -u $DOCKERHUB_USERNAME -p $DOCKERHUB_TOKEN
   ```
   Replace `$DOCKERHUB_USERNAME` and `$DOCKERHUB_TOKEN` with your actual Docker Hub credentials.

2. Build the Docker image:
   ```
   docker build -t $DOCKERHUB_USERNAME/$DOCKERHUB_REPO:latest .
   ```
   Replace `$DOCKERHUB_REPO` with your actual Docker Hub repository name.

3. Push the image to Docker Hub:
   ```
   docker push $DOCKERHUB_USERNAME/$DOCKERHUB_REPO:latest
   ```

Note: This manual process mimics the automated CD workflow but requires you to handle secrets manually.

### Step 6: Using the Docker Image

1. After the Docker image is pushed to Docker Hub (either via CD or manually), you can deploy it:

   ```
   docker pull $DOCKERHUB_USERNAME/$DOCKERHUB_REPO:latest
   docker run -p 3000:3000 $DOCKERHUB_USERNAME/$DOCKERHUB_REPO:latest
   ```

   Replace `$DOCKERHUB_USERNAME` and `$DOCKERHUB_REPO` with your actual Docker Hub username and repository name.

2. Follow [Website Usage](#website-usage)

## Troubleshooting

- Ensure your Blockfrost API key is correctly set in the `.env.local` file.
- Verify that your chosen wallet extension is properly installed and set up.
- Check that your wallet is connected to the correct testnet network.
- Confirm you have sufficient testnet ADA in your wallet for transactions.
- Check the browser console for any error messages.
- For CI/CD issues (in standalone setup), check the GitHub Actions logs for detailed error messages.

## Additional Resources

- [Cardano Developer Portal](https://developers.cardano.org/)
- [Blockfrost Documentation](https://docs.blockfrost.io/)
- [Lucid Library Documentation](https://lucid.spacebudz.io/)
- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [Docker Documentation](https://docs.docker.com/)
- [Docker Hub](https://hub.docker.com/)
- [Nami Wallet GitHub](https://github.com/berry-pool/nami)
- [Nami Wallet Extension](https://chromewebstore.google.com/detail/nami/lpfcbjknijpeeillifnkikgncikgfhdo)
- [Eternl Wallet Extension](https://chromewebstore.google.com/detail/eternl/kmhcihpebfmpgmihbkipmjlmmioameka)
- [Cardano Testnet Faucet](https://docs.cardano.org/cardano-testnets/tools/faucet/)

## Contributing

Contributions to improve this example are welcome! Please feel free to submit issues, fork the repository, and send pull requests.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

For any questions or support, please open an issue in the GitHub repository.





