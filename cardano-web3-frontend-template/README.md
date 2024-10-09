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
  - [CI and CD Pipeline](#ci-and-cd-pipeline)
    - [Continuous Integration (CI)](#continuous-integration-ci)
    - [Continuous Deployment (CD)](#continuous-deployment-cd)
    - [Important Notes on Environment Variables](#important-notes-on-environment-variables)
  - [Detailed Usage Guide](#detailed-usage-guide)
    - [Using the Template within Developer Studio](#using-the-template-within-developer-studio)
    - [Using the Template as a Standalone Project](#using-the-template-as-a-standalone-project)
      - [Step 1: Clone the Cardano Developer Studio Repository](#step-1-clone-the-cardano-developer-studio-repository)
      - [Step 2: Copy the Template to a New Directory](#step-2-copy-the-template-to-a-new-directory)
      - [Step 3: Initialize a New Git Repository](#step-3-initialize-a-new-git-repository)
      - [Step 4: Set Up Project](#step-4-set-up-project)
  - [CI and CD Setup](#ci-and-cd-setup)
    - [Step 1: Verify Workflow Files](#step-1-verify-workflow-files)
    - [Step 2: Configure Docker Hub and GitHub Secrets](#step-2-configure-docker-hub-and-github-secrets)
    - [Step 3: Understanding CI Workflow](#step-3-understanding-ci-workflow)
    - [Step 4: Understanding CD Workflow](#step-4-understanding-cd-workflow)
  - [Building and Pushing Docker Images](#building-and-pushing-docker-images)
    - [Automated Building via CD](#automated-building-via-cd)
    - [Manual Docker Image Creation](#manual-docker-image-creation)
  - [Deploying Your Application](#deploying-your-application)
    - [Local Deployment](#local-deployment)
    - [Cloud Deployment](#cloud-deployment)
      - [AWS Elastic Container Service (ECS)](#aws-elastic-container-service-ecs)
      - [Google Cloud Run](#google-cloud-run)
      - [Azure Container Instances](#azure-container-instances)
    - [Demeter.run Deployment](#demeterrun-deployment)
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
2. **Docker Hub Account**: Required for storing and managing Docker images. If you don't have one, create it [here](https://hub.docker.com/). You will need to create a [personal access token](https://app.docker.com/settings/personal-access-tokens) and ensure it has both read and write permissions.

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

1. Follow [Quick Start](#quick-start) to set up and run the website.
2. Ensure your chosen wallet extension is installed and set to the testnet network. Follow [Installing a Compatible Wallet Extension](#installing-a-compatible-wallet-extension).
3. Open the web application at [http://localhost:3000](http://localhost:3000).
4. Use the wallet connector to connect your preferred wallet. If you don't have a compatible wallet installed, use the provided "Install" buttons to set one up.
5. Once connected, you can view your ADA balance and other blockchain data.
6. Interact with deployed Plutus smart contracts through the user interface.
7. Create, sign, and submit transactions using your testnet ADA.

## Development

- Customize the UI by modifying React components in `src/components/`.
- Add new pages in `src/pages/`.
- Use `src/utils/` for helper functions and API calls.
- When testing transactions, always use small amounts of testnet ADA.

## CI and CD Pipeline

Note: The CI/CD pipeline is only functional when this template is used as a standalone project, not within the Cardano Developer Studio repository.

This project uses GitHub Actions for Continuous Integration and Continuous Deployment (CI/CD) when set up as a standalone repository.

Follow [Using the Template as a Standalone Project](#using-the-template-as-a-standalone-project) and then [CI/CD Setup](#ci-and-cd-setup).

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

- Triggered when a new release is created on GitHub. This can be done either by creating a release using the GitHub website or through the GitHub CLI tool, which involves creating a release with an associated tag.
- Builds a Docker image of your application using the specified Docker configuration.
- Pushes the Docker image to Docker Hub, making it available for deployment.

Benefits of CD:
- Automates the deployment process, reducing manual errors and ensuring reliability.
- Ensures consistent deployment across different environments by using containerization.
- Enables rapid and frequent releases, allowing new features and fixes to be delivered to users more quickly and efficiently.

### Important Notes on Environment Variables

- **Environment Variables Not Included in Docker Image**: The Docker image built during the CD process does not include environment variables, such as the Blockfrost API key stored in `.env.local`, since this file is listed in `.gitignore` and is not included in the repository or the Docker image.
  
- **Adding Environment Variables at Build Time and Runtime**: To ensure the application functions correctly, you must provide the necessary environment variables both when building and running the Docker container.
   
   **For Building the Docker Image**: 
   The Continuous Deployment (CD) workflow uses GitHub Secrets to securely pass environment variables needed for the build. 

   **For Building the Docker Image (Manual Build)**: 
   Use the `--build-arg` flag to pass the environment variable during the Docker image build process: 
   ```
   docker build -t $DOCKERHUB_USERNAME/$DOCKERHUB_REPO --build-arg BLOCKFROST_PREVIEW=your_blockfrost_key .
   ``` 
   This ensures the `BLOCKFROST_PREVIEW` environment variable is set during the image build process.

   **For Running the Docker Container**:

   Use the `-e` flag to pass environment variables when starting the container:
   ```
   docker run -p 3000:3000 -e BLOCKFROST_PREVIEW=your_blockfrost_key $DOCKERHUB_USERNAME/$DOCKERHUB_REPO:latest
   ```
   This sets the `BLOCKFROST_PREVIEW` environment variable at runtime, ensuring the application has the necessary configuration.

## Detailed Usage Guide

### Using the Template within Developer Studio

1. Follow the [Quick Start](#quick-start) guide.
2. The template is ready for development and testing within the Cardano Developer Studio environment.

Note that the CI/CD workflows will not run automatically in this setup.

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
   cp -r . ../../my-frontend-project/
   cd ../../my-frontend-project
   ```

#### Step 3: Initialize a New Git Repository

1. Initialize a new Git repository:
   ```
   git init --initial-branch=main
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

## CI and CD Setup

### Step 1: Verify Workflow Files

Ensure that the `.github/workflows` directory is at the root of your new repository, containing your CI/CD workflow files (`ci.yml` and `cd.yml`).

### Step 2: Configure Docker Hub and GitHub Secrets

1. Create a Docker Hub account if you haven't already. See [Account Setup](#account-setup)
2. Create a new repository on Docker Hub for your project (e.g., `my-frontend-project`)
3. Go to your GitHub repository Settings: 
   
   https://github.com/your-username/your-new-repo/settings
     
   And then > Secrets and variables > Actions
4. Add the following repository secrets:
   - `DOCKERHUB_USERNAME`: Your Docker Hub username.
   - `DOCKERHUB_TOKEN`: Your Docker Hub access token.
   - `DOCKERHUB_REPO`: Your Docker Hub repository name.
   - `BLOCKFROST_PREVIEW`: Your Blockfrost Preview Key. See: [Generating a Blockfrost API Key](#generating-a-blockfrost-api-key)

### Step 3: Understanding CI Workflow

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

### Step 4: Understanding CD Workflow

The Continuous Deployment (CD) workflow:

- Is triggered when a new release is created on GitHub. This can be done either by creating a release using the GitHub website or through the GitHub CLI tool, which involves creating a release with an associated tag.
- Builds a Docker image of your application using the specified Docker configuration.
- Pushes the Docker image to Docker Hub, making it available for deployment.

1. Create a new release using the GitHub website or GitHub CLI:

   - Using GitHub CLI:

   ```
   gh release create v1.0.0 --notes "Release notes here"
   ```

   - Using GitHub website:

   Go to your repository > "Releases" > "Draft a new release"
   Create a new tag (e.g., v1.0.0), fill in the release notes, and publish the release.

2. This action will trigger the CD pipeline, building and pushing the Docker image to Docker Hub.

## Building and Pushing Docker Images

### Automated Building via CD

The CD workflow automatically builds and pushes the Docker image when a new release is created. You don't need to take any additional steps beyond creating the release. 

The resulting Docker image will be available on Docker Hub with the following URL structure:

```
docker.io/$DOCKERHUB_USERNAME/$DOCKERHUB_REPO:$TAG
```

Where:
- `$DOCKERHUB_USERNAME` is your Docker Hub username
- `$DOCKERHUB_REPO` is the name of your Docker Hub repository
- `$TAG` is the tag of your release (e.g., "v1.0.0" or "latest")

For example, if your Docker Hub username is "cardanodev", your repository is "web3-frontend", and you've just released version 1.0.0, your image URL would be:

```
docker.io/cardanodev/web3-frontend:v1.0.0
```

### Manual Docker Image Creation

To manually create and push a Docker image:

1. Log in to Docker Hub:
   ```
   docker login -u $DOCKERHUB_USERNAME -p $DOCKERHUB_TOKEN
   ```
   Replace `$DOCKERHUB_USERNAME` and `$DOCKERHUB_TOKEN` with your actual Docker Hub credentials.

2. Build the Docker image:
   ```
   docker build -t $DOCKERHUB_USERNAME/$DOCKERHUB_REPO:latest --build-arg BLOCKFROST_PREVIEW=your_blockfrost_key .
   ```
   
   Replace `$DOCKERHUB_REPO` and `your_blockfrost_key` with your actual Docker Hub repository name and Blockfrost API key, respectively.

3. Push the image to Docker Hub:
   ```
   docker push $DOCKERHUB_USERNAME/$DOCKERHUB_REPO:latest
   ```

The resulting Docker image will have the same URL structure as described in the automated build section.

Note: This manual process mimics the automated CD workflow but requires you to handle secrets manually.

## Deploying Your Application

After building your Docker image (either through the CD pipeline or manually), you have several options for deploying and running your Cardano Web3 Frontend. This section covers various deployment methods, from local testing to cloud platforms and specialized Cardano infrastructure.

When deploying, you'll use the Docker image URL `docker.io/$DOCKERHUB_USERNAME/$DOCKERHUB_REPO:$TAG`.  
Make sure to replace `$DOCKERHUB_USERNAME`, `$DOCKERHUB_REPO`, and `$TAG` with your actual values in the following deployment instructions.

After deploying your application using any of these methods, you can access and interact with your Cardano Web3 Frontend as described in the [Website Usage](#website-usage) section.

### Local Deployment

For local testing or development purposes, you can run the Docker image on your machine. 
Make sure to include the necessary environment variables, such as the Blockfrost API key:

```
docker pull $DOCKERHUB_USERNAME/$DOCKERHUB_REPO:latest
docker run -p 3000:3000 -e BLOCKFROST_PREVIEW=your_blockfrost_key $DOCKERHUB_USERNAME/$DOCKERHUB_REPO:latest
```

Replace `$DOCKERHUB_USERNAME`, `$DOCKERHUB_REPO`, and `your_blockfrost_key` with your actual Docker Hub username, repository name, and Blockfrost API key, respectively.

### Cloud Deployment

For production or scalable deployments, consider using cloud platforms. In each case, you'll use your Docker image URL when configuring the deployment:

#### AWS Elastic Container Service (ECS)

1. Create an ECS cluster
2. Create a task definition using your Docker image
3. Run the task or create a service

For detailed steps, refer to the [AWS ECS documentation](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/Welcome.html).

#### Google Cloud Run

1. Enable the Cloud Run API in your Google Cloud project
2. Deploy your container using the following command:
   ```
   gcloud run deploy --image $DOCKERHUB_USERNAME/$DOCKERHUB_REPO:latest --platform managed
   ```

For more information, see the [Google Cloud Run documentation](https://cloud.google.com/run/docs).

#### Azure Container Instances

1. Create a container instance using Azure CLI or Azure Portal
2. Specify your Docker image and required environment variables

Refer to the [Azure Container Instances documentation](https://docs.microsoft.com/en-us/azure/container-instances/) for detailed instructions.

### Demeter.run Deployment

Demeter.run offers a specialized platform for Cardano projects with pre-configured tools and services. To deploy your Docker image on Demeter.run:

1. Go to [demeter.run](https://demeter.run) and create an account if you haven't already.
2. Create a new project in your Demeter dashboard.
3. Add a new product to your project.
4. Choose "Frontend By TxPipe" as your product type.
5. In the configuration settings, provide the URL of your Docker image from Docker Hub. This will be in the format:
   ```
   docker.io/$DOCKERHUB_USERNAME/$DOCKERHUB_REPO:$TAG
   ```
6. Set any required environment variables, including your Blockfrost API key.
7. Expose the port 3000.

Demeter.run will handle the deployment process and provide you with a URL where your frontend is accessible.

Remember to always follow best practices for managing sensitive information like API keys when deploying to any platform. Use environment variables or secure secret management services provided by your chosen platform.

## Troubleshooting

- Ensure your Blockfrost API key is correctly set in the `.env.local` file.
- Verify that your chosen wallet extension is properly installed and set up.
- Check that your wallet is connected to the correct testnet network.
- Confirm you have sufficient testnet ADA in your wallet for transactions.
- Check the browser console for any error messages.
- For CI/CD issues (in standalone setup), check the GitHub Actions logs for detailed error messages.
- If you encounter issues with Docker image deployment:
  - Verify that all required environment variables are correctly set.
  - Check the logs of your running container for any error messages.
  - Ensure that the port mappings are correct and the application is accessible from outside the container.
  - For cloud deployments, review the platform-specific logs and monitoring tools for any issues.

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
- [AWS ECS Documentation](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/Welcome.html)
- [Google Cloud Run Documentation](https://cloud.google.com/run/docs)
- [Azure Container Instances Documentation](https://docs.microsoft.com/en-us/azure/container-instances/)
- [Demeter.run Documentation](https://docs.demeter.run)

## Contributing

Contributions to improve this example are welcome! Please feel free to submit issues, fork the repository, and send pull requests.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

For any questions or support, please open an issue in the GitHub repository.





