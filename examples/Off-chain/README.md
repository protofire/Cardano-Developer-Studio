# Example Web Application Using Blockfrost

This project is an example web application that connects to the Cardano
blockchain using the Blockfrost API. It is designed to help developers
understand how to integrate Blockfrost into their own applications.

## Prerequisites

Before setting up the frontend, ensure that you have the following installed:

- Node.js (version 14 or higher)
- Yarn package manager
- A Blockfrost API key
- Nami wallet extension

## What is Blockfrost?

Blockfrost is a robust API that allows developers to interact with the Cardano
blockchain. It provides access to various blockchain data, enabling you to query
information about blocks, transactions, addresses, and more.

To use Blockfrost, you'll need to generate an API key, which will authenticate
your requests to the Blockfrost service.

## Generating a Blockfrost API Key

To use this example application, you'll need to generate a Blockfrost API key
for the `preview` environment:

1. Visit the [Blockfrost Dashboard](https://blockfrost.io/dashboard).
2. Create an account if you haven't already.
3. Navigate to the "Projects" section.
4. Create a new project and select the `preview` network.
5. Copy the API key provided by Blockfrost.

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

3. **Configure the Blockfrost API key**:

   - Open the `.env` file located in the `Examples/Off-chain/`
     directory.
   - Find the line that starts with `BLOCKFROST_PREVIEW=`.
   - Paste your Blockfrost API key after the equals sign, like so:
     ```env
     BLOCKFROST_PREVIEW=your_blockfrost_api_key_here
     ```

4. **Run the development server**:

   ```bash
   npm run dev
   ```

5. **Access the web application**:
   - Open your web browser and go to
     [http://localhost:3000](http://localhost:3000).

Now you can interact with the Cardano blockchain through the example
application.
