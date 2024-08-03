# Redeemer NFT policy

This project contains a Plutus smart contract called `RedeemerNftPolicy`. The contract defines a minting policy that is used to validate transactions based on the redeemer value (`Mint` or `Burn`). It ensures that the correct amount of tokens is minted or burned and that the specified UTxO is consumed.

## Overview

The `RedeemerNftPolicy` module provides a minting policy script that validates transactions involving NFTs. The policy enforces rules for minting and burning tokens and ensures the correct UTxO reference is used.

### Key Components

- **mkRedeemerNftPolicy**: The core minting policy function. It checks if a transaction is valid based on the redeemer value and the specified UTxO reference. It validates whether the correct amount of tokens is minted or burned.
- **redeemerNftPolicy**: The compiled Plutus V2 minting policy. It is optimized using `Plutonomy.optimizeUPLC` and converted into a Plutus V2 minting policy using `Plutonomy.mintingPolicyToPlutus`.
- **plutonomyPolicy**: An intermediary representation of the minting policy script created using `Plutonomy.mkMintingPolicyScript`.

## Project Structure

The project is organized into the following directories:

- `src`: Contains the source code for the `RedeemerNftPolicy` contract.
- `test`: Includes tests for the contract to ensure it behaves as expected.
- `<project>.cabal`: The Cabal project file for building and testing the contract.

## Building and Testing

To build and test the contract, follow these steps:
1. **Update cabal references**:
   ```bash
   cabal update
   ```

2. **Build the contract**:
   ```bash
   cabal build
   ```

3. **Run the tests**:
   ```bash
   cabal test
   ```

This README provides a brief overview of the contract, explains its components, and offers instructions on how to build and test it. For more information you can check [the code implementation](./src/RedeemerNftPolicy.hs).
