# Redeemer FT policy

This project contains a Plutus smart contract called `RedeemerFtPolicy`. The contract defines a minting policy for fungible tokens that validates transactions based on the redeemer action. It ensures that only valid actions (Mint or Burn) are allowed, and any invalid redeemer will cause the policy to fail.

## Overview

The `RedeemerFtPolicy` module provides a minting policy script for fungible tokens. The policy enforces rules for valid redeemer actions and rejects any invalid actions.

### Key Components

- **mkRedeemerFtPolicy**: The core minting policy function. It checks if the redeemer action is valid. Valid actions are `Mint` and `Burn`. Any other redeemer is considered invalid and causes the policy to fail.
- **redeemerFtPolicy**: The compiled Plutus V2 minting policy. It is optimized using `Plutonomy.optimizeUPLC` and converted into a Plutus V2 minting policy using `Plutonomy.mintingPolicyToPlutus`.
- **plutonomyPolicy**: An intermediary representation of the minting policy script created using `Plutonomy.mkMintingPolicyScript`.

## Project Structure

The project is organized into the following directories:

- `src`: Contains the source code for the `RedeemerFtPolicy` contract.
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

This README provides a brief overview of the contract, explains its components, and offers instructions on how to build and test it. For more information you can check [the code implementation](./src/RedeemerFtPolicy.hs).
