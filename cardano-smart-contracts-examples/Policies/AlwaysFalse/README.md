# AlwaysFalse policy

This project contains a Plutus smart contract called `AlwaysFalsePolicy`. The contract defines a minting policy that, for the purpose of this example, always fails. It serves as a basic example of a minting policy structure in Plutus.

## Overview

The `AlwaysFalsePolicy` module provides a minting policy script that is designed to always fail. This is achieved by the `mkAlwaysFalse` function, which raises an error regardless of the input parameters. 

### Key Components

- **mkAlwaysFalse**: The core minting policy function. This function currently raises an error regardless of its input, making the policy always fail. In a real implementation, this function would contain logic to validate conditions for minting tokens.
- **lockPolicy**: The compiled Plutus V2 minting policy. It is optimized using `Plutonomy.optimizeUPLC` and converted into a Plutus V2 minting policy using `Plutonomy.mintingPolicyToPlutus`.
- **plutonomyPolicy**: An intermediary representation of the minting policy script created using `Plutonomy.mkMintingPolicyScript`.

## Project Structure

The project is organized into the following directories:

- `src`: Contains the source code for the `AlwaysFalsePolicy` contract.
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

This README provides a brief overview of the contract, explains its components, and offers instructions on how to build and test it. For more information you can check [the code implementation](./src/AlwaysFalsePolicy.hs).
