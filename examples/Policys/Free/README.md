# Free policy

This project contains a Plutus smart contract called `FreePolicy`. The contract defines a minting policy that allows unrestricted minting and burning of tokens. It serves as a basic example of a permissive minting policy in Plutus.

## Overview

The `FreePolicy` module provides a minting policy script that does not enforce any conditions for minting or burning tokens. This policy is always successful, regardless of the inputs.

### Key Components

- **mkFreePolicy**: The core minting policy function. This function always succeeds as it does not enforce any conditions on the inputs. The arguments (redeemer and context data) are unused in this policy.
- **freePolicy**: The compiled Plutus V2 minting policy. It is optimized using `Plutonomy.optimizeUPLC` and converted into a Plutus V2 minting policy using `Plutonomy.mintingPolicyToPlutus`.
- **plutonomyPolicy**: An intermediary representation of the minting policy script created using `Plutonomy.mkMintingPolicyScript`.

## Project Structure

The project is organized into the following directories:

- `src`: Contains the source code for the `FreePolicy` contract.
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

This README provides a brief overview of the contract, explains its components, and offers instructions on how to build and test it. For more information you can check [the code implementation](./src/FreePolicy.hs).
