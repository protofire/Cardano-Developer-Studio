# AlwaysTrue validator

This project contains a simple Plutus smart contract called `AlwaysTrueValidator`. The contract is designed to always succeed validation. It serves as an example of a basic Plutus contract structure and compilation process.

## Overview

The `AlwaysTrueValidator` module defines a single validator script that always succeeds. This is achieved by returning unit `()` in the core validator function, making the script pass whenever it is evaluated.

### Key Components

- **mkAlwaysTrueValidator**: The core validator function that always succeeds. It takes three parameters of type `PlutusTx.BuiltinData` but does not use them.
- **alwaysTrueValidator**: The compiled Plutus V2 validator. It is optimized using `Plutonomy.optimizeUPLC` and converted into a Plutus V2 validator using `Plutonomy.validatorToPlutus`.
- **plutonomyValidator**: An intermediary representation of the validator script created using `Plutonomy.mkValidatorScript`.

## Project Structure

The project is organized into the following directories:

- `src`: Contains the source code for the `AlwaysTrueValidator` contract.
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

This README provides a brief overview of the contract, explains its components, and offers instructions on how to build and test it. For more information you can check [the code implementation](./src/AlwaysTrueValidator.hs).
