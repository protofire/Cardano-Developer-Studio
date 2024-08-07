# AllwaysFalse validator

This project contains a simple Plutus smart contract called `AllwaysFalseValidator`. The contract is designed to always fail validation. It serves as an example of a basic Plutus contract structure and compilation process.

## Overview

The `AllwaysFalseValidator` module defines a single validator script that always fails. This is achieved using the `error` function in Haskell, which causes the script to fail whenever it is evaluated.

### Key Components

- **mkAllwaysFalseValidator**: The core validator function that always fails. It takes three parameters of type `PlutusTx.BuiltinData` but does not use them.
- **allwaysFalseValidator**: The compiled Plutus V2 validator. It is optimized using `Plutonomy.optimizeUPLC` and converted into a Plutus V2 validator using `Plutonomy.validatorToPlutus`.
- **plutonomyValidator**: An intermediary representation of the validator script created using `Plutonomy.mkValidatorScript`.

## Project Structure

The project is organized into the following directories:

- `src`: Contains the source code for the `AllwaysFalseValidator` contract.
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

This README provides a brief overview of the contract, explains its components, and offers instructions on how to build and test it. For more information you can check [the code implementation](./src/AllwaysFalseValidator.hs).

