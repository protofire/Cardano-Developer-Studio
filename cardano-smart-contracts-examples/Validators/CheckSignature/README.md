# Check signature validator

This project contains two Plutus smart contracts: `DatumCheckSignatureValidator` and `ParamCheckSignatureValidator`. Both contracts are designed to check if a transaction is signed by a specific beneficiary, but they differ in how they receive the public key hash (PKH) of the beneficiary.

## Overview

- **DatumCheckSignatureValidator**: This contract checks if a transaction is signed by a beneficiary whose PKH is provided in the datum.
- **ParamCheckSignatureValidator**: This contract checks if a transaction is signed by a beneficiary whose PKH is provided as a parameter.

### Key Components

#### Common functions
- **(Contract name)Validator**: Compiled Plutus V2 validator script. Optimized using `Plutonomy.optimizeUPLC` and converted into a Plutus V2 validator.
- **plutonomyValidator**: An intermediary representation of the validator script created using `Plutonomy.mkValidatorScript`.

#### DatumCheckSignatureValidator

- **mkDatumCheckSignatureValidator**: The core validator function that checks if the transaction is signed by the PKH specified in the datum. It takes three parameters of type `PlutusTx.BuiltinData`: datum, redeemer (unused), and context.

#### ParamCheckSignatureValidator

- **mkParamCheckSignatureValidator**: The core validator function that checks if the transaction is signed by the PKH specified as a parameter. It takes three parameters of type `PlutusTx.BuiltinData`: parameter (PKH), redeemer (unused), and context.

## Project Structure

The project is organized into the following directories:

- `src`: Contains the source code for both `DatumCheckSignatureValidator` and `ParamCheckSignatureValidator` contracts.
- `test`: Includes tests for both contracts to ensure they behave as expected.
- `<project>.cabal`: The Cabal project file for building and testing the contracts.

## Building and Testing

To build and test the contracts, follow these steps:
1. **Update cabal references**:
   ```bash
   cabal update
   ```

2. **Build the contracts**:
   ```bash
   cabal build
   ```

3. **Run the tests**:
   ```bash
   cabal test
   ```

This README provides a brief overview of both contracts, explains their components, and offers instructions on how to build and test them. For more information, you can check the code implementation in the respective source files:

- [DatumCheckSignatureValidator](./src/DatumCheckSignatureValidator.hs)
- [ParamCheckSignatureValidator](./src/ParamCheckSignatureValidator.hs)
