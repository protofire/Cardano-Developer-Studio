# Check signature policy

This project contains a Plutus smart contract called `ParamCheckSignaturePolicy`. The contract defines a minting policy that requires a specific public key hash to sign the transaction. It serves as an example of a policy that enforces a signature requirement for minting.

## Overview

The `ParamCheckSignaturePolicy` module provides a minting policy script that ensures a transaction is signed by a specified public key hash. If the required signature is missing, the policy will fail.

### Key Components

- **mkParamCheckSignaturePolicy**: The core policy function. It checks if the transaction is signed by the specified public key hash. If the signature is missing, it raises an error. This function takes a `Parameter` representing the public key hash and two `PlutusTx.BuiltinData` arguments (which are unused).
- **paramCheckSignaturePolicy**: The compiled Plutus V2 minting policy. It is optimized using `Plutonomy.optimizeUPLC` and converted into a Plutus V2 minting policy using `Plutonomy.mintingPolicyToPlutus`.
- **plutonomyPolicy**: An intermediary representation of the minting policy script created using `Plutonomy.mkMintingPolicyScript`.

## Project Structure

The project is organized into the following directories:

- `src`: Contains the source code for the `ParamCheckSignaturePolicy` contract.
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

This README provides a brief overview of the contract, explains its components, and offers instructions on how to build and test it. For more information you can check [the code implementation](./src/ParamCheckSignaturePolicy.hs).
