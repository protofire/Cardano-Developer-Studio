# Check date policies

This project contains two Plutus smart contracts: `ParamCheckAfterDeadlinePolicy` and `ParamCheckBeforeDeadlinePolicy`.

These contracts define minting policies based on time constraints. `ParamCheckAfterDeadlinePolicy` ensures tokens can only be minted after a specified deadline, while `ParamCheckBeforeDeadlinePolicy` ensures tokens can only be minted before a specified deadline.

## Overview

The project includes two minting policy modules:

- **ParamCheckAfterDeadlinePolicy**: Enforces that tokens can only be minted after a specific deadline.
- **ParamCheckBeforeDeadlinePolicy**: Enforces that tokens can only be minted before a specific deadline.

### Key Components

#### Common functions

- **paramCheckAfterDeadlinePolicy**: The compiled Plutus V2 minting policy. It is optimized using `Plutonomy.optimizeUPLC` and converted into a Plutus V2 minting policy using `Plutonomy.mintingPolicyToPlutus`.
- **plutonomyPolicy**: An intermediary representation of the minting policy script created using `Plutonomy.mkMintingPolicyScript`.


#### ParamCheckAfterDeadlinePolicy

- **mkParamCheckAfterDeadlinePolicy**: The core policy function that checks if the current time is after the specified deadline. If the deadline is not reached, it raises an error.

#### ParamCheckBeforeDeadlinePolicy

- **mkParamCheckBeforeDeadlinePolicy**: The core policy function that checks if the current time is before the specified deadline. If the deadline has passed, it raises an error.

## Project Structure

The project is organized into the following directories:

- `src`: Contains the source code for the `ParamCheckAfterDeadlinePolicy` and `ParamCheckBeforeDeadlinePolicy` contracts.
- `test`: Includes tests for the contracts to ensure they behave as expected.
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

This README provides a brief overview of all four contracts, explains their components, and offers instructions on how to build and test them. For more information, you can check the code implementations in the respective source files:

 - [ParamCheckAfterDeadlinePolicy](./src/ParamCheckAfterDeadlinePolicy.hs)
 - [ParamCheckBeforeDeadlinePolicy](./src/ParamCheckBeforeDeadlinePolicy.hs).
