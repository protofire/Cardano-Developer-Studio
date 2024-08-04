# Check date validator

This project contains four Plutus smart contracts, each designed to check if a transaction is valid based on a specified deadline. The contracts differ in how they receive and process the deadline information.

## Overview

- **DatumCheckAfterDeadlineValidator**: Validates that the current blockchain time is after a deadline specified in the datum.
- **DatumCheckBeforeDeadlineValidator**: Validates that the current blockchain time is before a deadline specified in the datum.
- **ParamCheckAfterDeadlineValidator**: Validates that the current blockchain time is after a deadline specified as a parameter.
- **ParamCheckBeforeDeadlineValidator**: Validates that the current blockchain time is before a deadline specified as a parameter.

### Key Components

#### Common functions
- **(Contract name)Validator**: Compiled Plutus V2 validator script. Optimized using `Plutonomy.optimizeUPLC` and converted into a Plutus V2 validator.
- **plutonomyValidator**: An intermediary representation of the validator script created using `Plutonomy.mkValidatorScript`.

#### DatumCheckAfterDeadlineValidator

- **mkDatumCheckAfterDeadlineValidator**: Core validator function that checks if the current time is after a specified deadline. Takes three `PlutusTx.BuiltinData` parameters: datum (deadline), redeemer (unused), and context.

#### DatumCheckBeforeDeadlineValidator

- **mkDatumCheckBeforeDeadlineValidator**: Core validator function that checks if the current time is before a specified deadline. Takes three `PlutusTx.BuiltinData` parameters: datum (deadline), redeemer (unused), and context.

#### ParamCheckAfterDeadlineValidator

- **mkParamCheckAfterDeadlineValidator**: Core validator function that checks if the current time is after a specified deadline provided as a parameter. Takes four parameters: deadline, redeemer (unused), another unused parameter, and context.

#### ParamCheckBeforeDeadlineValidator

- **mkParamCheckBeforeDeadlineValidator**: Core validator function that checks if the current time is before a specified deadline provided as a parameter. Takes four parameters: deadline, redeemer (unused), another unused parameter, and context.

## Project Structure

The project is organized into the following directories:

- `src`: Contains the source code for all four validators.
- `test`: Includes tests for each validator to ensure they behave as expected.
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

- [DatumCheckAfterDeadlineValidator](./src/DatumCheckAfterDeadlineValidator.hs)
- [DatumCheckBeforeDeadlineValidator](./src/DatumCheckBeforeDeadlineValidator.hs)
- [ParamCheckAfterDeadlineValidator](./src/ParamCheckAfterDeadlineValidator.hs)
- [ParamCheckBeforeDeadlineValidator](./src/ParamCheckBeforeDeadlineValidator.hs)
