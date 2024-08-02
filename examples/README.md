# Plutus Contract Examples

This directory contains a collection of examples for Plutus validator contracts and minting policies, along with their corresponding tests. Each contract example is organized into its own directory with the following structure:

- **`src/`**: Contains the Haskell source code for the validator contract or minting policy.
- **`test/`**: Contains the Haskell source code for the tests related to the contract or policy.
- **`.cabal`**: The Cabal configuration file for building and testing the project.

## Getting Started

To use each contract example, follow these steps:

1. **Build the project**:
   ```bash
   cabal build
   ```

2. **Run the tests**:
   ```bash
   cabal test
   ```

If you want to build and test all the examples in the `examples` directory, you can use the following commands:

- **Build all examples**:
  ```bash
  cabal build all
  ```

- **Run all tests**:
  ```bash
  cabal test all
  ```

## Directory Structure

Each example is organized into its own directory within the `examples` directory. The structure of each example directory is:

- `src/` - Contains the main Haskell source files for the example.
- `test/` - Contains the Haskell source files for the tests.
- `.cabal` - The Cabal configuration file.

## Examples

For detailed information about each example, navigate to the specific example directory and review the `README.md` files and source code there.

## Requirements

Make sure you have the following installed to build and test the projects:

- [GHC 8.10.7](https://www.haskell.org/ghc/)
- [Cabal 3.10.3](https://www.haskell.org/cabal/)

If you encounter any issues or need further assistance, please refer to the Plutus documentation or seek help from the Plutus community.
