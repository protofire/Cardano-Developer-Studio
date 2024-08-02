{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Monad (replicateM, unless)
import PlutusTx.Prelude (($), (==))
import Prelude (IO, mapM, mconcat)

import qualified Plutus.Model as Model
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Test.Tasty as Tasty

import FreePolicy (freePolicy)

---------------------------------------------------------------------------------------------------
------------------------------------------ TESTING ------------------------------------------------
-- | 'main' sets up and runs the tests for the 'FreePolicy' policy.
-- The tests are organized into groups and are executed using Test.Tasty.
-- The `freeTest` function is expected to succeed as the 'FreePolicy' allows 
-- unrestricted minting and burning of tokens.
main :: IO ()
main = Tasty.defaultMain $ do
    Tasty.testGroup
        "Testing free policy"
        [ Tasty.testGroup
            "Must success"
            [ 
              good "Generic contract test" freeTest
            ]
        ]
  where
    -- | 'good' is a helper function to denote a test that is expected to succeed.
    -- It uses 'Model.testNoErrors' to ensure the test passes without errors.
    good = Model.testNoErrors (Model.adaValue 10_000_000_000) Model.defaultBabbage

-- | 'FreePolicyType' is a type alias for 'Model.TypedPolicy' specialized with unit type.
type FreePolicyType = Model.TypedPolicy ()

-- | 'freePolicyScript' converts the 'freePolicy' into a typed policy script suitable for testing.
freePolicyScript :: FreePolicyType
freePolicyScript = Model.TypedPolicy $ Model.toV2 freePolicy

-- | 'setupUsers' initializes a list of users with a starting balance of 1000 Lovelace.
-- It uses 'replicateM' to create multiple users.
setupUsers :: Model.Run [LedgerApiV2.PubKeyHash]
setupUsers = replicateM 2 $ Model.newUser $ Model.ada (Model.Lovelace 1000)

-- | 'freeTest' is a test function that checks the behavior of the 'FreePolicy'.
-- It performs the following actions:
-- 1. Sets up users and retrieves their UTXO references.
-- 2. Mints and burns tokens using the 'FreePolicy'.
-- 3. Verifies that the final balance is correct after minting and burning operations.
freeTest :: Model.Run ()
freeTest = do
  users <- setupUsers
  let [u1, _] = users
      cSymbol = Model.scriptCurrencySymbol freePolicyScript
      mintValue = LedgerApiV2.singleton cSymbol "Token" 1
      burnValue = LedgerApiV2.singleton cSymbol "Token" (-1)
  
  -- Submit a transaction to mint tokens
  Model.submitTx u1 $ mintTx u1 mintValue

  -- Spend the minted tokens
  sp <- Model.spend u1 mintValue

  -- Submit a transaction to burn tokens
  Model.submitTx u1 $ burnTx sp burnValue
  
  -- Check the final balances
  vals <- mapM Model.valueAt users
  let [v1, _] = vals
  unless (v1 == Model.adaValue 1000) $
    Model.logError "Final balances are incorrect"

-- | 'burnTx' creates a transaction to burn tokens.
-- It includes the spending of tokens and minting with the 'FreePolicy'.
burnTx :: Model.UserSpend -> LedgerApiV2.Value -> Model.Tx
burnTx usp val = 
  mconcat
    [ Model.userSpend usp
    , Model.mintValue freePolicyScript () val
    ]

-- | 'mintTx' creates a transaction to mint tokens.
-- It includes the minting of tokens and payment to the specified public key hash.
mintTx :: LedgerApiV2.PubKeyHash -> LedgerApiV2.Value -> Model.Tx
mintTx pkh val =
  mconcat
    [ 
      Model.mintValue freePolicyScript () val
    , Model.payToKey pkh val
    ]
