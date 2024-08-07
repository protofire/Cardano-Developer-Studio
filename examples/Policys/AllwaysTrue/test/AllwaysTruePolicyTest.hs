{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

--------------------------------------------------------------------------------
{- HLINT ignore "Use camelCase"               -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------
module Main where

import Control.Monad (replicateM, unless)
import PlutusTx.Prelude (($), (==))
import Prelude (IO, mapM, mconcat)

import qualified Plutus.Model as Model
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Test.Tasty as Tasty

import AllwaysTruePolicy (allwaysTruePolicy)

---------------------------------------------------------------------------------------------------
------------------------------------------ TESTING ------------------------------------------------
-- | 'main' sets up and runs the tests for the 'AllwaysTruePolicy' policy.
-- The tests are organized into groups and are executed using Test.Tasty.
-- The `allwaysTrueTest` function is expected to succeed as the 'AllwaysTruePolicy' allows 
-- unrestricted minting and burning of tokens.
main :: IO ()
main = Tasty.defaultMain $ do
    Tasty.testGroup
        "Testing allwaysTrue policy"
        [ Tasty.testGroup
            "Must success"
            [ 
              good "Generic contract test" allwaysTrueTest
            ]
        ]
  where
    -- | 'good' is a helper function to denote a test that is expected to succeed.
    -- It uses 'Model.testNoErrors' to ensure the test passes without errors.
    good = Model.testNoErrors (Model.adaValue 10_000_000_000) Model.defaultBabbage

-- | 'AllwaysTruePolicyType' is a type alias for 'Model.TypedPolicy' specialized with unit type.
type AllwaysTruePolicyType = Model.TypedPolicy ()

-- | 'allwaysTruePolicyScript' converts the 'allwaysTruePolicy' into a typed policy script suitable for testing.
allwaysTruePolicyScript :: AllwaysTruePolicyType
allwaysTruePolicyScript = Model.TypedPolicy $ Model.toV2 allwaysTruePolicy

-- | 'setupUsers' initializes a list of users with a starting balance of 1000 Lovelace.
-- It uses 'replicateM' to create multiple users.
setupUsers :: Model.Run [LedgerApiV2.PubKeyHash]
setupUsers = replicateM 2 $ Model.newUser $ Model.ada (Model.Lovelace 1000)

-- | 'allwaysTrueTest' is a test function that checks the behavior of the 'AllwaysTruePolicy'.
-- It performs the following actions:
-- 1. Sets up users and retrieves their UTXO references.
-- 2. Mints and burns tokens using the 'AllwaysTruePolicy'.
-- 3. Verifies that the final balance is correct after minting and burning operations.
allwaysTrueTest :: Model.Run ()
allwaysTrueTest = do
  users <- setupUsers
  let [u1, _] = users
      cSymbol = Model.scriptCurrencySymbol allwaysTruePolicyScript
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
-- It includes the spending of tokens and minting with the 'AllwaysTruePolicy'.
burnTx :: Model.UserSpend -> LedgerApiV2.Value -> Model.Tx
burnTx usp val = 
  mconcat
    [ Model.userSpend usp
    , Model.mintValue allwaysTruePolicyScript () val
    ]

-- | 'mintTx' creates a transaction to mint tokens.
-- It includes the minting of tokens and payment to the specified public key hash.
mintTx :: LedgerApiV2.PubKeyHash -> LedgerApiV2.Value -> Model.Tx
mintTx pkh val =
  mconcat
    [ 
      Model.mintValue allwaysTruePolicyScript () val
    , Model.payToKey pkh val
    ]
