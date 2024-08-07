{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module Main where

import Control.Monad (replicateM, unless)
import PlutusTx.Prelude ((.), ($), (==), (<>))
import Prelude (IO, mapM, mconcat)

import qualified Plutus.Model as Model
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Test.Tasty as Tasty

import RedeemerFtPolicy (redeemerFtPolicy, RedeemerFT(..))

---------------------------------------------------------------------------------------------------
------------------------------------------ TESTING ------------------------------------------------

-- | Main entry point for running tests using the Tasty testing framework.
-- This will execute all the test groups and report the results.
main :: IO ()
main = Tasty.defaultMain $ do
    Tasty.testGroup
        "Testing redeemerFt policy"
        [ Tasty.testGroup
            "Test the Policy of a FT"
            [ 
              -- Test minting and burning with correct redeemer
              good "Minting and burning with respectly redeemer" redeemerFtTest,
              
              -- Test minting with an invalid redeemer, should fail
              bad "Try minting with wrong redeemer" redeemerFtFailTest
            ]
        ]
  where
    -- Helper function to mark test cases that should fail.
    bad msg = good msg . Model.mustFail
    -- Helper function to mark test cases that should succeed.
    good = Model.testNoErrors (Model.adaValue 10_000_000_000) Model.defaultBabbage

type RedeemerFtPolicyType = Model.TypedPolicy RedeemerFT

-- | Create the TypedPolicy for the redeemer fungible token policy.
redeemerFtPolicyScript :: RedeemerFtPolicyType
redeemerFtPolicyScript = Model.TypedPolicy $ Model.toV2 redeemerFtPolicy

-- | Set up users with an initial balance.
setupUsers :: Model.Run [LedgerApiV2.PubKeyHash]
setupUsers = replicateM 2 $ Model.newUser $ Model.ada (Model.Lovelace 1000)

-- | Test minting and burning tokens with valid redeemers.
-- This test will:
--   1. Mint a token.
--   2. Spend the minted token.
--   3. Burn the token.
--   4. Verify that the final balance is as expected.
redeemerFtTest :: Model.Run ()
redeemerFtTest = do
  users <- setupUsers
  let [u1, _] = users
      cSymbol = Model.scriptCurrencySymbol redeemerFtPolicyScript
      mintValue = LedgerApiV2.singleton cSymbol "Token" 1
      burnValue = LedgerApiV2.singleton cSymbol "Token" (-1)
  
  -- Submit a transaction to mint a token.
  Model.submitTx u1 $ mintTx u1 mintValue
  
  -- Spend the minted token.
  sp <- Model.spend u1 mintValue
  
  -- Submit a transaction to burn the token.
  Model.submitTx u1 $ burnTx sp burnValue
  
  -- Check the final balance and ensure it matches the expected value.
  vals <- mapM Model.valueAt users
  let [v1, _] = vals
  unless (v1 == Model.adaValue 1000) $
    Model.logError "Final balances are incorrect"

-- | Create a transaction to burn tokens.
-- This transaction specifies the redeemer as Burn and includes the token to burn.
burnTx :: Model.UserSpend -> LedgerApiV2.Value -> Model.Tx
burnTx usp val = 
  mconcat
    [ Model.userSpend usp
    , Model.mintValue redeemerFtPolicyScript Burn val
    ]

-- | Create a transaction to mint tokens.
-- This transaction specifies the redeemer as Mint, mints the specified token,
-- and pays it to the given public key.
mintTx :: LedgerApiV2.PubKeyHash -> LedgerApiV2.Value -> Model.Tx
mintTx pkh val =
  mconcat
    [ 
      Model.mintValue redeemerFtPolicyScript Mint val
    , Model.payToKey pkh val
    ]

-- | Test minting with an invalid redeemer, which should fail.
-- This test will:
--   1. Attempt to mint a token with an invalid redeemer.
--   2. Verify that the final balance reflects the invalid operation.
redeemerFtFailTest :: Model.Run ()
redeemerFtFailTest = do
  users <- setupUsers
  let [u1, _] = users
      cSymbol = Model.scriptCurrencySymbol redeemerFtPolicyScript
      mintValue = LedgerApiV2.singleton cSymbol "Token" 1
  
  -- Submit a transaction to mint a token with an invalid redeemer.
  Model.submitTx u1 $ mintTxFail u1 mintValue

  -- Check the final balance and ensure it matches the expected value.
  vals <- mapM Model.valueAt users
  let [v1, _] = vals

  unless (v1 == (Model.adaValue 1000 <> mintValue)) $
    Model.logError "Final balances are incorrect"

-- | Create a transaction to mint tokens with an invalid redeemer.
-- This transaction specifies the redeemer as BadRedeemer, which should cause the policy to fail.
mintTxFail :: LedgerApiV2.PubKeyHash -> LedgerApiV2.Value -> Model.Tx
mintTxFail pkh val =
  mconcat
    [ 
      Model.mintValue redeemerFtPolicyScript BadRedeemer val
    , Model.payToKey pkh val
    ]
