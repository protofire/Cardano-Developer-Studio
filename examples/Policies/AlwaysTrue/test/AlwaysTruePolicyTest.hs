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
import PlutusTx.Prelude (($), (==), Maybe (Just, Nothing))
import Prelude (IO, mapM, mconcat)

import qualified Plutus.Model as Model
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Test.Tasty                        as Tasty
import qualified Test.Tasty.HUnit                  as Tasty
import qualified PlutusTx

import qualified Helpers.OffChain                  as OffChainHelpers
import qualified Helpers.OffChainEval              as OffChainEval

import AlwaysTruePolicy (alwaysTruePolicy)

---------------------------------------------------------------------------------------------------
------------------------------------------ TESTING ------------------------------------------------
-- | 'main' sets up and runs the tests for the 'AlwaysTruePolicy' policy.
-- The tests are organized into groups and are executed using Test.Tasty.
-- The `alwaysTrueTest` function is expected to succeed as the 'AlwaysTruePolicy' allows 
-- unrestricted minting and burning of tokens.
main :: IO ()
main = Tasty.defaultMain $ do
    Tasty.testGroup
        "Testing alwaysTrue policy"
        [ 
          Tasty.testGroup
            "Testing size and resources"
            [
                Tasty.testCase "Test Valid Mint; TxValidSize < 16Kb; Mem < 14Mb; Cpu < 10_000M" $
                  let
                      txDate = 6000
                      policy = alwaysTruePolicy
                      ctx = mintingTxContext policy txDate

                      getValidator :: LedgerApiV2.Address -> Maybe LedgerApiV2.Validator
                      getValidator _ = Nothing

                      getMintingPolicy :: LedgerApiV2.CurrencySymbol -> Maybe LedgerApiV2.MintingPolicy
                      getMintingPolicy _ = Just policy

                      (eval_log, eval_err, eval_size) = OffChainEval.testContext getValidator getMintingPolicy ctx
                  in do
                      eval_log `OffChainEval.assertContainsAnyOf` []
                      OffChainEval.assertBudgetAndSize eval_err eval_size OffChainEval.maxMemory OffChainEval.maxCPU OffChainEval.maxTxSize
            ]
          ,
          Tasty.testGroup
            "Must success"
            [ 
              good "Generic contract test" alwaysTrueTest
            ]
        ]
  where
    -- | 'good' is a helper function to denote a test that is expected to succeed.
    -- It uses 'Model.testNoErrors' to ensure the test passes without errors.
    good = Model.testNoErrors (Model.adaValue 10_000_000_000) Model.defaultBabbage

-- | 'AlwaysTruePolicyType' is a type alias for 'Model.TypedPolicy' specialized with unit type.
type AlwaysTruePolicyType = Model.TypedPolicy ()

-- | 'alwaysTruePolicyScript' converts the 'alwaysTruePolicy' into a typed policy script suitable for testing.
alwaysTruePolicyScript :: AlwaysTruePolicyType
alwaysTruePolicyScript = Model.TypedPolicy $ Model.toV2 alwaysTruePolicy

-- | 'setupUsers' initializes a list of users with a starting balance of 1000 Lovelace.
-- It uses 'replicateM' to create multiple users.
setupUsers :: Model.Run [LedgerApiV2.PubKeyHash]
setupUsers = replicateM 2 $ Model.newUser $ Model.ada (Model.Lovelace 1000)

-- | 'alwaysTrueTest' is a test function that checks the behavior of the 'AlwaysTruePolicy'.
-- It performs the following actions:
-- 1. Sets up users and retrieves their UTXO references.
-- 2. Mints and burns tokens using the 'AlwaysTruePolicy'.
-- 3. Verifies that the final balance is correct after minting and burning operations.
alwaysTrueTest :: Model.Run ()
alwaysTrueTest = do
  users <- setupUsers
  let [u1, _] = users
      cSymbol = Model.scriptCurrencySymbol alwaysTruePolicyScript
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
-- It includes the spending of tokens and minting with the 'AlwaysTruePolicy'.
burnTx :: Model.UserSpend -> LedgerApiV2.Value -> Model.Tx
burnTx usp val = 
  mconcat
    [ Model.userSpend usp
    , Model.mintValue alwaysTruePolicyScript () val
    ]

-- | 'mintTx' creates a transaction to mint tokens.
-- It includes the minting of tokens and payment to the specified public key hash.
mintTx :: LedgerApiV2.PubKeyHash -> LedgerApiV2.Value -> Model.Tx
mintTx pkh val =
  mconcat
    [ 
      Model.mintValue alwaysTruePolicyScript () val
    , Model.payToKey pkh val
    ]
    
-----------------------------------------------------------------------------------------

mintingTxContext :: LedgerApiV2.MintingPolicy -> LedgerApiV2.POSIXTime -> LedgerApiV2.ScriptContext
mintingTxContext policy txDate =
  let
    cSymbol = OffChainHelpers.getCurSymbolOfPolicy policy
    in
      OffChainEval.mkBaseValidMintingPolicyContext [] [] cSymbol
        OffChainEval.|> OffChainEval.setMintAndRedeemers [(LedgerApiV2.singleton cSymbol "Token" 1, LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData ())]
        OffChainEval.|> OffChainEval.setValidRange (OffChainEval.createValidRange txDate)

-----------------------------------------------------------------------------------------