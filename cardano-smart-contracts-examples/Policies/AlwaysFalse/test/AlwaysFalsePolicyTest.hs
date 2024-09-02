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
import PlutusTx.Prelude (Semigroup ((<>)), (.), ($), (==), Maybe (Just, Nothing))
import Prelude (IO, mapM, mconcat)

import qualified Plutus.Model as Model
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Test.Tasty                        as Tasty
import qualified Test.Tasty.HUnit                  as Tasty
import qualified PlutusTx

import qualified Helpers.OffChain                  as OffChainHelpers
import qualified Helpers.OffChainEval              as OffChainEval

import AlwaysFalsePolicy (alwaysFalsePolicy)

---------------------------------------------------------------------------------------------------
------------------------------------------ TESTING ------------------------------------------------
-- | Main function that runs the test suite using Tasty.
-- It groups tests into categories and runs them.
main :: IO ()
main = Tasty.defaultMain $ do
    Tasty.testGroup
        "Testing alwaysFalse policy"
        [   
           Tasty.testGroup
            "Testing size and resources"
            [
                Tasty.testCase "Test Valid Mint; TxValidSize < 16Kb; Mem < 14Mb; Cpu < 10_000M" $
                let
                      txDate = 6000
                      policy = alwaysFalsePolicy
                      ctx = mintingTxContext policy txDate

                      getValidator :: LedgerApiV2.Address -> Maybe LedgerApiV2.Validator
                      getValidator _ = Nothing

                      getMintingPolicy :: LedgerApiV2.CurrencySymbol -> Maybe LedgerApiV2.MintingPolicy
                      getMintingPolicy _ = Just policy

                      (eval_log, eval_err, eval_size) = OffChainEval.testContext getValidator getMintingPolicy ctx
                  in do
                      eval_log `OffChainEval.assertContainsAnyOf` ["ERROR EVALUATING SCRIPT"]
                      OffChainEval.assertBudgetAndSize eval_err eval_size OffChainEval.maxMemory OffChainEval.maxCPU OffChainEval.maxTxSize
            ]
          ,
          Tasty.testGroup
            "Must Fail"
            [ 
              bad "Generic contract test" alwaysFalseTest 
            ]
        ]
  where
    -- | Mark the test as a failure case; expects the test to fail.
    bad msg = good msg . Model.mustFail
    
    -- | Mark the test as a success case; expects the test to pass without errors.
    good = Model.testNoErrors (Model.adaValue 10_000_000_000) Model.defaultBabbage

-- | Type alias for the alwaysFalse policy with no specific parameters.
type AlwaysFalsePolicyType = Model.TypedPolicy ()

-- | Define the alwaysFalse policy script as a typed policy.
alwaysFalsePolicyScript :: AlwaysFalsePolicyType
alwaysFalsePolicyScript = Model.TypedPolicy $ Model.toV2 alwaysFalsePolicy

-- | Set up users with an initial balance of 1000 Lovelace.
-- This function creates two users for testing.
setupUsers :: Model.Run [LedgerApiV2.PubKeyHash]
setupUsers = replicateM 2 $ Model.newUser $ Model.ada (Model.Lovelace 1000)

-- | Test function to check if the token minting fails as expected.
-- In this test, we try to mint a token using the alwaysFalse policy,
-- which is expected to fail due to the policy's logic.
alwaysFalseTest :: Model.Run ()
alwaysFalseTest = do
  users <- setupUsers
  let [u1, _] = users
      cSymbol = Model.scriptCurrencySymbol alwaysFalsePolicyScript
      mintValue = LedgerApiV2.singleton cSymbol "Token" 1

  -- Attempt to mint a token and expect failure.
  Model.submitTx u1 $ mintTx u1 mintValue

  -- Check the final balances.
  vals <- mapM Model.valueAt users
  let [v1, _] = vals
  unless (v1 == (Model.adaValue 1000 <> mintValue)) $
    Model.logError "The token couldn't be minted"

-- | Create a transaction to mint a token.
-- This function specifies the policy, value, and recipient.
mintTx :: LedgerApiV2.PubKeyHash -> LedgerApiV2.Value -> Model.Tx
mintTx pkh val =
  mconcat
    [ 
      Model.mintValue alwaysFalsePolicyScript () val
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