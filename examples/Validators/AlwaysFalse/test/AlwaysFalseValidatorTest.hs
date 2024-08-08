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
import PlutusTx.Prelude (($), (&&), (.), (==), Maybe (Just, Nothing))
import Prelude (IO, mapM, mconcat)

import qualified Ledger.Ada                        as LedgerAda
import qualified Plutus.Model                      as Model
import qualified Plutus.V2.Ledger.Api              as LedgerApiV2
import qualified PlutusTx
import qualified Test.Tasty                        as Tasty
import qualified Test.Tasty.HUnit                  as Tasty

import qualified Helpers.OffChain                  as OffChainHelpers
import qualified Helpers.OffChainEval              as OffChainEval

import AlwaysFalseValidator (alwaysFalseValidator)

---------------------------------------------------------------------------------------------------
------------------------------------------ TESTING ------------------------------------------------

{- | This script is a testing module for the 'AlwaysFalseValidator'. It uses the 'Plutus.Model'
 testing framework to simulate balwaysFalsechain interactions.
-}

{- | 'main' is the entry point of the testing module.
 It sets up and runs a series of tests using the 'Test.Tasty' framework.
-}
main :: IO ()
main = Tasty.defaultMain $ do
  Tasty.testGroup
    "Testing alwaysFalse validator"
    [ 
      Tasty.testGroup
        "Testing size and resources"
        [
            Tasty.testCase "Test Valid Claim; TxValidSize < 16Kb; Mem < 14Mb; Cpu < 10_000M" $
            let
                validator = alwaysFalseValidator
                ctx = claimingTxContext validator 6000  

                getValidator :: LedgerApiV2.Address -> Maybe LedgerApiV2.Validator
                getValidator _ = Just validator

                getMintingPolicy :: LedgerApiV2.CurrencySymbol -> Maybe LedgerApiV2.MintingPolicy
                getMintingPolicy _ = Nothing

                (eval_log, eval_err, eval_size) = OffChainEval.testContext getValidator getMintingPolicy ctx
            in do
                eval_log `OffChainEval.assertContainsAnyOf` ["ERROR EVALUATING SCRIPT"]
                OffChainEval.assertBudgetAndSize eval_err eval_size OffChainEval.maxMemory OffChainEval.maxCPU OffChainEval.maxTxSize
        ]
      ,
      Tasty.testGroup
        "Must Fail"
        [ bad "Generic contract test" alwaysFalseTest
        ]
    ]
 where
  -- \| 'bad' is a helper function that marks a test as expected to fail.
  bad msg = good msg . Model.mustFail

  -- \| 'good' is a helper function that wraps a test case and ensures it runs without errors.
  good = Model.testNoErrors (Model.adaValue 10_000_000_000) Model.defaultBabbage

-- | 'AlwaysFalseValidatorType' is a type alias for the typed validator used in the tests.
type AlwaysFalseValidatorType = Model.TypedValidator () ()

-- | 'alwaysFalseValidatorScript' defines the typed validator for the alwaysFalse validator script.
alwaysFalseValidatorScript :: AlwaysFalseValidatorType
alwaysFalseValidatorScript = Model.TypedValidator $ Model.toV2 alwaysFalseValidator

-- | 'setupUsers' initializes two users with 1000 Lovelace each.
setupUsers :: Model.Run [LedgerApiV2.PubKeyHash]
setupUsers = replicateM 2 $ Model.newUser $ Model.ada (Model.Lovelace 1000)

-- | 'alwaysFalseTest' is a test case that checks the alwaysFalse validator's behavior.
alwaysFalseTest :: Model.Run ()
alwaysFalseTest = do
  users <- setupUsers
  let [u1, u2] = users
      val = Model.adaValue 100

  -- \| 'Model.checkBalance' ensures that the user's balance is updated correctly
  -- after submitting the transaction to give ADA to the script.
  Model.checkBalance (Model.gives u1 val alwaysFalseValidatorScript) $ do
    sp <- Model.spend u1 val
    Model.submitTx u1 $ giveTx sp val

  -- \| 'Model.utxoAt' retrieves the unspent transaction outputs (UTxOs) at the script address.
  utxos <- Model.utxoAt alwaysFalseValidatorScript
  let [(giftRef, giftOut)] = utxos

  -- \| 'Model.checkBalance' ensures that the user's balance is updated correctly
  -- after submitting the transaction to take ADA from the script.
  Model.checkBalance (Model.gives alwaysFalseValidatorScript (LedgerApiV2.txOutValue giftOut) u2) $ do
    Model.submitTx u2 $ takeTx u2 giftRef (LedgerApiV2.txOutValue giftOut)

  -- \| 'mapM Model.valueAt' retrieves the balance of each user.
  vals <- mapM Model.valueAt users
  let [v1, v2] = vals

  -- \| 'unless' checks if the final balances are as expected, logging an error if not.
  unless (v1 == Model.adaValue 900 && v2 == Model.adaValue 1100) $
    Model.logError "Final balances are incorrect"

-- | 'giveTx' creates a transaction that sends ADA to the alwaysFalse validator script.
giveTx :: Model.UserSpend -> LedgerApiV2.Value -> Model.Tx
giveTx usp val =
  mconcat
    [ Model.userSpend usp
    , Model.payToScript alwaysFalseValidatorScript (Model.HashDatum ()) val
    ]

-- | 'takeTx' creates a transaction that spends ADA from the alwaysFalse validator script to a given public key hash.
takeTx :: LedgerApiV2.PubKeyHash -> LedgerApiV2.TxOutRef -> LedgerApiV2.Value -> Model.Tx
takeTx pkh giftRef giftVal =
  mconcat
    [ Model.spendScript alwaysFalseValidatorScript giftRef () ()
    , Model.payToKey pkh giftVal
    ]

-----------------------------------------------------------------------------------------

claimingTxContext :: LedgerApiV2.Validator -> LedgerApiV2.POSIXTime ->  LedgerApiV2.ScriptContext
claimingTxContext validator txDate =
  let
    uTxO :: LedgerApiV2.TxOut
    uTxO = LedgerApiV2.TxOut
              (OffChainHelpers.addressValidator $ OffChainHelpers.hashValidator validator)
              (LedgerAda.lovelaceValueOf 100)
              (LedgerApiV2.OutputDatum $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData ())
              Nothing
    in
      OffChainEval.mkBaseValidatorContext [] [] 0
        OffChainEval.|> OffChainEval.setInputsAndAddRedeemers [(uTxO,  LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData ())]
        OffChainEval.|> OffChainEval.setValidRange (OffChainEval.createValidRange txDate)

-----------------------------------------------------------------------------------------