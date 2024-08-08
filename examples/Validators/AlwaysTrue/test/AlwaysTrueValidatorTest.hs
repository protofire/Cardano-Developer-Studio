
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
import PlutusTx.Prelude ((&&), ($), (==), Maybe (Just, Nothing))
import Prelude (IO, mapM, mconcat)

import qualified Ledger.Ada                        as LedgerAda
import qualified Plutus.Model                      as Model
import qualified Plutus.V2.Ledger.Api              as LedgerApiV2
import qualified PlutusTx
import qualified Test.Tasty                        as Tasty
import qualified Test.Tasty.HUnit                  as Tasty

import qualified Helpers.OffChain                  as OffChainHelpers
import qualified Helpers.OffChainEval              as OffChainEval

import AlwaysTrueValidator (alwaysTrueValidator)

-- | This module defines and runs tests for the 'AlwaysTrueValidator' using the Plutus model and Tasty testing framework.

---------------------------------------------------------------------------------------------------
------------------------------------------ TESTING ------------------------------------------------
-- | The main entry point for the testing suite. It sets up and runs tests using Tasty.
main :: IO ()
main = Tasty.defaultMain $ do
    Tasty.testGroup
        "Testing alwaysTrue validator"
        [ 
          Tasty.testGroup
            "Testing size and resources"
            [
                Tasty.testCase "Test Valid Claim; TxValidSize < 16Kb; Mem < 14Mb; Cpu < 10_000M" $
                let
                    txDate = 6000
                    validator = alwaysTrueValidator
                    ctx = claimingTxContext validator 6000  

                    getValidator :: LedgerApiV2.Address -> Maybe LedgerApiV2.Validator
                    getValidator _ = Just validator

                    getMintingPolicy :: LedgerApiV2.CurrencySymbol -> Maybe LedgerApiV2.MintingPolicy
                    getMintingPolicy _ = Nothing

                    (eval_log, eval_err, eval_size) = OffChainEval.testContext getValidator getMintingPolicy ctx
                in do
                    eval_log `OffChainEval.assertContainsAnyOf` []
                    OffChainEval.assertBudgetAndSize eval_err eval_size OffChainEval.maxMemory OffChainEval.maxCPU OffChainEval.maxTxSize
              ]
          ,
          Tasty.testGroup
            "Must succeed"
            [ 
              good "Generic contract test" alwaysTrueTest 
            ]
        ]
  where
    -- | Helper functions to define tests that must fail or succeed.
    good = Model.testNoErrors (Model.adaValue 10_000_000_000) Model.defaultBabbage

-- | 'AlwaysTrueValidatorType' is a type alias for a typed validator with unit data types for datum and redeemer.
type AlwaysTrueValidatorType = Model.TypedValidator () ()

-- | 'alwaysTrueValidatorScript' converts the 'alwaysTrueValidator' into a typed validator script.
alwaysTrueValidatorScript :: AlwaysTrueValidatorType
alwaysTrueValidatorScript = Model.TypedValidator $ Model.toV2 alwaysTrueValidator

-- | 'setupUsers' sets up two users with an initial amount of ADA.
setupUsers :: Model.Run [LedgerApiV2.PubKeyHash]
setupUsers = replicateM 2 $ Model.newUser $ Model.ada (Model.Lovelace 1000)

-- | 'alwaysTrueTest' runs a test that involves giving and taking ADA using the 'alwaysTrueValidator'.
alwaysTrueTest :: Model.Run ()
alwaysTrueTest = do
  users <- setupUsers
  let [u1, u2] = users
      val = Model.adaValue 100

  sp <- Model.spend u1 val
  Model.submitTx u1 $ giveTx sp val

  utxos <- Model.utxoAt alwaysTrueValidatorScript
  let [(giftRef, giftOut)] = utxos
  Model.submitTx u2 $ takeTx u2 giftRef (LedgerApiV2.txOutValue giftOut)
  
  vals <- mapM Model.valueAt users
  let [v1, v2] = vals
  unless (v1 == Model.adaValue 900 && v2 == Model.adaValue 1100) $
    Model.logError "Final balances are incorrect"

-- | 'giveTx' creates a transaction to give ADA to the script.
giveTx :: Model.UserSpend -> LedgerApiV2.Value -> Model.Tx
giveTx usp val = 
  mconcat
    [ Model.userSpend usp
    , Model.payToScript alwaysTrueValidatorScript (Model.HashDatum ()) val
    ]

-- | 'takeTx' creates a transaction to take ADA from the script.
takeTx :: LedgerApiV2.PubKeyHash ->  LedgerApiV2.TxOutRef -> LedgerApiV2.Value -> Model.Tx
takeTx pkh giftRef giftVal =
  mconcat
    [ Model.spendScript alwaysTrueValidatorScript giftRef () ()
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