{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- HLINT ignore "Reduce duplication"          -}

module Main where

import Control.Monad (replicateM, unless)
import PlutusTx.Prelude ((&&), (.), ($), (==))
import Prelude (IO, mapM, mconcat)

import qualified Plutus.Model as Model
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Test.Tasty as Tasty

import ParamCheckSignatureValidator (paramCheckSignatureValidator)
import DatumCheckSignatureValidator (datumCheckSignatureValidator)
---------------------------------------------------------------------------------------------------
------------------------------------------ TESTING ------------------------------------------------
-- | Main entry point for running tests
--
-- This function sets up and runs the test suite for the signature validation scripts.
--
-- It uses `Tasty` to organize and execute the tests for both parameter-based and datum-based signature validation.
main :: IO ()
main = Tasty.defaultMain $ do
    Tasty.testGroup
        "Testing checkSignature validator"
        [ Tasty.testGroup
            "Test using parameters"
            [ 
              good "User 1 deploys contract for User 2 and User 2 claims it" paramCheckSignatureTest
              , bad "User 1 deploys contract for User 1 and User 2 tries to claim it" paramCheckSignatureTestFail 
            ],
          Tasty.testGroup
            "Test using Datum information"
            [ 
              good "User 1 deploys contract for User 2 and User 2 claims it" datumCheckSignatureTest
              , bad "User 1 deploys contract for User 1 and User 2 tries to claim it" datumCheckSignatureTestFail 
            ]

        ]
  where
    bad msg = good msg . Model.mustFail
    good = Model.testNoErrors (Model.adaValue 10_000_000_000) Model.defaultBabbage

-- | Type alias for a datum check signature validator
type DatumCheckSignatureValidatorType = Model.TypedValidator LedgerApiV2.PubKeyHash ()

-- | Type alias for a parameter check signature validator
type ParamCheckSignatureValidatorType = Model.TypedValidator () ()

-- | Creates a TypedValidator for the datum check signature validator
datumCheckSignatureValidatorScript :: DatumCheckSignatureValidatorType
datumCheckSignatureValidatorScript = Model.TypedValidator $ Model.toV2 datumCheckSignatureValidator

-- | Creates a TypedValidator for the parameter check signature validator with the given public key hash
paramCheckSignatureValidatorScript :: LedgerApiV2.PubKeyHash -> ParamCheckSignatureValidatorType
paramCheckSignatureValidatorScript signatureKey = Model.TypedValidator $ Model.toV2 
                                        $ paramCheckSignatureValidator signatureKey

-- | Sets up two users with an initial balance
--
-- This function creates two new users, each with 1000 Lovelace.
--
-- Returns:
--   - A list containing the public key hashes of the two users.
setupUsers :: Model.Run [LedgerApiV2.PubKeyHash]
setupUsers = replicateM 2 $ Model.newUser $ Model.ada (Model.Lovelace 1000)

-- | Tests the datum check signature validator
--
-- This test verifies the correct operation of the datum check signature validator by:
-- 1. Deploying a contract from User 1 to User 2.
-- 2. Claiming the contract by User 2.
-- 3. Verifying that User 1 and User 2 have the correct final balances.
datumCheckSignatureTest :: Model.Run ()
datumCheckSignatureTest = do
  users <- setupUsers
  let [u1, u2] = users
      val = Model.adaValue 100

  sp <- Model.spend u1 val
  Model.submitTx u1 $ datumGiveTx sp u2 val

  utxos <- Model.utxoAt datumCheckSignatureValidatorScript
  let [(giftRef, giftOut)] = utxos
  Model.submitTx u2 $ datumTakeTx u2 giftRef (LedgerApiV2.txOutValue giftOut)
  
  vals <- mapM Model.valueAt users
  let [v1, v2] = vals
  unless (v1 == Model.adaValue 900 && v2 == Model.adaValue 1100) $
    Model.logError "Final balances are incorrect"

-- | Tests the datum check signature validator with a failure scenario
--
-- This test checks that the datum check signature validator fails correctly when:
-- 1. User 1 deploys a contract to themselves instead of User 2.
-- 2. User 2 tries to claim the contract.
datumCheckSignatureTestFail :: Model.Run ()
datumCheckSignatureTestFail = do
  users <- setupUsers
  let [u1, u2] = users
      val = Model.adaValue 100

  sp <- Model.spend u1 val
  Model.submitTx u1 $ datumGiveTx sp u1 val

  utxos <- Model.utxoAt datumCheckSignatureValidatorScript
  let [(giftRef, giftOut)] = utxos
  Model.submitTx u2 $ datumTakeTx u2 giftRef (LedgerApiV2.txOutValue giftOut)
  
  vals <- mapM Model.valueAt users
  let [v1, v2] = vals
  unless (v1 == Model.adaValue 900 && v2 == Model.adaValue 1100) $
    Model.logError "Final balances are incorrect"

-- | Creates a transaction to give an amount of Ada to the datum check signature validator
--
-- Parameters:
--   - `usp`: User spend information.
--   - `takePkh`: The public key hash of the recipient.
--   - `val`: The amount of Ada to give.
--
-- Returns:
--   - A transaction that sends Ada to the datum check signature validator with an inline datum.
datumGiveTx :: Model.UserSpend -> LedgerApiV2.PubKeyHash -> LedgerApiV2.Value -> Model.Tx
datumGiveTx usp takePkh val = 
  mconcat
    [ Model.userSpend usp
    , Model.payToScript datumCheckSignatureValidatorScript (Model.InlineDatum takePkh) val
    ]

-- | Creates a transaction to take an amount of Ada from the datum check signature validator
--
-- Parameters:
--   - `takePkh`: The public key hash of the recipient.
--   - `giftRef`: Reference to the output to spend.
--   - `giftVal`: The amount of Ada to take.
--
-- Returns:
--   - A transaction that spends from the datum check signature validator and pays to the recipient.
datumTakeTx :: LedgerApiV2.PubKeyHash ->  LedgerApiV2.TxOutRef -> LedgerApiV2.Value -> Model.Tx
datumTakeTx takePkh giftRef giftVal =
  mconcat
    [ Model.spendScript datumCheckSignatureValidatorScript giftRef () takePkh
    , Model.payToKey takePkh giftVal
    ]

-- | Tests the parameter check signature validator
--
-- This test verifies the correct operation of the parameter check signature validator by:
-- 1. Deploying a contract from User 1 to User 2.
-- 2. Claiming the contract by User 2.
-- 3. Verifying that User 1 and User 2 have the correct final balances.
paramCheckSignatureTest :: Model.Run ()
paramCheckSignatureTest = do
  users <- setupUsers
  let [u1, u2] = users
      val = Model.adaValue 100
      paramCheckScript = paramCheckSignatureValidatorScript u2

  sp <- Model.spend u1 val
  Model.submitTx u1 $ paramGiveTx sp paramCheckScript val

  utxos <- Model.utxoAt paramCheckScript
  let [(giftRef, giftOut)] = utxos
  Model.submitTx u2 $ paramTakeTx u2 paramCheckScript giftRef (LedgerApiV2.txOutValue giftOut)
  
  vals <- mapM Model.valueAt users
  let [v1, v2] = vals
  unless (v1 == Model.adaValue 900 && v2 == Model.adaValue 1100) $
    Model.logError "Final balances are incorrect"

-- | Tests the parameter check signature validator with a failure scenario
--
-- This test checks that the parameter check signature validator fails correctly when:
-- 1. User 1 deploys a contract to themselves instead of User 2.
-- 2. User 2 tries to claim the contract.
paramCheckSignatureTestFail :: Model.Run ()
paramCheckSignatureTestFail = do
  users <- setupUsers
  let [u1, u2] = users
      val = Model.adaValue 100
      paramCheckScriptFail = paramCheckSignatureValidatorScript u1
  sp <- Model.spend u1 val
  Model.submitTx u1 $ paramGiveTx sp paramCheckScriptFail val

  utxos <- Model.utxoAt paramCheckScriptFail
  let [(giftRef, giftOut)] = utxos
  Model.submitTx u2 $ paramTakeTx u2 paramCheckScriptFail giftRef (LedgerApiV2.txOutValue giftOut)
  
  vals <- mapM Model.valueAt users
  let [v1, v2] = vals
  unless (v1 == Model.adaValue 900 && v2 == Model.adaValue 1100) $
    Model.logError "Final balances are incorrect"

-- | Creates a transaction to give an amount of Ada to the parameter check signature validator
--
-- Parameters:
--   - `usp`: User spend information.
--   - `paramCheckScript`: The parameter check signature validator script.
--   - `val`: The amount of Ada to give.
--
-- Returns:
--   - A transaction that sends Ada to the parameter check signature validator with a hash datum.
paramGiveTx :: Model.UserSpend -> ParamCheckSignatureValidatorType -> LedgerApiV2.Value -> Model.Tx
paramGiveTx usp paramCheckScript val = 
  mconcat
    [ Model.userSpend usp
    , Model.payToScript paramCheckScript (Model.HashDatum ()) val
    ]

-- | Creates a transaction to take an amount of Ada from the parameter check signature validator
--
-- Parameters:
--   - `takePkh`: The public key hash of the recipient.
--   - `paramCheckScript`: The parameter check signature validator script.
--   - `giftRef`: Reference to the output to spend.
--   - `giftVal`: The amount of Ada to take.
--
-- Returns:
--   - A transaction that spends from the parameter check signature validator and pays to the recipient.
paramTakeTx :: LedgerApiV2.PubKeyHash -> ParamCheckSignatureValidatorType -> LedgerApiV2.TxOutRef -> LedgerApiV2.Value -> Model.Tx
paramTakeTx takePkh paramCheckScript giftRef giftVal =
  mconcat
    [ Model.spendScript paramCheckScript giftRef () ()
    , Model.payToKey takePkh giftVal
    ]

