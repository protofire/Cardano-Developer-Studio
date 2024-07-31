
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Monad (replicateM, unless)
import PlutusTx.Prelude ((&&), ($), (==))
import Prelude (IO, mapM, mconcat)

import qualified Plutus.Model as Model
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Test.Tasty as Tasty

import FreeValidator (freeValidator)

-- | This module defines and runs tests for the 'FreeValidator' using the Plutus model and Tasty testing framework.

---------------------------------------------------------------------------------------------------
------------------------------------------ TESTING ------------------------------------------------
-- | The main entry point for the testing suite. It sets up and runs tests using Tasty.
main :: IO ()
main = Tasty.defaultMain $ do
    Tasty.testGroup
        "Testing free validator"
        [ Tasty.testGroup
            "Must succeed"
            [ 
              good "Generic contract test" freeTest 
            ]
        ]
  where
    -- | Helper functions to define tests that must fail or succeed.
    good = Model.testNoErrors (Model.adaValue 10_000_000_000) Model.defaultBabbage

-- | 'FreeValidatorType' is a type alias for a typed validator with unit data types for datum and redeemer.
type FreeValidatorType = Model.TypedValidator () ()

-- | 'freeValidatorScript' converts the 'freeValidator' into a typed validator script.
freeValidatorScript :: FreeValidatorType
freeValidatorScript = Model.TypedValidator $ Model.toV2 freeValidator

-- | 'setupUsers' sets up two users with an initial amount of ADA.
setupUsers :: Model.Run [LedgerApiV2.PubKeyHash]
setupUsers = replicateM 2 $ Model.newUser $ Model.ada (Model.Lovelace 1000)

-- | 'freeTest' runs a test that involves giving and taking ADA using the 'freeValidator'.
freeTest :: Model.Run ()
freeTest = do
  users <- setupUsers
  let [u1, u2] = users
      val = Model.adaValue 100

  sp <- Model.spend u1 val
  Model.submitTx u1 $ giveTx sp val

  utxos <- Model.utxoAt freeValidatorScript
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
    , Model.payToScript freeValidatorScript (Model.HashDatum ()) val
    ]

-- | 'takeTx' creates a transaction to take ADA from the script.
takeTx :: LedgerApiV2.PubKeyHash ->  LedgerApiV2.TxOutRef -> LedgerApiV2.Value -> Model.Tx
takeTx pkh giftRef giftVal =
  mconcat
    [ Model.spendScript freeValidatorScript giftRef () ()
    , Model.payToKey pkh giftVal
    ]

