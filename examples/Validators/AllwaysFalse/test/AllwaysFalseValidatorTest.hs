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
import PlutusTx.Prelude (($), (&&), (.), (==))
import Prelude (IO, mapM, mconcat)

import qualified Plutus.Model as Model
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Test.Tasty as Tasty

import AllwaysFalseValidator (allwaysFalseValidator)

---------------------------------------------------------------------------------------------------
------------------------------------------ TESTING ------------------------------------------------

{- | This script is a testing module for the 'AllwaysFalseValidator'. It uses the 'Plutus.Model'
 testing framework to simulate ballwaysFalsechain interactions.
-}

{- | 'main' is the entry point of the testing module.
 It sets up and runs a series of tests using the 'Test.Tasty' framework.
-}
main :: IO ()
main = Tasty.defaultMain $ do
  Tasty.testGroup
    "Testing allwaysFalse validator"
    [ Tasty.testGroup
        "Must Fail"
        [ bad "Generic contract test" allwaysFalseTest
        ]
    ]
 where
  -- \| 'bad' is a helper function that marks a test as expected to fail.
  bad msg = good msg . Model.mustFail

  -- \| 'good' is a helper function that wraps a test case and ensures it runs without errors.
  good = Model.testNoErrors (Model.adaValue 10_000_000_000) Model.defaultBabbage

-- | 'AllwaysFalseValidatorType' is a type alias for the typed validator used in the tests.
type AllwaysFalseValidatorType = Model.TypedValidator () ()

-- | 'allwaysFalseValidatorScript' defines the typed validator for the allwaysFalse validator script.
allwaysFalseValidatorScript :: AllwaysFalseValidatorType
allwaysFalseValidatorScript = Model.TypedValidator $ Model.toV2 allwaysFalseValidator

-- | 'setupUsers' initializes two users with 1000 Lovelace each.
setupUsers :: Model.Run [LedgerApiV2.PubKeyHash]
setupUsers = replicateM 2 $ Model.newUser $ Model.ada (Model.Lovelace 1000)

-- | 'allwaysFalseTest' is a test case that checks the allwaysFalse validator's behavior.
allwaysFalseTest :: Model.Run ()
allwaysFalseTest = do
  users <- setupUsers
  let [u1, u2] = users
      val = Model.adaValue 100

  -- \| 'Model.checkBalance' ensures that the user's balance is updated correctly
  -- after submitting the transaction to give ADA to the script.
  Model.checkBalance (Model.gives u1 val allwaysFalseValidatorScript) $ do
    sp <- Model.spend u1 val
    Model.submitTx u1 $ giveTx sp val

  -- \| 'Model.utxoAt' retrieves the unspent transaction outputs (UTxOs) at the script address.
  utxos <- Model.utxoAt allwaysFalseValidatorScript
  let [(giftRef, giftOut)] = utxos

  -- \| 'Model.checkBalance' ensures that the user's balance is updated correctly
  -- after submitting the transaction to take ADA from the script.
  Model.checkBalance (Model.gives allwaysFalseValidatorScript (LedgerApiV2.txOutValue giftOut) u2) $ do
    Model.submitTx u2 $ takeTx u2 giftRef (LedgerApiV2.txOutValue giftOut)

  -- \| 'mapM Model.valueAt' retrieves the balance of each user.
  vals <- mapM Model.valueAt users
  let [v1, v2] = vals

  -- \| 'unless' checks if the final balances are as expected, logging an error if not.
  unless (v1 == Model.adaValue 900 && v2 == Model.adaValue 1100) $
    Model.logError "Final balances are incorrect"

-- | 'giveTx' creates a transaction that sends ADA to the allwaysFalse validator script.
giveTx :: Model.UserSpend -> LedgerApiV2.Value -> Model.Tx
giveTx usp val =
  mconcat
    [ Model.userSpend usp
    , Model.payToScript allwaysFalseValidatorScript (Model.HashDatum ()) val
    ]

-- | 'takeTx' creates a transaction that spends ADA from the allwaysFalse validator script to a given public key hash.
takeTx :: LedgerApiV2.PubKeyHash -> LedgerApiV2.TxOutRef -> LedgerApiV2.Value -> Model.Tx
takeTx pkh giftRef giftVal =
  mconcat
    [ Model.spendScript allwaysFalseValidatorScript giftRef () ()
    , Model.payToKey pkh giftVal
    ]
