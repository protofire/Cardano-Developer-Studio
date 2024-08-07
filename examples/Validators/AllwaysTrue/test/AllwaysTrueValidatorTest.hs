
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
import PlutusTx.Prelude ((&&), ($), (==))
import Prelude (IO, mapM, mconcat)

import qualified Plutus.Model as Model
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Test.Tasty as Tasty

import AllwaysTrueValidator (allwaysTrueValidator)

-- | This module defines and runs tests for the 'AllwaysTrueValidator' using the Plutus model and Tasty testing framework.

---------------------------------------------------------------------------------------------------
------------------------------------------ TESTING ------------------------------------------------
-- | The main entry point for the testing suite. It sets up and runs tests using Tasty.
main :: IO ()
main = Tasty.defaultMain $ do
    Tasty.testGroup
        "Testing allwaysTrue validator"
        [ Tasty.testGroup
            "Must succeed"
            [ 
              good "Generic contract test" allwaysTrueTest 
            ]
        ]
  where
    -- | Helper functions to define tests that must fail or succeed.
    good = Model.testNoErrors (Model.adaValue 10_000_000_000) Model.defaultBabbage

-- | 'AllwaysTrueValidatorType' is a type alias for a typed validator with unit data types for datum and redeemer.
type AllwaysTrueValidatorType = Model.TypedValidator () ()

-- | 'allwaysTrueValidatorScript' converts the 'allwaysTrueValidator' into a typed validator script.
allwaysTrueValidatorScript :: AllwaysTrueValidatorType
allwaysTrueValidatorScript = Model.TypedValidator $ Model.toV2 allwaysTrueValidator

-- | 'setupUsers' sets up two users with an initial amount of ADA.
setupUsers :: Model.Run [LedgerApiV2.PubKeyHash]
setupUsers = replicateM 2 $ Model.newUser $ Model.ada (Model.Lovelace 1000)

-- | 'allwaysTrueTest' runs a test that involves giving and taking ADA using the 'allwaysTrueValidator'.
allwaysTrueTest :: Model.Run ()
allwaysTrueTest = do
  users <- setupUsers
  let [u1, u2] = users
      val = Model.adaValue 100

  sp <- Model.spend u1 val
  Model.submitTx u1 $ giveTx sp val

  utxos <- Model.utxoAt allwaysTrueValidatorScript
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
    , Model.payToScript allwaysTrueValidatorScript (Model.HashDatum ()) val
    ]

-- | 'takeTx' creates a transaction to take ADA from the script.
takeTx :: LedgerApiV2.PubKeyHash ->  LedgerApiV2.TxOutRef -> LedgerApiV2.Value -> Model.Tx
takeTx pkh giftRef giftVal =
  mconcat
    [ Model.spendScript allwaysTrueValidatorScript giftRef () ()
    , Model.payToKey pkh giftVal
    ]

