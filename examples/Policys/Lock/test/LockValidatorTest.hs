{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Monad (replicateM, unless)
import PlutusTx.Prelude (Bool, (&&), (.), ($), (==))
import Prelude (IO, mapM, mconcat)

import qualified Plutus.Model as Model
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Test.Tasty as Tasty

import LockValidator (lockValidator)
---------------------------------------------------------------------------------------------------
------------------------------------------ TESTING ------------------------------------------------
type IsGood = Bool

main :: IO ()
main = Tasty.defaultMain $ do
    Tasty.testGroup
        "Testing lock validator"
        [ Tasty.testGroup
            "Must Fail"
            [ 
              bad "Generic contract test" lockTest 
            ]
        ]
  where
    bad msg = good msg . Model.mustFail
    good = Model.testNoErrors (Model.adaValue 10_000_000_000) Model.defaultBabbage


type LockValidatorType = Model.TypedValidator () ()

lockValidatorScript :: LockValidatorType
lockValidatorScript = Model.TypedValidator $ Model.toV2 lockValidator

setupUsers :: Model.Run [LedgerApiV2.PubKeyHash]
setupUsers = replicateM 2 $ Model.newUser $ Model.ada (Model.Lovelace 1000)

lockTest :: Model.Run ()
lockTest = do
  users <- setupUsers
  let [u1, u2] = users
      val = Model.adaValue 100
  Model.checkBalance (Model.gives u1 val lockValidatorScript) $ do
    sp <- Model.spend u1 val
    Model.submitTx u1 $ giveTx sp val

  utxos <- Model.utxoAt lockValidatorScript
  let [(giftRef, giftOut)] = utxos
  Model.checkBalance (Model.gives lockValidatorScript (LedgerApiV2.txOutValue giftOut) u2) $ do
    Model.submitTx u2 $ takeTx u2 giftRef (LedgerApiV2.txOutValue giftOut)
  
  vals <- mapM Model.valueAt users
  let [v1, v2] = vals
  unless (v1 == Model.adaValue 900 && v2 == Model.adaValue 1100) $
    Model.logError "Final balances are incorrect"

giveTx :: Model.UserSpend -> LedgerApiV2.Value -> Model.Tx
giveTx usp val = 
  mconcat
    [ Model.userSpend usp
    , Model.payToScript lockValidatorScript (Model.HashDatum ()) val
    ]

takeTx :: LedgerApiV2.PubKeyHash ->  LedgerApiV2.TxOutRef -> LedgerApiV2.Value -> Model.Tx
takeTx pkh giftRef giftVal =
  mconcat
    [ Model.spendScript lockValidatorScript giftRef () ()
    , Model.payToKey pkh giftVal
    ]


