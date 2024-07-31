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

import ParamCheckSignatureValidator (paramCheckSignatureValidator)
import DatumCheckSignatureValidator (datumCheckSignatureValidator)
---------------------------------------------------------------------------------------------------
------------------------------------------ TESTING ------------------------------------------------
type IsGood = Bool

main :: IO ()
main = Tasty.defaultMain $ do
    Tasty.testGroup
        "Testing checkSignature validator"
        [ Tasty.testGroup
            "Test using parameters"
            [ 
              good "User 1 deploy contract for User 2 and User 2 claim it" paramCheckSignatureTest
              , bad "User 1 deploy contract for User 1 and User 2 try claim it" paramCheckSignatureTestFail 
            ],
          Tasty.testGroup
            "Test using Datum information"
            [ 
              good "User 1 deploy contract for User 2 and User 2 claim it" datumCheckSignatureTest
              , bad "User 1 deploy contract for User 1 and User 2 try claim it" datumCheckSignatureTestFail 
            ]

        ]
  where
    bad msg = good msg . Model.mustFail
    good = Model.testNoErrors (Model.adaValue 10_000_000_000) Model.defaultBabbage


type DatumCheckSignatureValidatorType = Model.TypedValidator LedgerApiV2.PubKeyHash ()
type ParamCheckSignatureValidatorType = Model.TypedValidator () ()

datumCheckSignatureValidatorScript :: DatumCheckSignatureValidatorType
datumCheckSignatureValidatorScript = Model.TypedValidator $ Model.toV2 datumCheckSignatureValidator

paramCheckSignatureValidatorScript :: LedgerApiV2.PubKeyHash -> ParamCheckSignatureValidatorType
paramCheckSignatureValidatorScript signatureKey = Model.TypedValidator $ Model.toV2 
                                        $ paramCheckSignatureValidator signatureKey

setupUsers :: Model.Run [LedgerApiV2.PubKeyHash]
setupUsers = replicateM 2 $ Model.newUser $ Model.ada (Model.Lovelace 1000)

--------------------------------------------------------------------------------

datumCheckSignatureTest :: Model.Run ()
datumCheckSignatureTest = do
  users <- setupUsers
  let [u1, u2] = users
      val = Model.adaValue 100
  Model.checkBalance (Model.gives u1 val datumCheckSignatureValidatorScript) $ do
    sp <- Model.spend u1 val
    Model.submitTx u1 $ datumGiveTx sp u2 val

  utxos <- Model.utxoAt datumCheckSignatureValidatorScript
  let [(giftRef, giftOut)] = utxos
  Model.checkBalance (Model.gives datumCheckSignatureValidatorScript (LedgerApiV2.txOutValue giftOut) u2) $ do
    Model.submitTx u2 $ datumTakeTx u2 giftRef (LedgerApiV2.txOutValue giftOut)
  
  vals <- mapM Model.valueAt users
  let [v1, v2] = vals
  unless (v1 == Model.adaValue 900 && v2 == Model.adaValue 1100) $
    Model.logError "Final balances are incorrect"

datumCheckSignatureTestFail :: Model.Run ()
datumCheckSignatureTestFail = do
  users <- setupUsers
  let [u1, u2] = users
      val = Model.adaValue 100
  Model.checkBalance (Model.gives u1 val datumCheckSignatureValidatorScript) $ do
    sp <- Model.spend u1 val
    Model.submitTx u1 $ datumGiveTx sp u1 val

  utxos <- Model.utxoAt datumCheckSignatureValidatorScript
  let [(giftRef, giftOut)] = utxos
  Model.checkBalance (Model.gives datumCheckSignatureValidatorScript (LedgerApiV2.txOutValue giftOut) u2) $ do
    Model.submitTx u2 $ datumTakeTx u2 giftRef (LedgerApiV2.txOutValue giftOut)
  
  vals <- mapM Model.valueAt users
  let [v1, v2] = vals
  unless (v1 == Model.adaValue 900 && v2 == Model.adaValue 1100) $
    Model.logError "Final balances are incorrect"


datumGiveTx :: Model.UserSpend -> LedgerApiV2.PubKeyHash -> LedgerApiV2.Value -> Model.Tx
datumGiveTx usp takePkh val = 
  mconcat
    [ Model.userSpend usp
    , Model.payToScript datumCheckSignatureValidatorScript (Model.InlineDatum takePkh) val
    ]

datumTakeTx :: LedgerApiV2.PubKeyHash ->  LedgerApiV2.TxOutRef -> LedgerApiV2.Value -> Model.Tx
datumTakeTx takePkh giftRef giftVal =
  mconcat
    [ Model.spendScript datumCheckSignatureValidatorScript giftRef () takePkh
    , Model.payToKey takePkh giftVal
    ]

--------------------------------------------------------------------------------
paramCheckSignatureTest :: Model.Run ()
paramCheckSignatureTest = do
  users <- setupUsers
  let [u1, u2] = users
      val = Model.adaValue 100
      paramCheckScript = paramCheckSignatureValidatorScript u2
  Model.checkBalance (Model.gives u1 val paramCheckScript) $ do
    sp <- Model.spend u1 val
    Model.submitTx u1 $ paramGiveTx sp paramCheckScript val

  utxos <- Model.utxoAt paramCheckScript
  let [(giftRef, giftOut)] = utxos
  Model.checkBalance (Model.gives paramCheckScript (LedgerApiV2.txOutValue giftOut) u2) $ do
    Model.submitTx u2 $ paramTakeTx u2 paramCheckScript giftRef (LedgerApiV2.txOutValue giftOut)
  
  vals <- mapM Model.valueAt users
  let [v1, v2] = vals
  unless (v1 == Model.adaValue 900 && v2 == Model.adaValue 1100) $
    Model.logError "Final balances are incorrect"

paramCheckSignatureTestFail :: Model.Run ()
paramCheckSignatureTestFail = do
  users <- setupUsers
  let [u1, u2] = users
      val = Model.adaValue 100
      paramCheckScriptFail = paramCheckSignatureValidatorScript u1
  Model.checkBalance (Model.gives u1 val paramCheckScriptFail) $ do
    sp <- Model.spend u1 val
    Model.submitTx u1 $ paramGiveTx sp paramCheckScriptFail val

  utxos <- Model.utxoAt paramCheckScriptFail
  let [(giftRef, giftOut)] = utxos
  Model.checkBalance (Model.gives paramCheckScriptFail (LedgerApiV2.txOutValue giftOut) u2) $ do
    Model.submitTx u2 $ paramTakeTx u2 paramCheckScriptFail giftRef (LedgerApiV2.txOutValue giftOut)
  
  vals <- mapM Model.valueAt users
  let [v1, v2] = vals
  unless (v1 == Model.adaValue 900 && v2 == Model.adaValue 1100) $
    Model.logError "Final balances are incorrect"


paramGiveTx :: Model.UserSpend -> ParamCheckSignatureValidatorType -> LedgerApiV2.Value -> Model.Tx
paramGiveTx usp paramCheckScript val = 
  mconcat
    [ Model.userSpend usp
    , Model.payToScript paramCheckScript (Model.HashDatum ()) val
    ]

paramTakeTx :: LedgerApiV2.PubKeyHash -> ParamCheckSignatureValidatorType -> LedgerApiV2.TxOutRef -> LedgerApiV2.Value -> Model.Tx
paramTakeTx takePkh paramCheckScript giftRef giftVal =
  mconcat
    [ Model.spendScript paramCheckScript giftRef () ()
    , Model.payToKey takePkh giftVal
    ]


