{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Monad (replicateM)
import PlutusTx.Prelude (Bool, (.), ($)) 
import Prelude (IO, mconcat)

import qualified Plutus.Model as Model
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Test.Tasty as Tasty

import ParamCheckBeforeDeadlineValidator (paramCheckBeforeDeadlineValidator)
import DatumCheckBeforeDeadlineValidator (datumCheckBeforeDeadlineValidator)
import ParamCheckAfterDeadlineValidator (paramCheckAfterDeadlineValidator)
import DatumCheckAfterDeadlineValidator (datumCheckAfterDeadlineValidator)
import Plutus.Model.Fork.Ledger.Slot (Slot)
---------------------------------------------------------------------------------------------------
------------------------------------------ TESTING ------------------------------------------------
type IsGood = Bool

main :: IO ()
main = Tasty.defaultMain $ do
  Tasty.testGroup
    "Testing time checks"
    [ Tasty.testGroup
        "Test Claim Before the deadline using parameters"
        [ bad   "Deadline: 6000; Claim time 7000" $ datumCheckBeforeDeadlineTest 6000 (-999) 1000 2000
        , good  "Deadline: 6000; Claim time 5000" $ datumCheckBeforeDeadlineTest 6000 (-999) 1000 0
        ],
    Tasty.testGroup
        "Test claim after the deadline using parameters"
        [ good  "Deadline: 6000; Claim time 7000" $ datumCheckAfterDeadlineTest 6000 (-999) 1000 2000
        , bad   "Deadline: 6000; Claim time 5000" $ datumCheckAfterDeadlineTest 6000 (-999) 1000 0
        ],
    Tasty.testGroup
        "Test Claim Before the deadline using datums"
        [ bad   "Deadline: 6000; Claim time 7000" $ paramCheckBeforeDeadlineTest 6000 (-999) 1000 2000
        , good  "Deadline: 6000; Claim time 5000" $ paramCheckBeforeDeadlineTest 6000 (-999) 1000 0
        ],
    Tasty.testGroup
        "Test claim after the deadline using datums"
        [ good  "Deadline: 6000; Claim time 7000" $ paramCheckAfterDeadlineTest 6000 (-999) 1000 2000
        , bad   "Deadline: 6000; Claim time 5000" $ paramCheckAfterDeadlineTest 6000 (-999) 1000 0
        ]
    ]
  where
    bad msg = good msg . Model.mustFail
    good = Model.testNoErrors (Model.adaValue 10_000_000_000) Model.defaultBabbage

setupUsers :: Model.Run [LedgerApiV2.PubKeyHash]
setupUsers = replicateM 2 $ Model.newUser $ Model.ada (Model.Lovelace 1000)

type DatumCheckDateValidatorType = Model.TypedValidator LedgerApiV2.POSIXTime ()
type ParamCheckDateValidatorType = Model.TypedValidator () ()

datumCheckAfterDeadlineValidatorScript :: DatumCheckDateValidatorType
datumCheckAfterDeadlineValidatorScript = Model.TypedValidator $ Model.toV2 datumCheckAfterDeadlineValidator

paramCheckAfterDeadlineValidatorScript :: LedgerApiV2.POSIXTime -> ParamCheckDateValidatorType
paramCheckAfterDeadlineValidatorScript deadline = Model.TypedValidator $ Model.toV2 
                                        $ paramCheckAfterDeadlineValidator deadline

datumCheckBeforeDeadlineValidatorScript :: DatumCheckDateValidatorType
datumCheckBeforeDeadlineValidatorScript = Model.TypedValidator $ Model.toV2 datumCheckBeforeDeadlineValidator

paramCheckBeforeDeadlineValidatorScript :: LedgerApiV2.POSIXTime -> ParamCheckDateValidatorType
paramCheckBeforeDeadlineValidatorScript deadline = Model.TypedValidator $ Model.toV2 
                                        $ paramCheckBeforeDeadlineValidator deadline

--------------------------------------------------------------------------------
paramCheckAfterDeadlineTest :: LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Slot -> Model.Run ()
paramCheckAfterDeadlineTest deadline curMinT curMaxT wSlot = do
  users <- setupUsers
  let [u1, u2] = users
      paramCheckAfterDeadlineScript = paramCheckAfterDeadlineValidatorScript deadline
  makeParamCheckDate u1 u2 paramCheckAfterDeadlineScript curMinT curMaxT wSlot

paramCheckBeforeDeadlineTest :: LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Slot -> Model.Run ()
paramCheckBeforeDeadlineTest deadline curMinT curMaxT wSlot = do
  users <- setupUsers
  let [u1, u2] = users
      paramCheckBeforeDeadlineScript = paramCheckBeforeDeadlineValidatorScript deadline
  makeParamCheckDate u1 u2 paramCheckBeforeDeadlineScript curMinT curMaxT wSlot

makeParamCheckDate :: LedgerApiV2.PubKeyHash -> LedgerApiV2.PubKeyHash -> ParamCheckDateValidatorType -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Slot -> Model.Run ()
makeParamCheckDate sigUser receiver paramCheckDateScript curMinT curMaxT wSlot = do
  let val = Model.adaValue 100
  Model.checkBalance (Model.gives sigUser val paramCheckDateScript) $ do
    sp <- Model.spend sigUser val
    Model.submitTx sigUser $ vestingTxWithParams paramCheckDateScript sp val
  Model.waitNSlots wSlot
  utxos <- Model.utxoAt paramCheckDateScript
  let [(contractRef, contractOut)] = utxos
  Model.checkBalance (Model.gives paramCheckDateScript (LedgerApiV2.txOutValue contractOut) receiver) $ do
    range <- Model.currentTimeInterval curMinT curMaxT
    tx <- Model.validateIn range $ claimingTxWithParams receiver paramCheckDateScript contractRef (LedgerApiV2.txOutValue contractOut)
    Model.submitTx sigUser tx

vestingTxWithParams :: ParamCheckDateValidatorType -> Model.UserSpend -> LedgerApiV2.Value -> Model.Tx
vestingTxWithParams paramCheckDateScript usp val =
  mconcat
    [ Model.userSpend usp
    , Model.payToScript paramCheckDateScript (Model.HashDatum ()) val
    ]

claimingTxWithParams :: LedgerApiV2.PubKeyHash -> ParamCheckDateValidatorType -> LedgerApiV2.TxOutRef -> LedgerApiV2.Value -> Model.Tx
claimingTxWithParams pkh paramCheckDateScript contractRef vestVal =
  mconcat
    [ Model.spendScript paramCheckDateScript contractRef () ()
    , Model.payToKey pkh vestVal
    ]
--------------------------------------------------------------------------------
datumCheckAfterDeadlineTest :: LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Slot -> Model.Run ()
datumCheckAfterDeadlineTest deadline curMinT curMaxT wSlot = do
  users <- setupUsers
  let [u1, u2] = users
      datumCheckAfterDeadlineScript = datumCheckAfterDeadlineValidatorScript
  makeDatumCheckDate u1 u2 datumCheckAfterDeadlineScript deadline curMinT curMaxT wSlot

datumCheckBeforeDeadlineTest :: LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Slot -> Model.Run ()
datumCheckBeforeDeadlineTest deadline curMinT curMaxT wSlot = do
  users <- setupUsers
  let [u1, u2] = users
      datumCheckBeforeDeadlineScript = datumCheckBeforeDeadlineValidatorScript 
  makeDatumCheckDate u1 u2 datumCheckBeforeDeadlineScript deadline curMinT curMaxT wSlot

makeDatumCheckDate :: LedgerApiV2.PubKeyHash -> LedgerApiV2.PubKeyHash -> DatumCheckDateValidatorType -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Slot -> Model.Run ()
makeDatumCheckDate sigUser receiver datumCheckDateScript deadline curMinT curMaxT wSlot = do
  let val = Model.adaValue 100
  Model.checkBalance (Model.gives sigUser val datumCheckDateScript) $ do
    sp <- Model.spend sigUser val
    Model.submitTx sigUser $ vestingTxWithDatum datumCheckDateScript deadline sp val
  Model.waitNSlots wSlot
  utxos <- Model.utxoAt datumCheckDateScript
  let [(contractRef, contractOut)] = utxos
  Model.checkBalance (Model.gives datumCheckDateScript (LedgerApiV2.txOutValue contractOut) receiver) $ do
    range <- Model.currentTimeInterval curMinT curMaxT
    tx <- Model.validateIn range $ claimingTxWithDatum receiver datumCheckDateScript deadline contractRef (LedgerApiV2.txOutValue contractOut)
    Model.submitTx sigUser tx

vestingTxWithDatum :: DatumCheckDateValidatorType -> LedgerApiV2.POSIXTime -> Model.UserSpend -> LedgerApiV2.Value -> Model.Tx
vestingTxWithDatum datumCheckDateScript deadline usp val =
  mconcat
    [ Model.userSpend usp
    , Model.payToScript datumCheckDateScript (Model.InlineDatum deadline) val
    ]

claimingTxWithDatum :: LedgerApiV2.PubKeyHash -> DatumCheckDateValidatorType -> LedgerApiV2.POSIXTime -> LedgerApiV2.TxOutRef -> LedgerApiV2.Value -> Model.Tx
claimingTxWithDatum pkh datumCheckDateScript deadline contractRef vestVal =
  mconcat
    [ Model.spendScript datumCheckDateScript contractRef () deadline
    , Model.payToKey pkh vestVal
    ]

