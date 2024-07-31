{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import PlutusTx.Prelude (Bool, (.), ($)) 
import Prelude (IO, mconcat)

import qualified Plutus.Model as Model
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Test.Tasty as Tasty

import ParamCheckBeforeDeadlinePolicy (paramCheckBeforeDeadlinePolicy)
import ParamCheckAfterDeadlinePolicy (paramCheckAfterDeadlinePolicy)
---------------------------------------------------------------------------------------------------
------------------------------------------ TESTING ------------------------------------------------
type IsGood = Bool

main :: IO ()
main = Tasty.defaultMain $ do
  Tasty.testGroup
    "Testing time checks"
    [ Tasty.testGroup
        "Test Claim Before the deadline using datums"
        [ bad   "Deadline: 6000; TxValidRange (6900, 7100)" $ paramCheckBeforeDeadlineTest 6000 (-100) 100 7000
        , good  "Deadline: 6000; TxValidRange (4900, 5100)" $ paramCheckBeforeDeadlineTest 6000 (-100) 100  5000
        ],
    Tasty.testGroup
        "Test claim after the deadline using datums"
        [ good  "Deadline: 6000; TxValidRange (6900, 7100)" $ paramCheckAfterDeadlineTest 6000 (-100) 100  7000
        , bad   "Deadline: 6000; TxValidRange (4900, 5100)" $ paramCheckAfterDeadlineTest 6000 (-100) 100  5000
        ]
    ]
  where
    bad msg = good msg . Model.mustFail
    good = Model.testNoErrors (Model.adaValue 10_000_000_000) Model.defaultBabbage

setupUsers :: Model.Run LedgerApiV2.PubKeyHash
setupUsers = Model.newUser $ Model.ada (Model.Lovelace 1000)

type ParamCheckDatePolicyType = Model.TypedPolicy () 

paramCheckAfterDeadlinePolicyScript :: LedgerApiV2.POSIXTime -> ParamCheckDatePolicyType
paramCheckAfterDeadlinePolicyScript deadline = Model.TypedPolicy $ Model.toV2 
                                        $ paramCheckAfterDeadlinePolicy deadline
paramCheckBeforeDeadlinePolicyScript :: LedgerApiV2.POSIXTime -> ParamCheckDatePolicyType
paramCheckBeforeDeadlinePolicyScript deadline = Model.TypedPolicy $ Model.toV2 
                                        $ paramCheckBeforeDeadlinePolicy deadline

--------------------------------------------------------------------------------
paramCheckAfterDeadlineTest :: LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Model.Run ()
paramCheckAfterDeadlineTest deadline curMinT curMaxT wSlot = do
  users <- setupUsers
  let u1 = users
      paramCheckAfterDeadlineScript = paramCheckAfterDeadlinePolicyScript deadline
  makeParamCheckDate u1 paramCheckAfterDeadlineScript curMinT curMaxT wSlot

paramCheckBeforeDeadlineTest :: LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Model.Run ()
paramCheckBeforeDeadlineTest deadline curMinT curMaxT wSlot = do
  users <- setupUsers
  let u1 = users
      paramCheckBeforeDeadlineScript = paramCheckBeforeDeadlinePolicyScript deadline
  makeParamCheckDate u1 paramCheckBeforeDeadlineScript curMinT curMaxT wSlot


makeParamCheckDate :: LedgerApiV2.PubKeyHash -> ParamCheckDatePolicyType -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Model.Run ()
makeParamCheckDate sigUser paramCheckScript curMinT curMaxT wSlot = do
  let cSymbol = Model.scriptCurrencySymbol paramCheckScript
      mintValue = LedgerApiV2.singleton cSymbol "Token" 1
 
  Model.waitUntil wSlot

  range <- Model.currentTimeInterval curMinT curMaxT
  tx <- Model.validateIn range $ mintTx sigUser paramCheckScript mintValue

  Model.submitTx sigUser tx

burnTx :: Model.UserSpend -> ParamCheckDatePolicyType -> LedgerApiV2.Value -> Model.Tx
burnTx usp paramCheckPolicyScript val = 
  mconcat
    [ Model.userSpend usp
    , Model.mintValue paramCheckPolicyScript () val
    ]

mintTx :: LedgerApiV2.PubKeyHash -> ParamCheckDatePolicyType -> LedgerApiV2.Value -> Model.Tx
mintTx pkh paramCheckPolicyScript val =
  mconcat
    [ 
      Model.mintValue paramCheckPolicyScript () val
    , Model.payToKey pkh val
    ]
--------------------------------------------------------------------------------
