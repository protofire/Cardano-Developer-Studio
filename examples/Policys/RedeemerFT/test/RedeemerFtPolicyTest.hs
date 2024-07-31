{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Monad (replicateM, unless)
import PlutusTx.Prelude ((.), ($), (==), (<>))
import Prelude (IO, mapM, mconcat)

import qualified Plutus.Model as Model
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Test.Tasty as Tasty

import RedeemerFtPolicy (redeemerFtPolicy, RedeemerFT(..))
---------------------------------------------------------------------------------------------------
------------------------------------------ TESTING ------------------------------------------------
main :: IO ()
main = Tasty.defaultMain $ do
    Tasty.testGroup
        "Testing redeemerFt policy"
        [ Tasty.testGroup
            "Must success"
            [ 
              good "Minting and burning with respectly redeemer" redeemerFtTest,
              bad "Try minting with wrong redeemer" redeemerFtFailTest
            ]
        ]
  where
    bad msg = good msg . Model.mustFail
    good = Model.testNoErrors (Model.adaValue 10_000_000_000) Model.defaultBabbage


type RedeemerFtPolicyType = Model.TypedPolicy RedeemerFT

redeemerFtPolicyScript :: RedeemerFtPolicyType
redeemerFtPolicyScript = Model.TypedPolicy $ Model.toV2 redeemerFtPolicy

setupUsers :: Model.Run [LedgerApiV2.PubKeyHash]
setupUsers = replicateM 2 $ Model.newUser $ Model.ada (Model.Lovelace 1000)

redeemerFtTest :: Model.Run ()
redeemerFtTest = do
  users <- setupUsers
  let [u1, _] = users
      cSymbol = Model.scriptCurrencySymbol redeemerFtPolicyScript
      mintValue = LedgerApiV2.singleton cSymbol "Token" 1
      burnValue = LedgerApiV2.singleton cSymbol "Token" (-1)
  
  Model.submitTx u1 $ mintTx u1 mintValue
  sp <- Model.spend u1 mintValue
  Model.submitTx u1 $ burnTx sp burnValue
  
  vals <- mapM Model.valueAt users
  let [v1, _] = vals
  unless (v1 == Model.adaValue 1000) $
    Model.logError "Final balances are incorrect"

burnTx :: Model.UserSpend -> LedgerApiV2.Value -> Model.Tx
burnTx usp val = 
  mconcat
    [ Model.userSpend usp
    , Model.mintValue redeemerFtPolicyScript Burn val
    ]

mintTx :: LedgerApiV2.PubKeyHash -> LedgerApiV2.Value -> Model.Tx
mintTx pkh val =
  mconcat
    [ 
      Model.mintValue redeemerFtPolicyScript Mint val
    , Model.payToKey pkh val
    ]


redeemerFtFailTest :: Model.Run ()
redeemerFtFailTest = do
  users <- setupUsers
  let [u1, _] = users
      cSymbol = Model.scriptCurrencySymbol redeemerFtPolicyScript
      mintValue = LedgerApiV2.singleton cSymbol "Token" 1
  
  Model.submitTx u1 $ mintTxFail u1 mintValue

  vals <- mapM Model.valueAt users
  let [v1, _] = vals

  unless (v1 == (Model.adaValue 1000 <> mintValue)) $
    Model.logError "Final balances are incorrect"

mintTxFail :: LedgerApiV2.PubKeyHash -> LedgerApiV2.Value -> Model.Tx
mintTxFail pkh val =
  mconcat
    [ 
      Model.mintValue redeemerFtPolicyScript BadRedeemer val
    , Model.payToKey pkh val
    ]


