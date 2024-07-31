{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Monad (replicateM, unless)
import PlutusTx.Prelude (Bool, (.), ($), (==), Semigroup ((<>)))
import Prelude (IO, mapM, mconcat)

import qualified Plutus.Model as Model
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Test.Tasty as Tasty

import ParamCheckSignaturePolicy (paramCheckSignaturePolicy)
---------------------------------------------------------------------------------------------------
------------------------------------------ TESTING ------------------------------------------------
type IsGood = Bool

main :: IO ()
main = Tasty.defaultMain $ do
    Tasty.testGroup
        "Testing checkSignature policy"
        [ Tasty.testGroup
            "Test using parameters"
            [ 
              good "User 1 generate token policy and user 1 mint and burn" paramCheckSignatureTest
              , bad "User 1 generate token policy and user 2 try mint" paramCheckSignatureTestFail 
            ]
        ]
  where
    bad msg = good msg . Model.mustFail
    good = Model.testNoErrors (Model.adaValue 10_000_000_000) Model.defaultBabbage


type ParamCheckSignaturePolicyType = Model.TypedPolicy ()

paramCheckSignaturePolicyScript :: LedgerApiV2.PubKeyHash -> ParamCheckSignaturePolicyType
paramCheckSignaturePolicyScript signatureKey = Model.TypedPolicy $ Model.toV2 
                                        $ paramCheckSignaturePolicy signatureKey

setupUsers :: Model.Run [LedgerApiV2.PubKeyHash]
setupUsers = replicateM 2 $ Model.newUser $ Model.ada (Model.Lovelace 1000)

--------------------------------------------------------------------------------
paramCheckSignatureTest :: Model.Run ()
paramCheckSignatureTest = do
  users <- setupUsers
  let [u1, _] = users
      paramCheckScript = paramCheckSignaturePolicyScript u1
      cSymbol = Model.scriptCurrencySymbol paramCheckScript
      mintValue = LedgerApiV2.singleton cSymbol "Token" 1
      burnValue = LedgerApiV2.singleton cSymbol "Token" (-1)

  Model.submitTx u1 $ mintTx u1 paramCheckScript mintValue
  sp <- Model.spend u1 mintValue
  Model.submitTx u1 $ burnTx sp paramCheckScript burnValue
  
  vals <- mapM Model.valueAt users
  let [v1, _] = vals
  unless (v1 == Model.adaValue 1000) $
    Model.logError "Final balances are incorrect"

paramCheckSignatureTestFail :: Model.Run ()
paramCheckSignatureTestFail = do
  users <- setupUsers
  let [u1, u2] = users
      paramCheckScriptFail = paramCheckSignaturePolicyScript u1
      cSymbol = Model.scriptCurrencySymbol paramCheckScriptFail
      mintValue = LedgerApiV2.singleton cSymbol "Token" 1

  Model.submitTx u2 $ mintTx u2 paramCheckScriptFail mintValue

  vals <- mapM Model.valueAt users
  let [v1, _] = vals
  unless (v1 == (Model.adaValue 1000 <> mintValue)) $
    Model.logError "The token could'nt be minted"

burnTx :: Model.UserSpend -> ParamCheckSignaturePolicyType -> LedgerApiV2.Value -> Model.Tx
burnTx usp paramCheckPolicyScript val = 
  mconcat
    [ Model.userSpend usp
    , Model.mintValue paramCheckPolicyScript () val
    ]

mintTx :: LedgerApiV2.PubKeyHash -> ParamCheckSignaturePolicyType -> LedgerApiV2.Value -> Model.Tx
mintTx pkh paramCheckPolicyScript val =
  mconcat
    [ 
      Model.mintValue paramCheckPolicyScript () val
    , Model.payToKey pkh val
    ]


