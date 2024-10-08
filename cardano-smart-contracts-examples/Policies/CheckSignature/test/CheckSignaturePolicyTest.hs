
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
import PlutusTx.Prelude ((.), ($), (==), Semigroup ((<>)), Maybe (Just, Nothing))
import Prelude (IO, mapM, mconcat)

import qualified Plutus.Model as Model
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Test.Tasty                        as Tasty
import qualified Test.Tasty.HUnit                  as Tasty
import qualified PlutusTx

import qualified Helpers.OffChain                  as OffChainHelpers
import qualified Helpers.OffChainEval              as OffChainEval

import ParamCheckSignaturePolicy (paramCheckSignaturePolicy)

---------------------------------------------------------------------------------------------------
------------------------------------------ TESTING ------------------------------------------------
-- | Main function that runs the tests using Test.Tasty framework.
main :: IO ()
main = Tasty.defaultMain $ do
    Tasty.testGroup
        "Testing checkSignature policy"
        [ 
          Tasty.testGroup
            "Testing size and resources"
            [
              Tasty.testCase
                "Test Valid Mint; TxValidSize < 16Kb; Mem < 14Mb; Cpu < 10_000M"   $
                  let
                      walletPkh = LedgerApiV2.PubKeyHash "ed485b083eb5816c10c35a9d091d8af4cfdceef40c96578cae2b2266a8d976c9"
                      txDate = 6000
                      policy = paramCheckSignaturePolicy walletPkh
                      ctx = mintingTxContext policy walletPkh txDate

                      getValidator :: LedgerApiV2.Address -> Maybe LedgerApiV2.Validator
                      getValidator _ = Nothing

                      getMintingPolicy :: LedgerApiV2.CurrencySymbol -> Maybe LedgerApiV2.MintingPolicy
                      getMintingPolicy _ = Just policy

                      (eval_log, eval_err, eval_size) = OffChainEval.testContext getValidator getMintingPolicy ctx
                  in do
                      eval_log `OffChainEval.assertContainsAnyOf` []
                      OffChainEval.assertBudgetAndSize eval_err eval_size OffChainEval.maxMemory OffChainEval.maxCPU OffChainEval.maxTxSize
            ]
          ,
          Tasty.testGroup
            "Test using parameters"
            [ 
              good "User 1 generate token policy and user 1 mint and burn" paramCheckSignatureTest
              , bad "User 1 generate token policy and user 2 try mint" paramCheckSignatureTestFail 
            ]
        ]
  where
    -- | Marks a test as expected to fail, useful for testing failure scenarios.
    bad msg = good msg . Model.mustFail
    -- | Marks a test as expected to succeed, useful for testing success scenarios.
    good = Model.testNoErrors (Model.adaValue 10_000_000_000) Model.defaultBabbage

-- | Type alias for the ParamCheckSignature policy.
type ParamCheckSignaturePolicyType = Model.TypedPolicy ()

-- | Creates a TypedPolicy script with the provided public key hash as the signature key.
paramCheckSignaturePolicyScript :: LedgerApiV2.PubKeyHash -> ParamCheckSignaturePolicyType
paramCheckSignaturePolicyScript signatureKey = Model.TypedPolicy $ Model.toV2 
                                        $ paramCheckSignaturePolicy signatureKey

-- | Sets up two users with an initial balance of 1000 Lovelace each.
setupUsers :: Model.Run [LedgerApiV2.PubKeyHash]
setupUsers = replicateM 2 $ Model.newUser $ Model.ada (Model.Lovelace 1000)

--------------------------------------------------------------------------------
-- | Test that verifies minting and burning of tokens when the correct signature is used.
paramCheckSignatureTest :: Model.Run ()
paramCheckSignatureTest = do
  users <- setupUsers
  let [u1, _] = users
      paramCheckScript = paramCheckSignaturePolicyScript u1
      cSymbol = Model.scriptCurrencySymbol paramCheckScript
      mintValue = LedgerApiV2.singleton cSymbol "Token" 1
      burnValue = LedgerApiV2.singleton cSymbol "Token" (-1)

  -- Mint a token using user 1's signature.
  Model.submitTx u1 $ mintTx u1 paramCheckScript mintValue
  -- Spend the minted token.
  sp <- Model.spend u1 mintValue
  -- Burn the token using user 1's signature.
  Model.submitTx u1 $ burnTx sp paramCheckScript burnValue
  
  -- Check if the final balance of user 1 is 1000 Lovelace.
  vals <- mapM Model.valueAt users
  let [v1, _] = vals
  unless (v1 == Model.adaValue 1000) $
    Model.logError "Final balances are incorrect"

-- | Test that fails when an unauthorized user tries to mint tokens.
paramCheckSignatureTestFail :: Model.Run ()
paramCheckSignatureTestFail = do
  users <- setupUsers
  let [u1, u2] = users
      paramCheckScriptFail = paramCheckSignaturePolicyScript u1
      cSymbol = Model.scriptCurrencySymbol paramCheckScriptFail
      mintValue = LedgerApiV2.singleton cSymbol "Token" 1

  -- User 2 tries to mint a token using user 1's policy, which should fail.
  Model.submitTx u2 $ mintTx u2 paramCheckScriptFail mintValue

  -- Check the final balances of the users.
  vals <- mapM Model.valueAt users
  let [v1, _] = vals
  unless (v1 == (Model.adaValue 1000 <> mintValue)) $
    Model.logError "The token could'nt be minted"

-- | Creates a transaction to burn tokens, using the provided policy script.
burnTx :: Model.UserSpend -> ParamCheckSignaturePolicyType -> LedgerApiV2.Value -> Model.Tx
burnTx usp paramCheckPolicyScript val = 
  mconcat
    [ Model.userSpend usp
    , Model.mintValue paramCheckPolicyScript () val
    ]

-- | Creates a transaction to mint tokens, using the provided policy script.
mintTx :: LedgerApiV2.PubKeyHash -> ParamCheckSignaturePolicyType -> LedgerApiV2.Value -> Model.Tx
mintTx pkh paramCheckPolicyScript val =
  mconcat
    [ 
      Model.mintValue paramCheckPolicyScript () val
    , Model.payToKey pkh val
    ]

-----------------------------------------------------------------------------------------

mintingTxContext :: LedgerApiV2.MintingPolicy -> LedgerApiV2.PubKeyHash -> LedgerApiV2.POSIXTime -> LedgerApiV2.ScriptContext
mintingTxContext policy pkh txDate =
  let
    cSymbol = OffChainHelpers.getCurSymbolOfPolicy policy
    in
      OffChainEval.mkBaseValidMintingPolicyContext [] [] cSymbol
        OffChainEval.|> OffChainEval.setMintAndRedeemers [(LedgerApiV2.singleton cSymbol "Token" 1, LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData ())]
        OffChainEval.|> OffChainEval.addSignatory  pkh
        OffChainEval.|> OffChainEval.setValidRange (OffChainEval.createValidRange txDate)

-----------------------------------------------------------------------------------------