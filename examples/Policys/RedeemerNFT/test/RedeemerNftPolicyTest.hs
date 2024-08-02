
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

import RedeemerNftPolicy (redeemerNftPolicy, RedeemerNFT(..))
---------------------------------------------------------------------------------------------------
------------------------------------------ TESTING ------------------------------------------------
main :: IO ()
main = Tasty.defaultMain $ do
    Tasty.testGroup
        "Testing redeemerNft policy"
        [ Tasty.testGroup
            "Test the Policy of a NFT"
            [ 
              good "Minting and burning NFT with respectly redeemer" redeemerNftTest,
              bad "Try mint the nfts twice" redeemerNftMintingTwice
            ]
        ]
  where
    bad msg = good msg . Model.mustFail
    good = Model.testNoErrors (Model.adaValue 10_000_000_000) Model.defaultBabbage

type RedeemerNftPolicyType = Model.TypedPolicy RedeemerNFT

-- | Creates a typed policy script using the redeemer NFT policy
redeemerNftPolicyScript :: LedgerApiV2.TxOutRef -> RedeemerNftPolicyType
redeemerNftPolicyScript outRef = Model.TypedPolicy $ Model.toV2 $ redeemerNftPolicy outRef

-- | Sets up two users with initial ADA
setupUsers :: Model.Run [LedgerApiV2.PubKeyHash]
setupUsers = replicateM 2 $ Model.newUser $ Model.ada (Model.Lovelace 1000)

-- | Test the correct minting and burning of NFTs
redeemerNftTest :: Model.Run ()
redeemerNftTest = do
  users <- setupUsers
  let [u1, _] = users
  [(ref, out)] <- Model.utxoAt u1
  let cSymbol = Model.scriptCurrencySymbol $ redeemerNftPolicyScript ref
      mintValue = LedgerApiV2.singleton cSymbol "Token" 1
      burnValue = LedgerApiV2.singleton cSymbol "Token" (-1)
  
  -- Mint the NFT
  Model.submitTx u1 $ mintTx ref out mintValue u1 
  
  -- Spend the NFT and burn it
  sp <- Model.spend u1 mintValue
  Model.submitTx u1 $ burnTx ref sp burnValue
  
  -- Check final balances
  vals <- mapM Model.valueAt users
  let [v1, _] = vals
  unless (v1 == Model.adaValue 1000) $
    Model.logError "Final balances are incorrect"

-- | Transaction for burning an NFT
burnTx :: LedgerApiV2.TxOutRef -> Model.UserSpend -> LedgerApiV2.Value -> Model.Tx
burnTx ref usp val = 
  mconcat
    [ Model.userSpend usp
    , Model.mintValue (redeemerNftPolicyScript ref) Burn val
    ]

-- | Transaction for minting an NFT
mintTx :: LedgerApiV2.TxOutRef -> LedgerApiV2.TxOut -> LedgerApiV2.Value -> LedgerApiV2.PubKeyHash -> Model.Tx
mintTx ref out val pkh =
    mconcat
        [ Model.mintValue (redeemerNftPolicyScript ref) Mint val
        , Model.payToKey pkh $ val <> LedgerApiV2.txOutValue out
        , Model.spendPubKey ref
        ]

-- | Test attempting to mint NFTs twice with the same UTxO
redeemerNftMintingTwice :: Model.Run ()
redeemerNftMintingTwice = do
  users <- setupUsers
  let [u1, _] = users
  [(ref, out)] <- Model.utxoAt u1
  let cSymbol = Model.scriptCurrencySymbol $ redeemerNftPolicyScript ref
      mintValue = LedgerApiV2.singleton cSymbol "Token" 1
  
  -- Attempt to mint the NFT twice
  Model.submitTx u1 $ mintTwiceTx ref out mintValue u1 
  
  -- Check final balances
  vals <- mapM Model.valueAt users
  let [v1, _] = vals
  unless (v1 == (Model.adaValue 1000 <> mintValue <> mintValue)) $
    Model.logError "Final balances are incorrect"

-- | Transaction for minting an NFT twice
mintTwiceTx :: LedgerApiV2.TxOutRef -> LedgerApiV2.TxOut -> LedgerApiV2.Value -> LedgerApiV2.PubKeyHash -> Model.Tx
mintTwiceTx ref out val pkh =
    mconcat
        [ Model.mintValue (redeemerNftPolicyScript ref) Mint val
        , Model.mintValue (redeemerNftPolicyScript ref) Mint val
        , Model.payToKey pkh $ val <> val <> LedgerApiV2.txOutValue out
        , Model.spendPubKey ref
        ]

