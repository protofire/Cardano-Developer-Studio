{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Monad (replicateM, unless)
import PlutusTx.Prelude (Semigroup ((<>)), (.), ($), (==))
import Prelude (IO, mapM, mconcat)

import qualified Plutus.Model as Model
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Test.Tasty as Tasty

import LockPolicy (lockPolicy)

---------------------------------------------------------------------------------------------------
------------------------------------------ TESTING ------------------------------------------------
-- | Main function that runs the test suite using Tasty.
-- It groups tests into categories and runs them.
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
    -- | Mark the test as a failure case; expects the test to fail.
    bad msg = good msg . Model.mustFail
    
    -- | Mark the test as a success case; expects the test to pass without errors.
    good = Model.testNoErrors (Model.adaValue 10_000_000_000) Model.defaultBabbage

-- | Type alias for the lock policy with no specific parameters.
type LockPolicyType = Model.TypedPolicy ()

-- | Define the lock policy script as a typed policy.
lockPolicyScript :: LockPolicyType
lockPolicyScript = Model.TypedPolicy $ Model.toV2 lockPolicy

-- | Set up users with an initial balance of 1000 Lovelace.
-- This function creates two users for testing.
setupUsers :: Model.Run [LedgerApiV2.PubKeyHash]
setupUsers = replicateM 2 $ Model.newUser $ Model.ada (Model.Lovelace 1000)

-- | Test function to check if the token minting fails as expected.
-- In this test, we try to mint a token using the lock policy,
-- which is expected to fail due to the policy's logic.
lockTest :: Model.Run ()
lockTest = do
  users <- setupUsers
  let [u1, _] = users
      cSymbol = Model.scriptCurrencySymbol lockPolicyScript
      mintValue = LedgerApiV2.singleton cSymbol "Token" 1

  -- Attempt to mint a token and expect failure.
  Model.submitTx u1 $ mintTx u1 mintValue

  -- Check the final balances.
  vals <- mapM Model.valueAt users
  let [v1, _] = vals
  unless (v1 == (Model.adaValue 1000 <> mintValue)) $
    Model.logError "The token couldn't be minted"

-- | Create a transaction to mint a token.
-- This function specifies the policy, value, and recipient.
mintTx :: LedgerApiV2.PubKeyHash -> LedgerApiV2.Value -> Model.Tx
mintTx pkh val =
  mconcat
    [ 
      Model.mintValue lockPolicyScript () val
    , Model.payToKey pkh val
    ]

