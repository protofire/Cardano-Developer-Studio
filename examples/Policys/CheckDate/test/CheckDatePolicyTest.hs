{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NumericUnderscores   #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE NoImplicitPrelude    #-}

module Main where

import Control.Monad (replicateM)
import PlutusTx.Prelude ((.), ($)) 
import Prelude (IO, mconcat)

import qualified Plutus.Model as Model
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Test.Tasty as Tasty

import ParamCheckBeforeDeadlinePolicy (paramCheckBeforeDeadlinePolicy)
import ParamCheckAfterDeadlinePolicy (paramCheckAfterDeadlinePolicy)

---------------------------------------------------------------------------------------------------
------------------------------------------ TESTING ------------------------------------------------

-- | Main function to run the tests.
main :: IO ()
main = Tasty.defaultMain $ do
  Tasty.testGroup
    "Testing time checks"
    [ Tasty.testGroup
        "Test Claim Before the Deadline"
        [ bad   "Deadline: 6000; TxValidRange (6900, 7100)" $ paramCheckBeforeDeadlineTest 6000 (-100) 100 7000
        , good  "Deadline: 6000; TxValidRange (4900, 5100)" $ paramCheckBeforeDeadlineTest 6000 (-100) 100 5000        ],
    Tasty.testGroup
        "Test Claim After the Deadline"
        [ good  "Deadline: 6000; TxValidRange (6900, 7100)" $ paramCheckAfterDeadlineTest 6000 (-100) 100 7000
        , bad   "Deadline: 6000; TxValidRange (4900, 5100)" $ paramCheckAfterDeadlineTest 6000 (-100) 100 5000        ]
    ]
  where
    bad msg = good msg . Model.mustFail
    good = Model.testNoErrors (Model.adaValue 10_000_000_000) Model.defaultBabbage

-- | Create and return a list of users, each with some ADA.
-- This is used to simulate multiple users interacting with the policy.
setupUsers :: Model.Run [LedgerApiV2.PubKeyHash]
setupUsers = replicateM 2 $ Model.newUser $ Model.ada (Model.Lovelace 1000)

type ParamCheckDatePolicyType = Model.TypedPolicy ()

-- | Generate the policy script for after deadline checks.
-- Takes a POSIX time as the deadline and returns a typed policy script.
paramCheckAfterDeadlinePolicyScript :: LedgerApiV2.POSIXTime -> ParamCheckDatePolicyType
paramCheckAfterDeadlinePolicyScript deadline = Model.TypedPolicy $ Model.toV2 
                                        $ paramCheckAfterDeadlinePolicy deadline

-- | Generate the policy script for before deadline checks.
-- Takes a POSIX time as the deadline and returns a typed policy script.
paramCheckBeforeDeadlinePolicyScript :: LedgerApiV2.POSIXTime -> ParamCheckDatePolicyType
paramCheckBeforeDeadlinePolicyScript deadline = Model.TypedPolicy $ Model.toV2 
                                        $ paramCheckBeforeDeadlinePolicy deadline

-- | Test case for minting after the deadline.
-- Validates the transaction based on the provided deadline and time range.
paramCheckAfterDeadlineTest :: LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Model.Run ()
paramCheckAfterDeadlineTest deadline curMinT curMaxT wSlot = do
  [u1, _] <- setupUsers
  let paramCheckAfterDeadlineScript = paramCheckAfterDeadlinePolicyScript deadline
  makeParamCheckDate u1 paramCheckAfterDeadlineScript curMinT curMaxT wSlot

-- | Test case for minting before the deadline.
-- Validates the transaction based on the provided deadline and time range.
paramCheckBeforeDeadlineTest :: LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Model.Run ()
paramCheckBeforeDeadlineTest deadline curMinT curMaxT wSlot = do
  [u1, _] <- setupUsers
  let paramCheckBeforeDeadlineScript = paramCheckBeforeDeadlinePolicyScript deadline
  makeParamCheckDate u1 paramCheckBeforeDeadlineScript curMinT curMaxT wSlot

-- | Helper function to create and validate a transaction based on time checks.
-- Takes the user, policy script, current time range, and waiting slot as parameters.
makeParamCheckDate :: LedgerApiV2.PubKeyHash -> ParamCheckDatePolicyType -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Model.Run ()
makeParamCheckDate sigUser paramCheckScript curMinT curMaxT wSlot = do
  let cSymbol = Model.scriptCurrencySymbol paramCheckScript
      mintValue = LedgerApiV2.singleton cSymbol "Token" 1

  -- Wait until the specified slot to simulate time passing.
  Model.waitUntil wSlot

  -- Define the valid time interval for the transaction.
  range <- Model.currentTimeInterval curMinT curMaxT
  tx <- Model.validateIn range $ mintTx sigUser paramCheckScript mintValue

  -- Submit the transaction to the blockchain.
  Model.submitTx sigUser tx

-- | Creates a transaction to burn tokens.
-- Takes a user spend, policy script, and value to burn.
burnTx :: Model.UserSpend -> ParamCheckDatePolicyType -> LedgerApiV2.Value -> Model.Tx
burnTx usp paramCheckPolicyScript val = 
  mconcat
    [ Model.userSpend usp
    , Model.mintValue paramCheckPolicyScript () val
    ]

-- | Creates a transaction to mint tokens.
-- Takes a public key hash, policy script, and value to mint.
mintTx :: LedgerApiV2.PubKeyHash -> ParamCheckDatePolicyType -> LedgerApiV2.Value -> Model.Tx
mintTx pkh paramCheckPolicyScript val =
  mconcat
    [ Model.mintValue paramCheckPolicyScript () val
    , Model.payToKey pkh val
    ]

