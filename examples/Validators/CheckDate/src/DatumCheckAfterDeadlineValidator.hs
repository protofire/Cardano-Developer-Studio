{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE OverloadedStrings   #-}

module DatumCheckAfterDeadlineValidator where

import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts as LedgerContextsV2
import qualified PlutusTx

import           PlutusTx.Prelude     (($), Bool, traceIfFalse, error)

import qualified Helpers.OnChain as OnChainHelpers

-- | Validator function that checks if the current time is past a specified deadline
--
-- This function validates that the current blockchain time is after a deadline specified in the datum.
-- The validator will only succeed if the deadline has been reached.
--
-- Parameters:
--   - `datumRaw`: A `BuiltinData` representing the deadline as a `POSIXTime`.
--   - `_`: Unused parameter.
--   - `ctxRaw`: A `BuiltinData` representing the script context.
--
-- Returns:
--   - `()`: If the deadline has been reached.
--   - Calls `error()` if the deadline has not been reached.
{-# INLINEABLE mkDatumCheckAfterDeadlineValidator #-}
mkDatumCheckAfterDeadlineValidator :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkDatumCheckAfterDeadlineValidator datumRaw _ ctxRaw =
    if traceIfFalse "Deadline not reached" deadlineReached
      then ()
      else error()
  where
    info :: LedgerApiV2.TxInfo
    info = LedgerContextsV2.scriptContextTxInfo ctx

    ctx = PlutusTx.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
    deadline = PlutusTx.unsafeFromBuiltinData @LedgerApiV2.POSIXTime datumRaw

    -- Check if the current time is after the deadline
    deadlineReached :: Bool
    deadlineReached = OnChainHelpers.isDateReached deadline info

--------------------------------------------------------------------------------

-- | Creates the validator script for the datum check after deadline
--
-- This function creates an optimized Plutus validator script for the `mkDatumCheckAfterDeadlineValidator` function.
-- The validator script will only succeed if the current time is past the deadline specified in the datum.
--
-- Returns:
--   - A `Validator` that enforces the deadline check.
{-# INLINEABLE datumCheckAfterDeadlineValidator #-}
datumCheckAfterDeadlineValidator :: LedgerApiV2.Validator
datumCheckAfterDeadlineValidator = Plutonomy.optimizeUPLC $ Plutonomy.validatorToPlutus plutonomyValidator 

-- | Creates a Plutus validator script using Plutonomy
--
-- This function compiles the `mkDatumCheckAfterDeadlineValidator` function into a Plutus validator script.
-- The resulting validator script will be used for checking deadlines in transactions.
--
-- Returns:
--   - A `Validator` that checks if a transaction is valid after a specified deadline.
{-# INLINEABLE plutonomyValidator #-}
plutonomyValidator :: Plutonomy.Validator
plutonomyValidator =
    Plutonomy.mkValidatorScript 
        $$(PlutusTx.compile [|| mkDatumCheckAfterDeadlineValidator ||])

