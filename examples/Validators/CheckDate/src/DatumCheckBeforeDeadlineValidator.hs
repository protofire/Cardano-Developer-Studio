
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE OverloadedStrings   #-}

module DatumCheckBeforeDeadlineValidator where

import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts as LedgerContextsV2
import qualified Plutus.V1.Ledger.Interval as LedgerIntervalV1
import qualified PlutusTx

import           PlutusTx.Prelude     (($), Bool, traceIfFalse, error)

-- | Validator function that checks if the current time is before a specified deadline
--
-- This function validates that the current blockchain time is before a deadline specified in the datum.
-- The validator will only succeed if the deadline has not yet been reached.
--
-- Parameters:
--   - `datumRaw`: A `BuiltinData` representing the deadline as a `POSIXTime`.
--   - `_`: Unused parameter.
--   - `ctxRaw`: A `BuiltinData` representing the script context.
--
-- Returns:
--   - `()`: If the deadline has not yet been reached.
--   - Calls `error()` if the deadline has been reached.
{-# INLINEABLE mkDatumCheckBeforeDeadlineValidator #-}
mkDatumCheckBeforeDeadlineValidator :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkDatumCheckBeforeDeadlineValidator datumRaw _ ctxRaw =
    if traceIfFalse "Deadline reached" deadlineNotReached
      then ()
      else error()
  where
    info :: LedgerApiV2.TxInfo
    info = LedgerContextsV2.scriptContextTxInfo ctx

    ctx = PlutusTx.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
    deadline = PlutusTx.unsafeFromBuiltinData @LedgerApiV2.POSIXTime datumRaw

    -- Check if the current time is before the deadline
    deadlineNotReached :: Bool
    deadlineNotReached = LedgerIntervalV1.contains (LedgerIntervalV1.to deadline) $ LedgerContextsV2.txInfoValidRange info

--------------------------------------------------------------------------------

-- | Creates the validator script for the datum check before deadline
--
-- This function creates an optimized Plutus validator script for the `mkDatumCheckBeforeDeadlineValidator` function.
-- The validator script will only succeed if the current time is before the deadline specified in the datum.
--
-- Returns:
--   - A `Validator` that enforces the deadline check.
{-# INLINEABLE datumCheckBeforeDeadlineValidator #-}
datumCheckBeforeDeadlineValidator :: LedgerApiV2.Validator
datumCheckBeforeDeadlineValidator = Plutonomy.optimizeUPLC $ Plutonomy.validatorToPlutus plutonomyValidator 

-- | Creates a Plutus validator script using Plutonomy
--
-- This function compiles the `mkDatumCheckBeforeDeadlineValidator` function into a Plutus validator script.
-- The resulting validator script will be used for checking that transactions are valid before a specified deadline.
--
-- Returns:
--   - A `Validator` that checks if a transaction is valid before a specified deadline.
{-# INLINEABLE plutonomyValidator #-}
plutonomyValidator :: Plutonomy.Validator
plutonomyValidator =
    Plutonomy.mkValidatorScript 
        $$(PlutusTx.compile [|| mkDatumCheckBeforeDeadlineValidator ||])

