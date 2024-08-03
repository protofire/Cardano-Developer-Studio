{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE OverloadedStrings   #-}

module ParamCheckBeforeDeadlineValidator where

import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts as LedgerContextsV2
import qualified PlutusTx

import           PlutusTx.Prelude     (($), Bool, traceIfFalse, error)
import qualified Helpers.OnChain as OnChainHelpers

type Parameter = LedgerApiV2.POSIXTime

-- | Validator function that checks if the current time is before a specified deadline
--
-- This function validates that the current blockchain time is before a deadline specified in the parameters.
-- The validator will only succeed if the deadline has not yet been reached.
--
-- Parameters:
--   - `deadline`: The deadline as a `POSIXTime`.
--   - `_`: Unused parameter.
--   - `_`: Unused parameter.
--   - `ctxRaw`: A `BuiltinData` representing the script context.
--
-- Returns:
--   - `()`: If the deadline has not yet been reached.
--   - Calls `error()` if the deadline has been reached.
{-# INLINEABLE mkParamCheckBeforeDeadlineValidator #-}
mkParamCheckBeforeDeadlineValidator :: Parameter -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkParamCheckBeforeDeadlineValidator deadline _ _ ctxRaw =
    if traceIfFalse "Deadline reached" deadlineNotReached
      then ()
      else error()
  where
    info :: LedgerApiV2.TxInfo
    info = LedgerContextsV2.scriptContextTxInfo ctx

    ctx = PlutusTx.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw

    -- Check if the current time is before the deadline
    deadlineNotReached :: Bool
    deadlineNotReached = OnChainHelpers.isDateNotReached deadline info

--------------------------------------------------------------------------------

-- | Creates the validator script for the parameter check before deadline
--
-- This function creates an optimized Plutus validator script for the `mkParamCheckBeforeDeadlineValidator` function.
-- The validator script will only succeed if the current time is before the deadline specified in the parameters.
--
-- Parameters:
--   - `deadline`: The deadline as a `POSIXTime`.
--
-- Returns:
--   - A `Validator` that enforces the deadline check based on the parameter.
{-# INLINEABLE paramCheckBeforeDeadlineValidator #-}
paramCheckBeforeDeadlineValidator :: LedgerApiV2.POSIXTime -> LedgerApiV2.Validator
paramCheckBeforeDeadlineValidator deadline =
      Plutonomy.optimizeUPLC $ Plutonomy.validatorToPlutus $ plutonomyValidator deadline

-- | Creates a Plutus validator script using Plutonomy
--
-- This function compiles the `mkParamCheckBeforeDeadlineValidator` function into a Plutus validator script,
-- and applies the specified deadline as a parameter.
--
-- Parameters:
--   - `deadline`: The deadline as a `POSIXTime`.
--
-- Returns:
--   - A `Validator` that checks if a transaction is valid before the specified deadline.
{-# INLINEABLE plutonomyValidator #-}
plutonomyValidator :: LedgerApiV2.POSIXTime -> Plutonomy.Validator
plutonomyValidator deadline =
    Plutonomy.mkValidatorScript $
        $$(PlutusTx.compile [|| mkParamCheckBeforeDeadlineValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode deadline

