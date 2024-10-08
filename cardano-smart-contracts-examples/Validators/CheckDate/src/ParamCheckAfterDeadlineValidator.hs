{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE OverloadedStrings   #-}
--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module ParamCheckAfterDeadlineValidator where

import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts as LedgerContextsV2
import qualified PlutusTx

import           PlutusTx.Prelude     (($), Bool, traceIfFalse, error)
import qualified Helpers.OnChain as OnChainHelpers

type Parameter = LedgerApiV2.POSIXTime

-- | Validator function that checks if the current time is after a specified deadline
--
-- This function validates that the current blockchain time is after a deadline specified in the parameters.
-- The validator will only succeed if the deadline has been reached.
--
-- Parameters:
--   - `deadline`: The deadline as a `POSIXTime`.
--   - `_`: Unused parameter.
--   - `_`: Unused parameter.
--   - `ctxRaw`: A `BuiltinData` representing the script context.
--
-- Returns:
--   - `()`: If the deadline has been reached.
--   - Calls `error()` if the deadline has not been reached.
{-# INLINEABLE mkParamCheckAfterDeadlineValidator #-}
mkParamCheckAfterDeadlineValidator :: Parameter -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkParamCheckAfterDeadlineValidator deadline _ _ ctxRaw =
    if traceIfFalse "Deadline not reached" deadlineReached
      then ()
      else error()
  where
    info :: LedgerApiV2.TxInfo
    info = LedgerContextsV2.scriptContextTxInfo ctx

    ctx = PlutusTx.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw

    -- Check if the current time is after the deadline
    deadlineReached :: Bool
    deadlineReached = OnChainHelpers.isDateReached deadline info

--------------------------------------------------------------------------------

-- | Creates the validator script for the parameter check after deadline
--
-- This function creates an optimized Plutus validator script for the `mkParamCheckAfterDeadlineValidator` function.
-- The validator script will only succeed if the current time is after the deadline specified in the parameters.
--
-- Parameters:
--   - `deadline`: The deadline as a `POSIXTime`.
--
-- Returns:
--   - A `Validator` that enforces the deadline check based on the parameter.
{-# INLINEABLE paramCheckAfterDeadlineValidator #-}
paramCheckAfterDeadlineValidator :: LedgerApiV2.POSIXTime -> LedgerApiV2.Validator
paramCheckAfterDeadlineValidator deadline =
      Plutonomy.optimizeUPLC $ Plutonomy.validatorToPlutus $ plutonomyValidator deadline

-- | Creates a Plutus validator script using Plutonomy
--
-- This function compiles the `mkParamCheckAfterDeadlineValidator` function into a Plutus validator script,
-- and applies the specified deadline as a parameter.
--
-- Parameters:
--   - `deadline`: The deadline as a `POSIXTime`.
--
-- Returns:
--   - A `Validator` that checks if a transaction is valid after the specified deadline.
{-# INLINEABLE plutonomyValidator #-}
plutonomyValidator :: LedgerApiV2.POSIXTime -> Plutonomy.Validator
plutonomyValidator deadline =
    Plutonomy.mkValidatorScript $
        $$(PlutusTx.compile [|| mkParamCheckAfterDeadlineValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode deadline

