{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE OverloadedStrings #-}

module DatumCheckAfterDeadlineValidator where

import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts as LedgerContextsV2
import qualified Plutus.V1.Ledger.Interval as LedgerIntervalV1
import qualified PlutusTx

import           PlutusTx.Prelude     (($), Bool, traceIfFalse, error)

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

    -- signedByBeneficiary :: Bool
    -- signedByBeneficiary = LedgerContextsV2.txSignedBy info signatureKey

    deadlineReached :: Bool
    deadlineReached = LedgerIntervalV1.contains (LedgerIntervalV1.from deadline) $ LedgerContextsV2.txInfoValidRange info

--------------------------------------------------------------------------------

{-# INLINEABLE datumCheckAfterDeadlineValidator #-}
datumCheckAfterDeadlineValidator :: LedgerApiV2.Validator
datumCheckAfterDeadlineValidator = Plutonomy.optimizeUPLC $ Plutonomy.validatorToPlutus plutonomyValidator 

{-# INLINEABLE plutonomyValidator #-}
plutonomyValidator :: Plutonomy.Validator
plutonomyValidator =
    Plutonomy.mkValidatorScript 
        $$(PlutusTx.compile [|| mkDatumCheckAfterDeadlineValidator ||])
--------------------------------------------------------------------------------
