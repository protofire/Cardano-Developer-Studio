{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE OverloadedStrings #-}

module DatumCheckBeforeDeadlineValidator where

import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts as LedgerContextsV2
import qualified Plutus.V1.Ledger.Interval as LedgerIntervalV1
import qualified PlutusTx

import           PlutusTx.Prelude     (($), Bool, traceIfFalse, error)

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

    -- signedByBeneficiary :: Bool
    -- signedByBeneficiary = LedgerContextsV2.txSignedBy info signatureKey

    deadlineNotReached :: Bool
    deadlineNotReached = LedgerIntervalV1.contains (LedgerIntervalV1.to deadline) $ LedgerContextsV2.txInfoValidRange info

--------------------------------------------------------------------------------

{-# INLINEABLE datumCheckBeforeDeadlineValidator #-}
datumCheckBeforeDeadlineValidator :: LedgerApiV2.Validator
datumCheckBeforeDeadlineValidator = Plutonomy.optimizeUPLC $ Plutonomy.validatorToPlutus plutonomyValidator 

{-# INLINEABLE plutonomyValidator #-}
plutonomyValidator :: Plutonomy.Validator
plutonomyValidator =
    Plutonomy.mkValidatorScript 
        $$(PlutusTx.compile [|| mkDatumCheckBeforeDeadlineValidator ||])
--------------------------------------------------------------------------------
