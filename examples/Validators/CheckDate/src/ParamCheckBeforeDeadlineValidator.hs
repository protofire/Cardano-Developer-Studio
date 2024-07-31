{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE OverloadedStrings #-}

module ParamCheckBeforeDeadlineValidator where

import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts as LedgerContextsV2
import qualified Plutus.V1.Ledger.Interval as LedgerIntervalV1
import qualified PlutusTx

import           PlutusTx.Prelude     (($), Bool, traceIfFalse, error)

type Parameter = LedgerApiV2.POSIXTime

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

    -- signedByBeneficiary :: Bool
    -- signedByBeneficiary = LedgerContextsV2.txSignedBy info signatureKey

    deadlineNotReached :: Bool
    deadlineNotReached = LedgerIntervalV1.contains (LedgerIntervalV1.to deadline) $ LedgerContextsV2.txInfoValidRange info

--------------------------------------------------------------------------------

{-# INLINEABLE paramCheckBeforeDeadlineValidator #-}
paramCheckBeforeDeadlineValidator ::  LedgerApiV2.POSIXTime -> LedgerApiV2.Validator
paramCheckBeforeDeadlineValidator deadline =
      Plutonomy.optimizeUPLC $ Plutonomy.validatorToPlutus $ plutonomyValidator deadline

{-# INLINEABLE plutonomyValidator #-}
plutonomyValidator :: LedgerApiV2.POSIXTime -> Plutonomy.Validator
plutonomyValidator deadline =
    Plutonomy.mkValidatorScript $
        $$(PlutusTx.compile [|| mkParamCheckBeforeDeadlineValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode deadline
--------------------------------------------------------------------------------
