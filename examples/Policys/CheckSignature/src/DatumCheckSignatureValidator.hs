{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE OverloadedStrings #-}

module DatumCheckSignatureValidator where

import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts as LedgerContextsV2
import qualified PlutusTx

import           PlutusTx.Prelude     (($), Bool, traceIfFalse, error)

{-# INLINEABLE mkDatumCheckSignatureValidator #-}
mkDatumCheckSignatureValidator :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkDatumCheckSignatureValidator datumRaw _ ctxRaw =
    if traceIfFalse "Beneficiary's signature missing" signedByBeneficiary
      then ()
      else error()
    -- traceIfFalse "deadline not reached" deadlineReached
  where
    info :: LedgerApiV2.TxInfo
    info = LedgerContextsV2.scriptContextTxInfo ctx

    ctx = PlutusTx.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
    signatureKey = PlutusTx.unsafeFromBuiltinData @LedgerApiV2.PubKeyHash datumRaw

    signedByBeneficiary :: Bool
    signedByBeneficiary = LedgerContextsV2.txSignedBy info signatureKey

    -- deadlineReached :: Bool
    -- deadlineReached = contains (from $ deadline params) $ txInfoValidRange info

--------------------------------------------------------------------------------

{-# INLINEABLE datumCheckSignatureValidator #-}
datumCheckSignatureValidator :: LedgerApiV2.Validator
datumCheckSignatureValidator = Plutonomy.optimizeUPLC $ Plutonomy.validatorToPlutus plutonomyValidator 

{-# INLINEABLE plutonomyValidator #-}
plutonomyValidator :: Plutonomy.Validator
plutonomyValidator =
    Plutonomy.mkValidatorScript 
        $$(PlutusTx.compile [|| mkDatumCheckSignatureValidator ||])
--------------------------------------------------------------------------------
