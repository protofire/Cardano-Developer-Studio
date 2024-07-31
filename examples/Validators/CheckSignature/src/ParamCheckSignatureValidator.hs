{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE OverloadedStrings #-}

module ParamCheckSignatureValidator where

import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusTx

import           PlutusTx.Prelude     (($), Bool, traceIfFalse, error)
import qualified Plutus.V2.Ledger.Contexts as LedgerContextsV2

type Parameter = LedgerApiV2.PubKeyHash

{-# INLINEABLE mkParamCheckSignatureValidator #-}
mkParamCheckSignatureValidator :: Parameter -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkParamCheckSignatureValidator signatureKey _ _ ctxRaw =
 if traceIfFalse "Beneficiary's signature missing" signedByBeneficiary
      then ()
      else error()
  where
    info :: LedgerApiV2.TxInfo
    info = LedgerContextsV2.scriptContextTxInfo ctx

    ctx = PlutusTx.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw

    signedByBeneficiary :: Bool
    signedByBeneficiary = LedgerContextsV2.txSignedBy info signatureKey
--------------------------------------------------------------------------------

{-# INLINEABLE paramCheckSignatureValidator #-}
paramCheckSignatureValidator ::  LedgerApiV2.PubKeyHash -> LedgerApiV2.Validator
paramCheckSignatureValidator signatureKey =
      Plutonomy.optimizeUPLC $ Plutonomy.validatorToPlutus $ plutonomyValidator signatureKey

{-# INLINEABLE plutonomyValidator #-}
plutonomyValidator :: LedgerApiV2.PubKeyHash -> Plutonomy.Validator
plutonomyValidator signatureKey =
    Plutonomy.mkValidatorScript $
        $$(PlutusTx.compile [|| mkParamCheckSignatureValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode signatureKey
--------------------------------------------------------------------------------
