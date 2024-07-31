{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE OverloadedStrings #-}

module ParamCheckSignaturePolicy where

import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusTx

import           PlutusTx.Prelude     (($), Bool, traceIfFalse, error)
import qualified Plutus.V2.Ledger.Contexts as LedgerContextsV2

type Parameter = LedgerApiV2.PubKeyHash

{-# INLINEABLE mkParamCheckSignaturePolicy #-}
mkParamCheckSignaturePolicy :: Parameter -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkParamCheckSignaturePolicy signatureKey _ ctxRaw =
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

{-# INLINEABLE paramCheckSignaturePolicy #-}
paramCheckSignaturePolicy ::  LedgerApiV2.PubKeyHash -> LedgerApiV2.MintingPolicy
paramCheckSignaturePolicy signatureKey =
      Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ plutonomyPolicy signatureKey

{-# INLINEABLE plutonomyPolicy #-}
plutonomyPolicy :: LedgerApiV2.PubKeyHash -> Plutonomy.MintingPolicy
plutonomyPolicy signatureKey =
    Plutonomy.mkMintingPolicyScript $
        $$(PlutusTx.compile [|| mkParamCheckSignaturePolicy ||])
        `PlutusTx.applyCode` PlutusTx.liftCode signatureKey
--------------------------------------------------------------------------------
