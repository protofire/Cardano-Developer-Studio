{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module ParamCheckSignatureValidator where

import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusTx

import           PlutusTx.Prelude     (($), Bool, traceIfFalse, error)
import qualified Plutus.V2.Ledger.Contexts as LedgerContextsV2

-- | Type alias for a public key hash parameter
type Parameter = LedgerApiV2.PubKeyHash

-- | Validator function to check if a specific public key hash has signed the transaction
-- 
-- This function verifies if the specified `signatureKey` has signed the transaction. 
-- If not, it triggers an error.
--
-- Parameters:
--   - `signatureKey`: The public key hash that is expected to have signed the transaction.
--   - `ctxRaw`: The raw script context data.
--   - `_`: Unused parameters.
--
-- Returns:
--   - `()` on success or triggers an error if the signature is missing.
{-# INLINEABLE mkParamCheckSignatureValidator #-}
mkParamCheckSignatureValidator :: Parameter -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkParamCheckSignatureValidator signatureKey _ _ ctxRaw =
 if traceIfFalse "Beneficiary's signature missing" signedByBeneficiary
      then ()
      else error()
  where
    -- | Extracts transaction information from the script context
    info :: LedgerApiV2.TxInfo
    info = LedgerContextsV2.scriptContextTxInfo ctx

    -- | Converts raw script context data to a script context
    ctx = PlutusTx.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw

    -- | Checks if the transaction is signed by the specified public key hash
    signedByBeneficiary :: Bool
    signedByBeneficiary = LedgerContextsV2.txSignedBy info signatureKey
--------------------------------------------------------------------------------

-- | Wrapper function to create a Plutonomy validator using the parameter check signature validator
--
-- This function creates a Plutonomy validator script that uses `mkParamCheckSignatureValidator` to verify signatures.
--
-- Parameters:
--   - `signatureKey`: The public key hash expected to sign the transaction.
--
-- Returns:
--   - A Plutonomy `Validator` that wraps the compiled validation script.
{-# INLINEABLE paramCheckSignatureValidator #-}
paramCheckSignatureValidator ::  LedgerApiV2.PubKeyHash -> LedgerApiV2.Validator
paramCheckSignatureValidator signatureKey =
      Plutonomy.optimizeUPLC $ Plutonomy.validatorToPlutus $ plutonomyValidator signatureKey

-- | Helper function to compile the parameter check signature validator script
--
-- This function compiles the `mkParamCheckSignatureValidator` into a Plutonomy validator script.
--
-- Parameters:
--   - `signatureKey`: The public key hash that will be included in the validator script.
--
-- Returns:
--   - A Plutonomy validator script compiled with the provided public key hash.
{-# INLINEABLE plutonomyValidator #-}
plutonomyValidator :: LedgerApiV2.PubKeyHash -> Plutonomy.Validator
plutonomyValidator signatureKey =
    Plutonomy.mkValidatorScript $
        $$(PlutusTx.compile [|| mkParamCheckSignatureValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode signatureKey
--------------------------------------------------------------------------------

