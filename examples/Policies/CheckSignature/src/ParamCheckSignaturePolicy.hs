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
module ParamCheckSignaturePolicy where

import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts as LedgerContextsV2
import qualified PlutusTx

import           PlutusTx.Prelude     (($), Bool, traceIfFalse, error)

-- | 'Parameter' is a type alias for 'LedgerApiV2.PubKeyHash', which represents the public key hash required to sign the transaction.
type Parameter = LedgerApiV2.PubKeyHash

-- | 'mkParamCheckSignaturePolicy' is the policy function that checks if the transaction is signed by the specified public key.
-- It takes a 'Parameter' representing the public key hash that must sign the transaction,
-- and two 'PlutusTx.BuiltinData' arguments (which are unused in this policy function).
-- If the required signature is missing, it raises an error.
{-# INLINEABLE mkParamCheckSignaturePolicy #-}
mkParamCheckSignaturePolicy :: Parameter -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkParamCheckSignaturePolicy signatureKey _ ctxRaw =
 if traceIfFalse "Beneficiary's signature missing" signedByBeneficiary
      then ()
      else error()
  where
    -- Extracts the transaction information from the script context.
    info :: LedgerApiV2.TxInfo
    info = LedgerContextsV2.scriptContextTxInfo ctx

    -- Converts the 'BuiltinData' to 'ScriptContext'.
    ctx = PlutusTx.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw

    -- Checks if the transaction is signed by the specified public key.
    signedByBeneficiary :: Bool
    signedByBeneficiary = LedgerContextsV2.txSignedBy info signatureKey

--------------------------------------------------------------------------------

-- | 'paramCheckSignaturePolicy' creates a minting policy that requires the specified public key hash to sign the transaction.
-- It converts the policy function 'plutonomyPolicy' to a Plutus minting policy script.
{-# INLINEABLE paramCheckSignaturePolicy #-}
paramCheckSignaturePolicy :: LedgerApiV2.PubKeyHash -> LedgerApiV2.MintingPolicy
paramCheckSignaturePolicy signatureKey =
      Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ plutonomyPolicy signatureKey

-- | 'plutonomyPolicy' is the function that constructs the minting policy script by applying the parameter to the compiled policy.
-- The parameter 'signatureKey' is used to ensure that transactions are signed by the specified public key.
{-# INLINEABLE plutonomyPolicy #-}
plutonomyPolicy :: LedgerApiV2.PubKeyHash -> Plutonomy.MintingPolicy
plutonomyPolicy signatureKey =
    Plutonomy.mkMintingPolicyScript $
        $$(PlutusTx.compile [|| mkParamCheckSignaturePolicy ||])
        `PlutusTx.applyCode` PlutusTx.liftCode signatureKey

--------------------------------------------------------------------------------

{-# INLINEABLE mkWrappedPolicy #-}
mkWrappedPolicy :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkWrappedPolicy signatureKeyData = mkParamCheckSignaturePolicy signatureKey
    where
        signatureKey = PlutusTx.unsafeFromBuiltinData signatureKeyData :: LedgerApiV2.PubKeyHash

paramCheckSignaturePolicyCode :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
paramCheckSignaturePolicyCode = Plutonomy.optimizeUPLC $$(PlutusTx.compile [||mkWrappedPolicy||])
