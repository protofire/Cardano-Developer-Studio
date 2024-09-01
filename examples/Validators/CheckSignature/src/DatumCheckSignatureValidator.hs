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

module DatumCheckSignatureValidator where

import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts as LedgerContextsV2
import qualified PlutusTx

import           PlutusTx.Prelude     (($), Bool, traceIfFalse, error, (&&))

import qualified Helpers.OnChain as OnChainHelpers

-- | This module defines a validator that checks if a transaction is signed
-- by a specific beneficiary whose public key hash is provided in the datum.

-- | The 'mkDatumCheckSignatureValidator' function is the core logic of the validator.
-- It takes three 'PlutusTx.BuiltinData' parameters: datum, redeemer (unused), and context.
-- The function checks if the transaction is signed by the public key hash specified in the datum.
{-# INLINEABLE mkDatumCheckSignatureValidator #-}
mkDatumCheckSignatureValidator :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkDatumCheckSignatureValidator datumRaw _ ctxRaw =
    if traceIfFalse "Beneficiary's signature missing" signedByBeneficiary
      then ()
      else error()
  where
    -- | Extracts transaction information from the context.
    info :: LedgerApiV2.TxInfo
    info = LedgerContextsV2.scriptContextTxInfo ctx

    -- | Converts the raw context data into a 'ScriptContext'.
    ctx = PlutusTx.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw

    -- | Converts the raw datum data into a 'PubKeyHash'.
    signatureKey = PlutusTx.unsafeFromBuiltinData @LedgerApiV2.PubKeyHash datumRaw

    -- | Checks if the transaction is signed by the public key hash specified in the datum.
    signedByBeneficiary :: Bool
    signedByBeneficiary = LedgerContextsV2.txSignedBy info signatureKey
--------------------------------------------------------------------------------

-- | 'datumCheckSignatureValidator' is the optimized version of the Plutus validator script.
-- It uses 'Plutonomy.optimizeUPLC' to optimize the script and convert it to a Plutus V2 validator.
{-# INLINEABLE datumCheckSignatureValidator #-}
datumCheckSignatureValidator :: LedgerApiV2.Validator
datumCheckSignatureValidator = Plutonomy.optimizeUPLC $ Plutonomy.validatorToPlutus plutonomyValidator 

-- | 'plutonomyValidator' is the raw Plutus validator script before optimization.
-- It is created using 'Plutonomy.mkValidatorScript' and compiled from 'mkDatumCheckSignatureValidator'.
{-# INLINEABLE plutonomyValidator #-}
plutonomyValidator :: Plutonomy.Validator
plutonomyValidator =
    Plutonomy.mkValidatorScript 
        $$(PlutusTx.compile [|| mkDatumCheckSignatureValidator ||])
--------------------------------------------------------------------------------

