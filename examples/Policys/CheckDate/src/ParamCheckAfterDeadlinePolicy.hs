{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE OverloadedStrings #-}

module ParamCheckAfterDeadlinePolicy where

import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts as LedgerContextsV2
import qualified Plutus.V1.Ledger.Interval as LedgerIntervalV1
import qualified PlutusTx

import           PlutusTx.Prelude     (($), Bool, traceIfFalse, error)

type Parameter = LedgerApiV2.POSIXTime

{-# INLINEABLE mkParamCheckAfterDeadlinePolicy #-}
mkParamCheckAfterDeadlinePolicy :: Parameter -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkParamCheckAfterDeadlinePolicy deadline _ ctxRaw =
    if traceIfFalse "Deadline not reached" deadlineReached
      then ()
      else error()
  where
    info :: LedgerApiV2.TxInfo
    info = LedgerContextsV2.scriptContextTxInfo ctx

    ctx = PlutusTx.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw

    -- signedByBeneficiary :: Bool
    -- signedByBeneficiary = LedgerContextsV2.txSignedBy info signatureKey

    deadlineReached :: Bool
    deadlineReached = LedgerIntervalV1.contains (LedgerIntervalV1.from deadline) $ LedgerContextsV2.txInfoValidRange info

--------------------------------------------------------------------------------

{-# INLINEABLE paramCheckAfterDeadlinePolicy #-}
paramCheckAfterDeadlinePolicy ::  LedgerApiV2.POSIXTime -> LedgerApiV2.MintingPolicy
paramCheckAfterDeadlinePolicy deadline =
      Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ plutonomyPolicy deadline

{-# INLINEABLE plutonomyPolicy #-}
plutonomyPolicy :: LedgerApiV2.POSIXTime -> Plutonomy.MintingPolicy
plutonomyPolicy deadline =
    Plutonomy.mkMintingPolicyScript $
        $$(PlutusTx.compile [|| mkParamCheckAfterDeadlinePolicy ||])
        `PlutusTx.applyCode` PlutusTx.liftCode deadline
--------------------------------------------------------------------------------
