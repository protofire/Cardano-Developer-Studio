{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE OverloadedStrings   #-}

module ParamCheckAfterDeadlinePolicy where

import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts as LedgerContextsV2
import qualified Plutus.V1.Ledger.Interval as LedgerIntervalV1
import qualified PlutusTx

import           PlutusTx.Prelude     (($), Bool, traceIfFalse, error)

-- | Type alias for the deadline parameter.
type Parameter = LedgerApiV2.POSIXTime

-- | The minting policy function that checks if the current time is after the deadline.
--   This function will be used to enforce that tokens can only be minted after the specified deadline.
{-# INLINEABLE mkParamCheckAfterDeadlinePolicy #-}
mkParamCheckAfterDeadlinePolicy :: Parameter -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkParamCheckAfterDeadlinePolicy deadline _ ctxRaw =
    if traceIfFalse "Deadline not reached" deadlineReached
      then ()
      else error()
  where
    -- | Extracts the transaction info from the script context.
    info :: LedgerApiV2.TxInfo
    info = LedgerContextsV2.scriptContextTxInfo ctx

    -- | Parses the script context from raw built-in data.
    ctx = PlutusTx.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw

    -- | Checks if the current time is after the deadline.
    deadlineReached :: Bool
    deadlineReached = LedgerIntervalV1.contains (LedgerIntervalV1.from deadline) $ LedgerContextsV2.txInfoValidRange info

--------------------------------------------------------------------------------

-- | Creates a minting policy script that checks if the current time is after the deadline.
--   The policy is optimized and compiled into Plutus UPLC code.
{-# INLINEABLE paramCheckAfterDeadlinePolicy #-}
paramCheckAfterDeadlinePolicy :: LedgerApiV2.POSIXTime -> LedgerApiV2.MintingPolicy
paramCheckAfterDeadlinePolicy deadline =
      Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ plutonomyPolicy deadline

-- | Defines the Plutus script for the minting policy, applying the deadline parameter.
{-# INLINEABLE plutonomyPolicy #-}
plutonomyPolicy :: LedgerApiV2.POSIXTime -> Plutonomy.MintingPolicy
plutonomyPolicy deadline =
    Plutonomy.mkMintingPolicyScript $
        $$(PlutusTx.compile [|| mkParamCheckAfterDeadlinePolicy ||])
        `PlutusTx.applyCode` PlutusTx.liftCode deadline

