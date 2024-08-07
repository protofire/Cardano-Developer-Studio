{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE OverloadedStrings   #-}

--------------------------------------------------------------------------------
{- HLINT ignore "Use camelCase"               -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------
module ParamCheckBeforeDeadlinePolicy where

import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts as LedgerContextsV2
import qualified PlutusTx

import           PlutusTx.Prelude     (($), Bool, traceIfFalse, error)
import qualified Helpers.OnChain as OnChainHelpers

-- | Type alias for the deadline parameter.
type Parameter = LedgerApiV2.POSIXTime

-- | The minting policy function that checks if the current time is before the deadline.
--   This function will be used to enforce that tokens can only be minted before the specified deadline.
{-# INLINEABLE mkParamCheckBeforeDeadlinePolicy #-}
mkParamCheckBeforeDeadlinePolicy :: Parameter -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkParamCheckBeforeDeadlinePolicy deadline _ ctxRaw =
    if traceIfFalse "Deadline reached" deadlineNotReached
      then ()
      else error()
  where
    -- | Extracts the transaction info from the script context.
    info :: LedgerApiV2.TxInfo
    info = LedgerContextsV2.scriptContextTxInfo ctx

    -- | Parses the script context from raw built-in data.
    ctx = PlutusTx.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw

    -- | Checks if the current time is before the deadline.
    deadlineNotReached :: Bool
    deadlineNotReached = OnChainHelpers.isDateNotReached deadline info
--------------------------------------------------------------------------------

-- | Creates a minting policy script that checks if the current time is before the deadline.
--   The policy is optimized and compiled into Plutus UPLC code.
{-# INLINEABLE paramCheckBeforeDeadlinePolicy #-}
paramCheckBeforeDeadlinePolicy :: LedgerApiV2.POSIXTime -> LedgerApiV2.MintingPolicy
paramCheckBeforeDeadlinePolicy deadline =
      Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ plutonomyPolicy deadline

-- | Defines the Plutus script for the minting policy, applying the deadline parameter.
{-# INLINEABLE plutonomyPolicy #-}
plutonomyPolicy :: LedgerApiV2.POSIXTime -> Plutonomy.MintingPolicy
plutonomyPolicy deadline =
    Plutonomy.mkMintingPolicyScript $
        $$(PlutusTx.compile [|| mkParamCheckBeforeDeadlinePolicy ||])
        `PlutusTx.applyCode` PlutusTx.liftCode deadline

