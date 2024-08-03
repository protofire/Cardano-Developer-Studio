{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE OverloadedStrings   #-}

module RedeemerNftPolicy where

import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusTx

import           PlutusTx.Prelude     (($), traceIfFalse, (&&), error)
import qualified Plutus.V2.Ledger.Contexts as LedgerContextsV2
import qualified Helpers.OnChain as OnChainHelpers

-- | Data type for redeemer
data RedeemerNFT = Mint | Burn
PlutusTx.unstableMakeIsData ''RedeemerNFT

{-# INLINEABLE mkRedeemerNftPolicy #-}
-- | The minting policy function that determines if a transaction is valid
--
-- This function checks whether a transaction is valid based on the redeemer value (Mint or Burn).
-- It validates if the correct amount is minted or burned and if the correct UTxO is consumed.
mkRedeemerNftPolicy :: LedgerApiV2.TxOutRef -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkRedeemerNftPolicy oref redRaw ctxRaw =
    if case redeemer of
        Burn -> traceIfFalse "Wrong amount burned" checkBurnAmount
        Mint -> traceIfFalse "UTxO not consumed"   hasInputUTxO &&
                traceIfFalse "Wrong amount minted" checkMintedAmount
    then () else error ()
    where
        -- Convert raw data to Plutus types
        ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        redeemer = LedgerApiV2.unsafeFromBuiltinData @RedeemerNFT redRaw
        info = LedgerContextsV2.scriptContextTxInfo ctx

        -- Define currency symbol for the policy
        currencySymbol = LedgerContextsV2.ownCurrencySymbol ctx

        -- Check if the UTxO reference is present in the inputs
        hasInputUTxO = OnChainHelpers.isTxOutAnInput oref info

        -- Check if minted amount is correct
        checkMintedAmount = OnChainHelpers.isNFT_Minting_With_CS currencySymbol info 

        -- Check if burned amount is correct
        checkBurnAmount = OnChainHelpers.isNFT_Burning_With_CS currencySymbol info 

--------------------------------------------------------------------------------

{-# INLINEABLE redeemerNftPolicy #-}
-- | Converts the policy into a minting policy
redeemerNftPolicy :: LedgerApiV2.TxOutRef -> LedgerApiV2.MintingPolicy
redeemerNftPolicy outRef = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ plutonomyPolicy outRef

{-# INLINEABLE plutonomyPolicy #-}
-- | Compiles the minting policy script
plutonomyPolicy :: LedgerApiV2.TxOutRef -> Plutonomy.MintingPolicy
plutonomyPolicy outRef =
    Plutonomy.mkMintingPolicyScript $
      $$(PlutusTx.compile [|| mkRedeemerNftPolicy ||])
      `PlutusTx.applyCode` PlutusTx.liftCode outRef

