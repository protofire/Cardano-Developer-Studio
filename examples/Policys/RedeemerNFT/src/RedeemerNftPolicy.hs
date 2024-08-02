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

import           PlutusTx.Prelude     (($), traceIfFalse, (==), (&&), error, all, any)
import qualified Plutus.V2.Ledger.Contexts as LedgerContextsV2
import qualified Plutus.V1.Ledger.Value as LedgerValue

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
        hasInputUTxO = any (\i -> LedgerApiV2.txInInfoOutRef i == oref) $ LedgerApiV2.txInfoInputs info

        -- Check if minted amount is correct
        checkMintedAmount = allOnes (LedgerValue.flattenValue (LedgerApiV2.txInfoMint info))

        -- Check if burned amount is correct
        checkBurnAmount = allOnesNegatives (LedgerValue.flattenValue (LedgerApiV2.txInfoMint info))

        -- Helper function to check if all amounts are 1
        allOnes lst = all (\(cs', _, amt) -> cs' == currencySymbol && amt == 1) lst

        -- Helper function to check if all amounts are -1
        allOnesNegatives lst = all (\(cs', _, amt) -> cs' == currencySymbol && amt == (-1)) lst

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

