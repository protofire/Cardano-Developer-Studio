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

-- Data type for redeemer
data RedeemerNFT = Mint | Burn 
PlutusTx.unstableMakeIsData ''RedeemerNFT

{-# INLINEABLE mkRedeemerNftPolicy #-}
mkRedeemerNftPolicy ::  LedgerApiV2.TxOutRef -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkRedeemerNftPolicy oref redRaw ctxRaw =
    if case redeemer of
        Burn -> traceIfFalse "Wrong amount burned" checkBurnAmount
        Mint -> traceIfFalse "UTxO not consumed"   hasInputUTxO &&
                traceIfFalse "Wrong amount minted" checkMintedAmount
    then () else error ()
    where
        ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        redeemer = LedgerApiV2.unsafeFromBuiltinData @RedeemerNFT redRaw
        info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------------------------------------------------------------
        currencySymbol = LedgerContextsV2.ownCurrencySymbol ctx
        ------------------------------------------------------------------------
        hasInputUTxO = any (\i -> LedgerApiV2.txInInfoOutRef i == oref) $ LedgerApiV2.txInfoInputs info
        checkMintedAmount = allOnes (LedgerValue.flattenValue (LedgerApiV2.txInfoMint info))
        checkBurnAmount = allOnesNegatives (LedgerValue.flattenValue (LedgerApiV2.txInfoMint info))

        allOnes lst = all (\(cs', _, amt) -> cs' == currencySymbol && amt == 1) lst
        allOnesNegatives lst = all (\(cs', _, amt) -> cs' == currencySymbol && amt == (-1)) lst

--------------------------------------------------------------------------------

{-# INLINEABLE redeemerNftPolicy #-}
redeemerNftPolicy :: LedgerApiV2.TxOutRef -> LedgerApiV2.MintingPolicy
redeemerNftPolicy outRef = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ plutonomyPolicy outRef

{-# INLINEABLE plutonomyPolicy #-}
plutonomyPolicy :: LedgerApiV2.TxOutRef -> Plutonomy.MintingPolicy
plutonomyPolicy outRef =
    Plutonomy.mkMintingPolicyScript $
      $$(PlutusTx.compile [|| mkRedeemerNftPolicy ||])
      `PlutusTx.applyCode` PlutusTx.liftCode outRef
--------------------------------------------------------------------------------
