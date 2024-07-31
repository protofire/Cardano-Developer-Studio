{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module RedeemerFtPolicy where

import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusTx

import           PlutusTx.Prelude     (($), Bool (True, False), error)

-- Data type for redeemer
data RedeemerFT = Mint | Burn | BadRedeemer
PlutusTx.unstableMakeIsData ''RedeemerFT

{-# INLINEABLE mkRedeemerFtPolicy #-}
mkRedeemerFtPolicy :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkRedeemerFtPolicy redRaw _ =
  if case redeemer of
      Burn -> True
      Mint -> True
      _    -> False
      then () else error ()
  where
    redeemer = LedgerApiV2.unsafeFromBuiltinData @RedeemerFT redRaw
--------------------------------------------------------------------------------

{-# INLINEABLE redeemerFtPolicy #-}
redeemerFtPolicy :: LedgerApiV2.MintingPolicy
redeemerFtPolicy = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus plutonomyPolicy 

{-# INLINEABLE plutonomyPolicy #-}
plutonomyPolicy :: Plutonomy.MintingPolicy
plutonomyPolicy =
    Plutonomy.mkMintingPolicyScript 
        $$(PlutusTx.compile [|| mkRedeemerFtPolicy ||])
--------------------------------------------------------------------------------
