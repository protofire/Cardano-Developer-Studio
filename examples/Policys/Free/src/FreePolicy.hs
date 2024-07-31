{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}

module FreePolicy where

import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusTx

import           PlutusTx.Prelude     (($))

{-# INLINEABLE mkFreePolicy #-}
mkFreePolicy :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkFreePolicy _ _ = ()
--------------------------------------------------------------------------------

{-# INLINEABLE freePolicy #-}
freePolicy :: LedgerApiV2.MintingPolicy
freePolicy = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus plutonomyPolicy 

{-# INLINEABLE plutonomyPolicy #-}
plutonomyPolicy :: Plutonomy.MintingPolicy
plutonomyPolicy =
    Plutonomy.mkMintingPolicyScript 
        $$(PlutusTx.compile [|| mkFreePolicy ||])
--------------------------------------------------------------------------------
