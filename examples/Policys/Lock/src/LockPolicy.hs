{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}

module LockPolicy where

import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusTx

import           PlutusTx.Prelude     (($), error)

{-# INLINEABLE mkLockPolicy #-}
mkLockPolicy :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkLockPolicy _ _ = error ()
--------------------------------------------------------------------------------

{-# INLINEABLE lockPolicy #-}
lockPolicy :: LedgerApiV2.MintingPolicy
lockPolicy = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus plutonomyPolicy 

{-# INLINEABLE plutonomyPolicy #-}
plutonomyPolicy :: Plutonomy.MintingPolicy
plutonomyPolicy =
  Plutonomy.mkMintingPolicyScript 
    $$(PlutusTx.compile [|| mkLockPolicy ||])
--------------------------------------------------------------------------------
