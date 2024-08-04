{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}

module LockPolicy where

import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusTx

import           PlutusTx.Prelude     (($), error)

-- | The `mkLockPolicy` function defines the lock policy logic. 
-- For the purpose of this example, it currently always raises an error.
-- 
-- The policy takes two `BuiltinData` arguments and returns `()`. 
-- In a real implementation, the logic would include conditions that
-- need to be satisfied for minting tokens.
{-# INLINEABLE mkLockPolicy #-}
mkLockPolicy :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkLockPolicy _ _ = error ()

--------------------------------------------------------------------------------

-- | Create the minting policy using the `mkLockPolicy` function.
-- This policy is used to define the rules for minting tokens. 
-- In this implementation, the policy is non-functional and will
-- always fail because `mkLockPolicy` raises an error.
{-# INLINEABLE lockPolicy #-}
lockPolicy :: LedgerApiV2.MintingPolicy
lockPolicy = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus plutonomyPolicy 

--------------------------------------------------------------------------------

-- | Define the minting policy script by compiling `mkLockPolicy`.
-- The script produced by this function will always fail due to
-- the error in `mkLockPolicy`.
{-# INLINEABLE plutonomyPolicy #-}
plutonomyPolicy :: Plutonomy.MintingPolicy
plutonomyPolicy =
  Plutonomy.mkMintingPolicyScript 
    $$(PlutusTx.compile [|| mkLockPolicy ||])

