{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}

module FreePolicy where

import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusTx

import           PlutusTx.Prelude     (($))

---------------------------------------------------------------------------------------------------
------------------------------------------ FREE POLICY ------------------------------------------------

-- | 'mkFreePolicy' defines the policy logic for the free minting policy.
-- This policy allows unrestricted minting and burning of tokens.
-- 
-- Arguments:
--   - The first argument is the redeemer data, which is unused in this policy.
--   - The second argument is the context data, which is also unused.
-- 
-- The policy always succeeds as it does not enforce any conditions.
{-# INLINEABLE mkFreePolicy #-}
mkFreePolicy :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkFreePolicy _ _ = ()

--------------------------------------------------------------------------------

-- | 'freePolicy' provides the minting policy script optimized for Plutonomy.
-- This is the main entry point to retrieve the policy in a format suitable
-- for use in transactions.
{-# INLINEABLE freePolicy #-}
freePolicy :: LedgerApiV2.MintingPolicy
freePolicy = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus plutonomyPolicy 

--------------------------------------------------------------------------------

-- | 'plutonomyPolicy' defines the minting policy script as a Plutonomy minting policy.
-- It compiles the Haskell code for 'mkFreePolicy' into Plutus Core.
{-# INLINEABLE plutonomyPolicy #-}
plutonomyPolicy :: Plutonomy.MintingPolicy
plutonomyPolicy =
    Plutonomy.mkMintingPolicyScript 
        $$(PlutusTx.compile [|| mkFreePolicy ||])

--------------------------------------------------------------------------------

