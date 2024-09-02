{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
--------------------------------------------------------------------------------
{- HLINT ignore "Use camelCase"               -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------
module AlwaysTruePolicy where

import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusTx

import           PlutusTx.Prelude     (($))

---------------------------------------------------------------------------------------------------
------------------------------------------ FREE POLICY ------------------------------------------------

-- | 'mkAlwaysTruePolicy' defines the policy logic for the alwaysTrue minting policy.
-- This policy allows unrestricted minting and burning of tokens.
-- 
-- Arguments:
--   - The first argument is the redeemer data, which is unused in this policy.
--   - The second argument is the context data, which is also unused.
-- 
-- The policy always succeeds as it does not enforce any conditions.
{-# INLINEABLE mkAlwaysTruePolicy #-}
mkAlwaysTruePolicy :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkAlwaysTruePolicy _ _ = ()

--------------------------------------------------------------------------------

-- | 'alwaysTruePolicy' provides the minting policy script optimized for Plutonomy.
-- This is the main entry point to retrieve the policy in a format suitable
-- for use in transactions.
{-# INLINEABLE alwaysTruePolicy #-}
alwaysTruePolicy :: LedgerApiV2.MintingPolicy
alwaysTruePolicy = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus plutonomyPolicy 

--------------------------------------------------------------------------------

-- | 'plutonomyPolicy' defines the minting policy script as a Plutonomy minting policy.
-- It compiles the Haskell code for 'mkAlwaysTruePolicy' into Plutus Core.
{-# INLINEABLE plutonomyPolicy #-}
plutonomyPolicy :: Plutonomy.MintingPolicy
plutonomyPolicy =
    Plutonomy.mkMintingPolicyScript 
        $$(PlutusTx.compile [|| mkAlwaysTruePolicy ||])

--------------------------------------------------------------------------------

