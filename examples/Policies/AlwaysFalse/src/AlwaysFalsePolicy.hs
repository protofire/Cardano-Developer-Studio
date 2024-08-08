{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}

--------------------------------------------------------------------------------
{- HLINT ignore "Use camelCase"               -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------
module AlwaysFalsePolicy where

import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusTx

import           PlutusTx.Prelude     (($), error)

-- | The `mkAlwaysFalsePolicy` function defines the alwaysFalse policy logic. 
-- For the purpose of this example, it currently always raises an error.
-- 
-- The policy takes two `BuiltinData` arguments and returns `()`. 
-- In a real implementation, the logic would include conditions that
-- need to be satisfied for minting tokens.
{-# INLINEABLE mkAlwaysFalsePolicy #-}
mkAlwaysFalsePolicy :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkAlwaysFalsePolicy _ _ = error ()

--------------------------------------------------------------------------------

-- | Create the minting policy using the `mkAlwaysFalsePolicy` function.
-- This policy is used to define the rules for minting tokens. 
-- In this implementation, the policy is non-functional and will
-- always fail because `mkAlwaysFalsePolicy` raises an error.
{-# INLINEABLE alwaysFalsePolicy #-}
alwaysFalsePolicy :: LedgerApiV2.MintingPolicy
alwaysFalsePolicy = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus plutonomyPolicy 

--------------------------------------------------------------------------------

-- | Define the minting policy script by compiling `mkAlwaysFalsePolicy`.
-- The script produced by this function will always fail due to
-- the error in `mkAlwaysFalsePolicy`.
{-# INLINEABLE plutonomyPolicy #-}
plutonomyPolicy :: Plutonomy.MintingPolicy
plutonomyPolicy =
  Plutonomy.mkMintingPolicyScript 
    $$(PlutusTx.compile [|| mkAlwaysFalsePolicy ||])

