{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}

--------------------------------------------------------------------------------
{- HLINT ignore "Use camelCase"               -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------
module AllwaysFalsePolicy where

import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusTx

import           PlutusTx.Prelude     (($), error)

-- | The `mkAllwaysFalsePolicy` function defines the allwaysFalse policy logic. 
-- For the purpose of this example, it currently always raises an error.
-- 
-- The policy takes two `BuiltinData` arguments and returns `()`. 
-- In a real implementation, the logic would include conditions that
-- need to be satisfied for minting tokens.
{-# INLINEABLE mkAllwaysFalsePolicy #-}
mkAllwaysFalsePolicy :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkAllwaysFalsePolicy _ _ = error ()

--------------------------------------------------------------------------------

-- | Create the minting policy using the `mkAllwaysFalsePolicy` function.
-- This policy is used to define the rules for minting tokens. 
-- In this implementation, the policy is non-functional and will
-- always fail because `mkAllwaysFalsePolicy` raises an error.
{-# INLINEABLE allwaysFalsePolicy #-}
allwaysFalsePolicy :: LedgerApiV2.MintingPolicy
allwaysFalsePolicy = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus plutonomyPolicy 

--------------------------------------------------------------------------------

-- | Define the minting policy script by compiling `mkAllwaysFalsePolicy`.
-- The script produced by this function will always fail due to
-- the error in `mkAllwaysFalsePolicy`.
{-# INLINEABLE plutonomyPolicy #-}
plutonomyPolicy :: Plutonomy.MintingPolicy
plutonomyPolicy =
  Plutonomy.mkMintingPolicyScript 
    $$(PlutusTx.compile [|| mkAllwaysFalsePolicy ||])

