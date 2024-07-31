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

-- | Data type representing possible redeemer actions for the fungible token policy.
-- The redeemer can be:
--   - Mint: to mint new tokens.
--   - Burn: to burn existing tokens.
--   - BadRedeemer: an invalid redeemer that should cause the policy to fail.
data RedeemerFT = Mint | Burn | BadRedeemer
PlutusTx.unstableMakeIsData ''RedeemerFT

-- | The core minting policy function.
-- This function enforces that only valid redeemer actions are allowed.
-- It checks:
--   - If the redeemer is Burn, it must be a valid Burn operation.
--   - If the redeemer is Mint, it must be a valid Mint operation.
--   - Any other redeemer is considered invalid and will cause an error.
{-# INLINEABLE mkRedeemerFtPolicy #-}
mkRedeemerFtPolicy :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkRedeemerFtPolicy redRaw _ =
  if case redeemer of
      Burn -> True
      Mint -> True
      _    -> False
      then () else error ()
  where
    -- Convert the redeemer data from BuiltinData to RedeemerFT.
    redeemer = LedgerApiV2.unsafeFromBuiltinData @RedeemerFT redRaw

--------------------------------------------------------------------------------

-- | Creates the minting policy script for the fungible token.
-- This function optimizes the policy script and converts it into a Plutus minting policy.
{-# INLINEABLE redeemerFtPolicy #-}
redeemerFtPolicy :: LedgerApiV2.MintingPolicy
redeemerFtPolicy = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus plutonomyPolicy 

{-# INLINEABLE plutonomyPolicy #-}
plutonomyPolicy :: Plutonomy.MintingPolicy
plutonomyPolicy =
    -- Creates the minting policy script by compiling the policy function.
    Plutonomy.mkMintingPolicyScript 
        $$(PlutusTx.compile [|| mkRedeemerFtPolicy ||])

