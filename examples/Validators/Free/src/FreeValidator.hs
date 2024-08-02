{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}

module FreeValidator where

import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusTx

import           PlutusTx.Prelude     (($))

-- | This module defines a simple "free" validator in Plutus. The validator
-- does not enforce any constraints, meaning it will always succeed.

-- | The 'mkFreeValidator' function is the core logic of the validator. 
-- It takes three 'PlutusTx.BuiltinData' parameters and always succeeds by returning unit '()'.
{-# INLINEABLE mkFreeValidator #-}
mkFreeValidator :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkFreeValidator _ _ _ = ()
--------------------------------------------------------------------------------

-- | 'freeValidator' is the optimized version of the Plutus validator script.
-- It uses 'Plutonomy.optimizeUPLC' to optimize the script and convert it to a Plutus V2 validator.
{-# INLINEABLE freeValidator #-}
freeValidator :: LedgerApiV2.Validator
freeValidator = Plutonomy.optimizeUPLC $ Plutonomy.validatorToPlutus plutonomyValidator 

-- | 'plutonomyValidator' is the raw Plutus validator script before optimization.
-- It is created using 'Plutonomy.mkValidatorScript' and compiled from 'mkFreeValidator'.
{-# INLINEABLE plutonomyValidator #-}
plutonomyValidator :: Plutonomy.Validator
plutonomyValidator =
    Plutonomy.mkValidatorScript 
        $$(PlutusTx.compile [|| mkFreeValidator ||])
--------------------------------------------------------------------------------

