{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}

module AllwaysTrueValidator where

import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusTx

import           PlutusTx.Prelude     (($))

-- | This module defines a simple "allwaysTrue" validator in Plutus. The validator
-- does not enforce any constraints, meaning it will always succeed.

-- | The 'mkAllwaysTrueValidator' function is the core logic of the validator. 
-- It takes three 'PlutusTx.BuiltinData' parameters and always succeeds by returning unit '()'.
{-# INLINEABLE mkAllwaysTrueValidator #-}
mkAllwaysTrueValidator :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkAllwaysTrueValidator _ _ _ = ()
--------------------------------------------------------------------------------

-- | 'allwaysTrueValidator' is the optimized version of the Plutus validator script.
-- It uses 'Plutonomy.optimizeUPLC' to optimize the script and convert it to a Plutus V2 validator.
{-# INLINEABLE allwaysTrueValidator #-}
allwaysTrueValidator :: LedgerApiV2.Validator
allwaysTrueValidator = Plutonomy.optimizeUPLC $ Plutonomy.validatorToPlutus plutonomyValidator 

-- | 'plutonomyValidator' is the raw Plutus validator script before optimization.
-- It is created using 'Plutonomy.mkValidatorScript' and compiled from 'mkAllwaysTrueValidator'.
{-# INLINEABLE plutonomyValidator #-}
plutonomyValidator :: Plutonomy.Validator
plutonomyValidator =
    Plutonomy.mkValidatorScript 
        $$(PlutusTx.compile [|| mkAllwaysTrueValidator ||])
--------------------------------------------------------------------------------

