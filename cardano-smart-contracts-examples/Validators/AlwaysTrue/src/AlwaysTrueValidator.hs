{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE OverloadedStrings    #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module AlwaysTrueValidator where

import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusTx

import           PlutusTx.Prelude     (($))

-- | This module defines a simple "alwaysTrue" validator in Plutus. The validator
-- does not enforce any constraints, meaning it will always succeed.

-- | The 'mkAlwaysTrueValidator' function is the core logic of the validator. 
-- It takes three 'PlutusTx.BuiltinData' parameters and always succeeds by returning unit '()'.
{-# INLINEABLE mkAlwaysTrueValidator #-}
mkAlwaysTrueValidator :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkAlwaysTrueValidator _ _ _ = ()
--------------------------------------------------------------------------------

-- | 'alwaysTrueValidator' is the optimized version of the Plutus validator script.
-- It uses 'Plutonomy.optimizeUPLC' to optimize the script and convert it to a Plutus V2 validator.
{-# INLINEABLE alwaysTrueValidator #-}
alwaysTrueValidator :: LedgerApiV2.Validator
alwaysTrueValidator = Plutonomy.optimizeUPLC $ Plutonomy.validatorToPlutus plutonomyValidator 

-- | 'plutonomyValidator' is the raw Plutus validator script before optimization.
-- It is created using 'Plutonomy.mkValidatorScript' and compiled from 'mkAlwaysTrueValidator'.
{-# INLINEABLE plutonomyValidator #-}
plutonomyValidator :: Plutonomy.Validator
plutonomyValidator =
    Plutonomy.mkValidatorScript 
        $$(PlutusTx.compile [|| mkAlwaysTrueValidator ||])
--------------------------------------------------------------------------------
