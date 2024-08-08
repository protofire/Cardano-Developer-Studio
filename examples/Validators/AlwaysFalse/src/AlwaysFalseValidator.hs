
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module AlwaysFalseValidator where

import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusTx
import           PlutusTx.Prelude     (($), error)

-- | The 'mkAlwaysFalseValidator' function is a simple validator that always fails.
-- This is done using the 'error' function, which causes the validation to fail.
-- The function takes three parameters of type 'PlutusTx.BuiltinData',
-- but it does not use any of them.
{-# INLINEABLE mkAlwaysFalseValidator #-}
mkAlwaysFalseValidator :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkAlwaysFalseValidator _ _ _ = error ()

--------------------------------------------------------------------------------

-- | 'alwaysFalseValidator' is the compiled Plutus V2 validator.
-- It uses the 'Plutonomy.optimizeUPLC' function to optimize the on-chain code,
-- and the 'Plutonomy.validatorToPlutus' function to convert it into a Plutus V2 validator.
{-# INLINEABLE alwaysFalseValidator #-}
alwaysFalseValidator :: LedgerApiV2.Validator
alwaysFalseValidator = Plutonomy.optimizeUPLC $ Plutonomy.validatorToPlutus plutonomyValidator 

-- | 'plutonomyValidator' is the intermediary representation of the validator script.
-- It is created using the 'Plutonomy.mkValidatorScript' function, which compiles the Haskell code
-- of 'mkAlwaysFalseValidator' into Plutus Core.
{-# INLINEABLE plutonomyValidator #-}
plutonomyValidator :: Plutonomy.Validator
plutonomyValidator =
    Plutonomy.mkValidatorScript 
        $$(PlutusTx.compile [|| mkAlwaysFalseValidator ||])

--------------------------------------------------------------------------------

