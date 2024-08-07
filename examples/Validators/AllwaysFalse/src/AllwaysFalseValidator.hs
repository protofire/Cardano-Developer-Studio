
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}

module AllwaysFalseValidator where

import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusTx
import           PlutusTx.Prelude     (($), error)

-- | The 'mkAllwaysFalseValidator' function is a simple validator that always fails.
-- This is done using the 'error' function, which causes the validation to fail.
-- The function takes three parameters of type 'PlutusTx.BuiltinData',
-- but it does not use any of them.
{-# INLINEABLE mkAllwaysFalseValidator #-}
mkAllwaysFalseValidator :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkAllwaysFalseValidator _ _ _ = error ()

--------------------------------------------------------------------------------

-- | 'allwaysFalseValidator' is the compiled Plutus V2 validator.
-- It uses the 'Plutonomy.optimizeUPLC' function to optimize the on-chain code,
-- and the 'Plutonomy.validatorToPlutus' function to convert it into a Plutus V2 validator.
{-# INLINEABLE allwaysFalseValidator #-}
allwaysFalseValidator :: LedgerApiV2.Validator
allwaysFalseValidator = Plutonomy.optimizeUPLC $ Plutonomy.validatorToPlutus plutonomyValidator 

-- | 'plutonomyValidator' is the intermediary representation of the validator script.
-- It is created using the 'Plutonomy.mkValidatorScript' function, which compiles the Haskell code
-- of 'mkAllwaysFalseValidator' into Plutus Core.
{-# INLINEABLE plutonomyValidator #-}
plutonomyValidator :: Plutonomy.Validator
plutonomyValidator =
    Plutonomy.mkValidatorScript 
        $$(PlutusTx.compile [|| mkAllwaysFalseValidator ||])

--------------------------------------------------------------------------------

