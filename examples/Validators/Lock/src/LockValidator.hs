
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}

module LockValidator where

import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusTx
import           PlutusTx.Prelude     (($), error)

-- | The 'mkLockValidator' function is a simple validator that always fails.
-- This is done using the 'error' function, which causes the validation to fail.
-- The function takes three parameters of type 'PlutusTx.BuiltinData',
-- but it does not use any of them.
{-# INLINEABLE mkLockValidator #-}
mkLockValidator :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkLockValidator _ _ _ = error ()

--------------------------------------------------------------------------------

-- | 'lockValidator' is the compiled Plutus V2 validator.
-- It uses the 'Plutonomy.optimizeUPLC' function to optimize the on-chain code,
-- and the 'Plutonomy.validatorToPlutus' function to convert it into a Plutus V2 validator.
{-# INLINEABLE lockValidator #-}
lockValidator :: LedgerApiV2.Validator
lockValidator = Plutonomy.optimizeUPLC $ Plutonomy.validatorToPlutus plutonomyValidator 

-- | 'plutonomyValidator' is the intermediary representation of the validator script.
-- It is created using the 'Plutonomy.mkValidatorScript' function, which compiles the Haskell code
-- of 'mkLockValidator' into Plutus Core.
{-# INLINEABLE plutonomyValidator #-}
plutonomyValidator :: Plutonomy.Validator
plutonomyValidator =
    Plutonomy.mkValidatorScript 
        $$(PlutusTx.compile [|| mkLockValidator ||])

--------------------------------------------------------------------------------

