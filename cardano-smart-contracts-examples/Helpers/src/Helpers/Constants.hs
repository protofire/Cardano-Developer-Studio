{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

--------------------------------------------------------------------------------
{- HLINT ignore "Use camelCase"               -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------

module Helpers.Constants where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Cardano.Node.Emulator.Params as CardanoNodeEmulatorParams
import qualified Ledger
import qualified Plutus.V2.Ledger.Api         as LedgerApiV2

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

-- TODO: Usar plutus-1.1.0
networkId :: Ledger.NetworkId
networkId = CardanoNodeEmulatorParams.testnet

--------------------------------------------------------------------------------2
