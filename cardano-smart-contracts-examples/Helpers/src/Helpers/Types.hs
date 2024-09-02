{-# LANGUAGE AllowAmbiguousTypes #-}

--------------------------------------------------------------------------------
{- HLINT ignore "Use camelCase"               -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------
module Helpers.Types where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Ledger.Value         as LedgerValue (AssetClass)
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Schema

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

type CS = LedgerApiV2.CurrencySymbol
type TN = LedgerApiV2.TokenName

type NFT = LedgerValue.AssetClass
type WalletPaymentPKH = LedgerApiV2.PubKeyHash
type StakeCredentialPubKeyHash = LedgerApiV2.PubKeyHash

--------------------------------------------------------------------------------2

instance Schema.ToSchema LedgerApiV2.Validator where
    toSchema = Schema.FormSchemaUnit

-- TODO: para cuando vuelva a usar plutus-1.1.0, tengo que desactivar esto
-- instance Schema.ToSchema  LedgerAddress.Address where
--   toSchema = Schema.FormSchemaUnit

instance Schema.ToSchema LedgerApiV2.MintingPolicy where
    toSchema = Schema.FormSchemaUnit

-- instance Schema.ToSchema LedgerApiV2.CurrencySymbol where
--     toSchema = Schema.FormSchemaUnit

-- instance Schema.ToSchema LedgerApiV2.TokenName where
--     toSchema = Schema.FormSchemaUnit

--------------------------------------------------------------------------------2
