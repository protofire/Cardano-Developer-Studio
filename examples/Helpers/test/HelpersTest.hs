
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

--------------------------------------------------------------------------------
{- HLINT ignore "Use camelCase"               -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------
module Main where

import Control.Monad (replicateM, unless)
import PlutusTx.Prelude ((&&), ($), (==))
import Prelude (IO, mapM, mconcat)

import qualified Plutus.Model as Model
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Test.Tasty as Tasty


---------------------------------------------------------------------------------------------------
------------------------------------------ TESTING ------------------------------------------------
-- | The main entry point for the testing suite. It sets up and runs tests using Tasty.
main :: IO ()
main = Tasty.defaultMain $ do
    Tasty.testGroup
        "Testing Helpers"
        [ 
        ]
  where
    -- | Helper functions to define tests that must fail or succeed.
    good = Model.testNoErrors (Model.adaValue 10_000_000_000) Model.defaultBabbage
