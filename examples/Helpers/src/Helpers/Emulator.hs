{-# LANGUAGE DataKinds #-}

--------------------------------------------------------------------------------
{- HLINT ignore "Use camelCase"               -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------


module Helpers.Emulator where

--------------------------------------------------------------------------------2
-- External Imports
--------------------------------------------------------------------------------2

import qualified Control.Lens                as ControlLens
import qualified Control.Monad.Freer.Extras  as MonadExtras
import qualified Data.Aeson                  as DataAeson
import qualified Data.Default                as DataDefault (def)
import qualified Data.Map                    as DataMap
import qualified Ledger
import qualified Ledger.Ada                  as LedgerAda
import qualified Ledger.Address              as LedgerAddress
import qualified Plutus.Trace.Emulator       as TraceEmulator
import qualified Plutus.Trace.Emulator.Types as TraceEmulatorTypes
import qualified Plutus.V2.Ledger.Api        as LedgerApiV2
import           PlutusTx.Prelude
import qualified Prelude                     as P
import qualified Prettyprinter               (defaultLayoutOptions, layoutPretty, pretty)
import qualified Prettyprinter.Render.String as Prettyprinter (renderString)
import qualified System.IO                   as SystemIO
import qualified Wallet.Emulator.MultiAgent  as WalletEmulatorMultiAgent
import qualified Wallet.Emulator.Types       as WalletEmulatorTypes
import qualified Wallet.Emulator.Wallet      as WalletEmulator

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Helpers.OffChain     as OffChainHelpers

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

getUtxos :: LedgerAddress.Address -> TraceEmulator.EmulatorTrace [(Ledger.TxOutRef, Ledger.TxOut)]
getUtxos addr = do
    state <- TraceEmulator.chainState
    let utxoIndex = Ledger.getIndex $ state ControlLens.^. TraceEmulator.index
        utxos = [(oref, o) | (oref, o) <- DataMap.toList utxoIndex, OffChainHelpers.cardanoAddressToAddress (Ledger.txOutAddress o) == addr]
    return utxos

--------------------------------------------------------------------------------2

emulatorConfig :: TraceEmulator.EmulatorConfig
emulatorConfig = TraceEmulator.EmulatorConfig (Left $ DataMap.fromList [(WalletEmulator.knownWallet w, v) | w <- [1 .. 1]]) DataDefault.def
    where
        v :: LedgerApiV2.Value
        v = LedgerAda.lovelaceValueOf 2000_000_000

--------------------------------------------------------------------------------2

traceConfig :: TraceEmulator.TraceConfig
traceConfig =
    TraceEmulator.TraceConfig
        { TraceEmulator.traceConfigShowEvent = traceConfigShowEvent,
          -- \^ Function to decide how to print the particular events.
          TraceEmulator.traceConfigOutputHandle = SystemIO.stdout, -- withFile "/tmp/trace-log.txt" WriteMode $ \h -> h
          -- \^ Where to print the outputs to. Default: 'System.IO.stdout'
          TraceEmulator.traceConfigMinLogLevel = MonadExtras.Notice
          -- Debug
          -- Info
          -- Notice
          -- Warning
          -- Error
          -- Critical
          -- Alert
          -- Emergency
        }

traceConfigShowEvent :: MonadExtras.LogMessage WalletEmulatorTypes.EmulatorEvent -> Maybe P.String
traceConfigShowEvent (MonadExtras.LogMessage _minLogLevel (WalletEmulatorTypes.EmulatorTimeEvent slot e)) =
    let logMsgMaybe = case e of
            WalletEmulatorMultiAgent.UserThreadEvent (TraceEmulatorTypes.UserLog msg) ->
                Just $ "*** USER LOG: " <> msg
            WalletEmulatorMultiAgent.InstanceEvent (TraceEmulatorTypes.ContractInstanceLog (TraceEmulatorTypes.ContractLog (DataAeson.String msg)) _ _) ->
                Just $ "*** CONTRACT LOG: " <> P.show msg
            WalletEmulatorMultiAgent.InstanceEvent (TraceEmulatorTypes.ContractInstanceLog (TraceEmulatorTypes.StoppedWithError err) _ _) ->
                Just $ "*** CONTRACT STOPPED WITH ERROR: " <> P.show err
            WalletEmulatorMultiAgent.InstanceEvent (TraceEmulatorTypes.ContractInstanceLog TraceEmulatorTypes.NoRequestsHandled _ _) ->
                Nothing
            WalletEmulatorMultiAgent.InstanceEvent (TraceEmulatorTypes.ContractInstanceLog (TraceEmulatorTypes.HandledRequest _) _ _) ->
                Nothing
            WalletEmulatorMultiAgent.InstanceEvent (TraceEmulatorTypes.ContractInstanceLog (TraceEmulatorTypes.CurrentRequests _) _ _) ->
                Nothing
            WalletEmulatorMultiAgent.SchedulerEvent _ ->
                Nothing
            WalletEmulatorMultiAgent.WalletEvent _ _ ->
                Nothing
            ev ->
                Just . Prettyprinter.renderString . Prettyprinter.layoutPretty Prettyprinter.defaultLayoutOptions . Prettyprinter.pretty $ ev
        paddedSlotNo = pad 5 (Ledger.getSlot slot)
    in  fmap (\m -> "Slot " <> paddedSlotNo <> ": " <> m) logMsgMaybe
    where
        pad :: P.Int -> Integer -> P.String
        pad n = (\x -> P.replicate (n P.- P.length x) '0' ++ x) . P.show

---------------------------------------------------
