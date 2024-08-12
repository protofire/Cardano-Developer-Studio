{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module Helpers.OffChainEval where

--------------------------------------------------------------------------------2
-- External Imports
--------------------------------------------------------------------------------2

import qualified Cardano.Api                                     as CardanoApi
import qualified Cardano.Api.Shelley                             as CardanoApiShelley
import qualified Cardano.Binary                                  as CardanoBinary 
import qualified Cardano.Ledger.Alonzo.Scripts                   as AlonzoScripts
import qualified Cardano.Ledger.Alonzo.Tx                        as AlonzoTx
import qualified Cardano.Ledger.Alonzo.TxWitness                 as AlonzoTxWitness
import qualified Cardano.Ledger.Babbage.TxBody                   as BabbageTxBody
import qualified Cardano.Ledger.Babbage.TxInfo                   as BabbageTxInfo
import qualified Cardano.Ledger.Core                             as CardanoLedgerCore
import qualified Cardano.Node.Emulator.TimeSlot                  as CardanoNodeEmulatorTimeSlot
import qualified Cardano.Node.Emulator.Validation                as CardanoNodeEmulatorValidation
import qualified Control.Lens                                    as ControlLens
import qualified Control.Monad                                   as ControlMonad
import qualified Control.Monad                                   as Monad
import qualified Data.Aeson                                      as DataAeson
import qualified Data.ByteString.Lazy                            as LBS
import qualified Data.Default                                    as DataDefault
import qualified Data.Map                                        as DataMap
import qualified Data.Text                                       as DataText
import qualified Data.Typeable                                   as DataTypeable (Typeable)
import qualified Data.Void                                       as DataVoid (Void)
import qualified Debug.Trace                                     as DebugTrace
import qualified GHC.Generics                                    as Generic
import qualified GHC.Natural                                     as GHCNatural
import qualified GHC.Stack                                       as GHC
import qualified Ledger
import qualified Ledger.Ada                                      as LedgerAda
import qualified Ledger.Constraints                              as LedgerConstraints
import qualified Ledger.Tx                                       as LedgerTx
import qualified Ledger.Tx.CardanoAPI                            as LedgerTxCardanoAPI
import qualified Ledger.Tx.CardanoAPI.Internal                   as LedgerTxCardanoAPIInternal
import qualified Ledger.Tx.Constraints                           as LedgerTxConstraints
import qualified Plutus.ChainIndex                               as ChainIndex
import qualified Plutus.Contract                                 as PlutusContract
import qualified Plutus.Contract.CardanoAPI                      as PlutusContractCardanoAPI
import qualified Plutus.Script.Utils.V1.Typed.Scripts.Validators as UtilsTypedScriptsValidatorsV1 (DatumType, RedeemerType)
import qualified Plutus.V1.Ledger.Interval                       as LedgerInterval
import qualified Plutus.V1.Ledger.ProtocolVersions               as LedgerProtocolVersionsV1
import qualified Plutus.V1.Ledger.Scripts                        as LedgerScriptsV1
import qualified Plutus.V2.Ledger.Api                            as LedgerApiV2
import qualified Plutus.V2.Ledger.EvaluationContext              as LedgerEvaluationContextV2
import qualified PlutusCore                                      as PLC                               
import qualified PlutusTx                                        
import qualified PlutusTx.AssocMap                               as TxAssocMap
import           PlutusTx.Prelude                                hiding (unless)
import qualified Prelude                                         as P
import qualified System.Directory                                   as SystemDirectory
import qualified System.FilePath                                 as SystemFilePath
import qualified Test.QuickCheck                                 as QC
import qualified Test.Tasty.HUnit                                as Tasty
import qualified Text.Printf                                     as TextPrintf (printf)
import qualified Text.Read                                       as TextRead (readMaybe)

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Helpers.OffChain                                as OffChainHelpers
import qualified Helpers.OnChain                                 as OnChainHelpers

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

maxMemory :: Integer
maxMemory = 14_000_000

maxCPU :: Integer
maxCPU = 10_000_000_000

maxTxSize :: Integer
maxTxSize = 16_384

--------------------------------------------------------------------------------

someTxId :: LedgerApiV2.TxId
someTxId = LedgerApiV2.TxId "dd"

--------------------------------------------------------------------------------

data ParamsEvalPolicy = ParamsEvalPolicy
    { epHash     :: P.String
    , epRedeemer :: P.String
    , epCtx      :: P.String
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, Generic.Generic, P.Eq, P.Ord, P.Show)

data ParamsEvalValidator = ParamsEvalValidator
    { evHash     :: P.String
    , evDatum    :: P.String
    , evRedeemer :: P.String
    , evCtx      :: P.String
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, Generic.Generic, P.Eq, P.Ord, P.Show)

--------------------------------------------------------------------------------

evaluateScriptValidator :: LedgerApiV2.Validator -> [PlutusTx.Data] -> PlutusTx.Data -> PlutusTx.Data -> PlutusTx.Data -> (LedgerApiV2.LogOutput, P.Either LedgerApiV2.EvaluationError LedgerApiV2.ExBudget, Integer)
evaluateScriptValidator validator params datum redeemer ctx =
    let datas :: [PlutusTx.Data]
        datas = params ++ [datum, redeemer, ctx]
        ----------------------
        !pv = LedgerProtocolVersionsV1.vasilPV
        !scriptUnValidatorV2 = OffChainHelpers.getScriptUnValidator validator
        !scriptShortBsV2 = OffChainHelpers.getScriptShortBs scriptUnValidatorV2
        ----------------------
        exBudget :: LedgerApiV2.ExBudget
        exBudget = LedgerApiV2.ExBudget 10000000000 14000000
        ----------------------
        -- !(logout, e) = LedgerApiV2.evaluateScriptCounting pv LedgerApiV2.Verbose LedgerEvaluationContextV2.evalCtxForTesting scriptShortBsV2 datas
        !(logout, e) = LedgerApiV2.evaluateScriptRestricting pv LedgerApiV2.Verbose LedgerEvaluationContextV2.evalCtxForTesting exBudget scriptShortBsV2 datas
        ----------------------
        !size = LedgerScriptsV1.scriptSize scriptUnValidatorV2
     in (logout, e, size)

-- | Compiles a validator and executes it with the given parameters.
evaluateScriptValidatorEX :: LedgerApiV2.Validator -> LedgerApiV2.Datum -> LedgerApiV2.Redeemer -> LedgerApiV2.ScriptContext -> (LedgerApiV2.LogOutput, P.Either LedgerApiV2.EvaluationError LedgerApiV2.ExBudget, P.Either Integer Integer)
evaluateScriptValidatorEX validator datum redeemer context =
    let scriptUnValidatorV2 = OffChainHelpers.getScriptUnValidator validator
        scriptShortBsV2 = OffChainHelpers.getScriptShortBs scriptUnValidatorV2
        size = LedgerScriptsV1.scriptSize scriptUnValidatorV2
     in case PLC.defaultCostModelParams of
            Just costModelParams ->
                let arguments =
                        [ LedgerApiV2.toData datum
                        , LedgerApiV2.toData redeemer
                        , LedgerApiV2.toData context
                        ]
                    mcontext :: Either LedgerApiV2.CostModelApplyError LedgerApiV2.EvaluationContext
                    mcontext = LedgerApiV2.mkEvaluationContext costModelParams
                    (logout, ev) =
                        case mcontext of
                            Right evalContext ->
                                LedgerApiV2.evaluateScriptCounting
                                    LedgerProtocolVersionsV1.vasilPV
                                    LedgerApiV2.Verbose
                                    evalContext
                                    scriptShortBsV2
                                    arguments
                            Left _ -> (["ERROR GETTING EVALUATION CONTEXT"], Left P.undefined)
                 in case ev of
                        Right _ -> (logout, ev, Right size)
                        Left _ ->
                            case logout of
                                [] -> (["ERROR EVALUATING SCRIPT"], Left P.undefined, Right size)
                                _ -> (logout, Left P.undefined, Right size)
            Nothing -> (["COST MODEL NOT FOUND"], Left P.undefined, Right size)


evaluateScriptValidator_With_StringParams :: P.String -> P.IO (LedgerApiV2.LogOutput, P.Either LedgerApiV2.EvaluationError LedgerApiV2.ExBudget, Integer)
evaluateScriptValidator_With_StringParams paramsJsonStr = do
    let paramsEvalValidator' = DataAeson.decode @ParamsEvalValidator (OffChainHelpers.stringToLazyByteString $ replaceQuotes paramsJsonStr)
    case paramsEvalValidator' of
        Nothing -> do
            P.error "Cant decode paramsEvalValidator"
        Just ParamsEvalValidator{..} -> do
            validator' <- findHashAndReadValidator "export" evHash
            case validator' of
                Nothing -> P.error "Policy not found"
                Just validator -> do
                    P.putStrLn $ "Validator: " ++ P.show validator
                    P.putStrLn $ "DatumStr: " ++ P.show evDatum
                    P.putStrLn $ "RedeemerStr: " ++ P.show evRedeemer
                    let paramsData = [] :: [LedgerApiV2.Data]
                        datumData = OffChainHelpers.getDataFromEncodedJson (OffChainHelpers.stringToLazyByteString evDatum)
                        redeemerData = OffChainHelpers.getDataFromEncodedJson (OffChainHelpers.stringToLazyByteString evRedeemer)
                        ctxData = OffChainHelpers.getDataFromEncodedJson (OffChainHelpers.stringToLazyByteString evCtx)
                    P.putStrLn $ "datumData: " ++ P.show datumData
                    P.putStrLn $ "redeemerData: " ++ P.show redeemerData
                    let (eval_log, eval_err, eval_size) = evaluateScriptValidator validator paramsData datumData redeemerData ctxData
                    return (eval_log, eval_err, eval_size)

--------------------------------------------------------------------------------2

evaluateScriptPolicy :: LedgerApiV2.MintingPolicy -> [PlutusTx.Data] -> PlutusTx.Data -> PlutusTx.Data -> (LedgerApiV2.LogOutput, P.Either LedgerApiV2.EvaluationError LedgerApiV2.ExBudget, Integer)
evaluateScriptPolicy policy params redeemer ctx =
    let datas :: [PlutusTx.Data]
        datas = params ++ [redeemer, ctx]
        ----------------------
        !pv = LedgerProtocolVersionsV1.vasilPV
        !scriptMintingPolicyV2 = OffChainHelpers.getScriptMintingPolicy policy
        !scriptShortBsV2 = OffChainHelpers.getScriptShortBs scriptMintingPolicyV2
        ----------------------
        exBudget :: LedgerApiV2.ExBudget
        exBudget = LedgerApiV2.ExBudget 10000000000 14000000
        ----------------------
        -- !(logout, e) = Ledge rApiV2.evaluateScriptCounting pv LedgerApiV2.Verbose LedgerEvaluationContextV2.evalCtxForTesting scriptShortBsV2 datas
        !(logout, e) = LedgerApiV2.evaluateScriptRestricting pv LedgerApiV2.Verbose LedgerEvaluationContextV2.evalCtxForTesting exBudget scriptShortBsV2 datas
        ----------------------
        !size = LedgerScriptsV1.scriptSize scriptMintingPolicyV2
     in (logout, e, size)

evaluateScriptPolicyEX :: LedgerApiV2.MintingPolicy -> LedgerApiV2.Redeemer -> LedgerApiV2.ScriptContext -> (LedgerApiV2.LogOutput, P.Either LedgerApiV2.EvaluationError LedgerApiV2.ExBudget, P.Either Integer Integer)
evaluateScriptPolicyEX policy redeemer context =
    let 
        scriptMintingPolicyV2 = OffChainHelpers.getScriptMintingPolicy policy
        scriptShortBsV2 = OffChainHelpers.getScriptShortBs scriptMintingPolicyV2
        size = LedgerScriptsV1.scriptSize scriptMintingPolicyV2
     in case PLC.defaultCostModelParams of
            Just costModelParams ->
                let arguments =
                        [ LedgerApiV2.toData redeemer
                        , LedgerApiV2.toData context
                        ]
                    mcontext :: Either LedgerApiV2.CostModelApplyError LedgerApiV2.EvaluationContext
                    mcontext = LedgerApiV2.mkEvaluationContext costModelParams
                    (logout, ev) =
                        case mcontext of
                            Right evalContext ->
                                LedgerApiV2.evaluateScriptCounting
                                    LedgerProtocolVersionsV1.vasilPV
                                    LedgerApiV2.Verbose
                                    evalContext
                                    scriptShortBsV2
                                    arguments
                            Left _ -> (["ERROR GETTING EVALUATION CONTEXT"], Left P.undefined)
                 in case ev of
                        Right _ -> (logout, ev, Right size)
                        Left _ ->
                            case logout of
                                [] -> (["ERROR EVALUATING SCRIPT"], Left P.undefined, Right size)
                                _ -> (logout, Left P.undefined, Right size)
            Nothing -> (["COST MODEL NOT FOUND"], Left P.undefined, Right size)

evaluateScriptPolicy_With_StringParams :: P.String -> P.IO (LedgerApiV2.LogOutput, P.Either LedgerApiV2.EvaluationError LedgerApiV2.ExBudget, Integer)
evaluateScriptPolicy_With_StringParams paramsJsonStr = do
    let paramsEvalPolicy' = DataAeson.decode @ParamsEvalPolicy (OffChainHelpers.stringToLazyByteString $ replaceQuotes paramsJsonStr)
    case paramsEvalPolicy' of
        Nothing -> do
            P.error "Cant decode paramsEvalPolicy"
        Just ParamsEvalPolicy{..} -> do
            policy' <- findHashAndReadMintingPolicy "export" epHash
            case policy' of
                Nothing -> P.error "Policy not found"
                Just policy -> do
                    P.putStrLn $ "Policy: " ++ P.show policy
                    P.putStrLn $ "RedeemerStr: " ++ P.show epRedeemer
                    let paramsData = [] :: [LedgerApiV2.Data]
                        redeemerData = OffChainHelpers.getDataFromEncodedJson (OffChainHelpers.stringToLazyByteString epRedeemer)
                        ctxData = OffChainHelpers.getDataFromEncodedJson (OffChainHelpers.stringToLazyByteString epCtx)
                    P.putStrLn $ "RedeemerData: " ++ P.show redeemerData
                    let (eval_log, eval_err, eval_size) = evaluateScriptPolicy policy paramsData redeemerData ctxData
                    return (eval_log, eval_err, eval_size)

--------------------------------------------------------------------------------2

evalAndSubmitTx ::
    forall w s.
    P.String ->
    [(LedgerApiV2.CurrencySymbol, LedgerApiV2.MintingPolicy)] ->
    [(LedgerApiV2.Address, LedgerApiV2.Validator)] ->
    [LedgerApiV2.BuiltinData -> Maybe P.String] ->
    LedgerConstraints.ScriptLookups DataVoid.Void ->
    LedgerTxConstraints.TxConstraints (UtilsTypedScriptsValidatorsV1.RedeemerType DataVoid.Void) (UtilsTypedScriptsValidatorsV1.DatumType DataVoid.Void) ->
    -- LedgerApiV2.POSIXTime ->
    PlutusContract.Contract w s DataText.Text ()
evalAndSubmitTx nameEndPoint listOfMintingScripts listOfValidators showDatum lookupsTx tx = do
    -- slotZeroTime
    ------------------------
    OffChainHelpers.printTitle (nameEndPoint ++ " : Create Tx")
    ------------------------
    !txUnBalanced <- PlutusContract.mkTxConstraints @DataVoid.Void lookupsTx tx
    !balanceTx <- PlutusContract.balanceTx txUnBalanced
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "txUnBalanced: %s" (P.show txUnBalanced)
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "balanceTx: %s" (P.show balanceTx)
    ------------------------
    let getSomeCardanoApiTx :: LedgerTx.CardanoTx -> PlutusContract.Contract w s DataText.Text LedgerTx.SomeCardanoApiTx
        getSomeCardanoApiTx (Ledger.CardanoApiTx someCardanoApiTx') = return someCardanoApiTx'
        getSomeCardanoApiTx (LedgerTx.EmulatorTx _) = PlutusContract.throwError "validateTx: No CardanoApiTx"
    someCardanoApiTx <- getSomeCardanoApiTx balanceTx
    let getEmulatorEraTx :: Ledger.SomeCardanoApiTx -> PlutusContract.Contract w s DataText.Text (CardanoApiShelley.Tx CardanoApiShelley.BabbageEra)
        getEmulatorEraTx (LedgerTx.SomeTx tx' CardanoApiShelley.BabbageEraInCardanoMode) = return tx'
        getEmulatorEraTx _ = PlutusContract.throwError "validateTx: No BabbageEraInCardanoMode"
    ------------------------
    emulatorEraTx <- getEmulatorEraTx someCardanoApiTx
    ------------------------
    -- !(CardanoApiShelley.ShelleyTx _ (AlonzoTx.ValidatedTx bodyTx AlonzoTxWitness.TxWitness {txrdmrs = AlonzoTxWitness.Redeemers redemersTx} (AlonzoTx.IsValid validTx) auxiliaryData)) = emulatorEraTx
    let !(CardanoApiShelley.ShelleyTx _ (AlonzoTx.ValidatedTx bodyTx AlonzoTxWitness.TxWitness{txrdmrs = AlonzoTxWitness.Redeemers redemersTx} (AlonzoTx.IsValid validTx) _)) = emulatorEraTx
        -- PlutusContract.logInfo @P.String $ TextPrintf.printf "BodyTx: %s" (P.show bodyTx)
        ------------------------
        !(CardanoApiShelley.ShelleyTx _ txVal) = emulatorEraTx
        ------------------------
        getSize :: (DataTypeable.Typeable era, CardanoBinary.ToCBOR (CardanoLedgerCore.TxBody era), CardanoBinary.ToCBOR (CardanoLedgerCore.AuxiliaryData era)) => AlonzoTx.ValidatedTx era -> P.Integer
        getSize !tx' = P.fromIntegral . LBS.length . CardanoBinary.serializeEncoding $ AlonzoTx.toCBORForSizeComputation tx'
        ------------------------
        !sizeTx = getSize txVal
        ------------------------
        !allRedeemers = DataMap.toList redemersTx
        ------------------------
        !( BabbageTxBody.TxBody
                _spendInputs
                _collateralInputs
                _referenceInputs
                _outputs
                _collateralReturn
                _totalCollateral
                _certs
                _wdrls
                _txfee
                _vldt
                _update
                _reqSignerHashes
                _mint
                _scriptIntegrityHash
                _adHash
                _txnetworkid
            ) = bodyTx
        ----------------------
        getTxBodyContent :: LedgerTx.SomeCardanoApiTx -> CardanoApiShelley.TxBodyContent CardanoApiShelley.ViewTx CardanoApiShelley.BabbageEra
        getTxBodyContent (LedgerTx.CardanoApiEmulatorEraTx (CardanoApiShelley.Tx (CardanoApiShelley.TxBody bodyContent) _)) = bodyContent
        ----------------------
        !txBodyContent = getTxBodyContent someCardanoApiTx
        -- PlutusContract.logInfo @P.String $ TextPrintf.printf "TxBodyContent: %s" (P.show txBodyContent)
        ----------------------
        getTxBodyContentInputsReference :: CardanoApi.TxBodyContent ctx era -> [LedgerTx.TxIn]
        getTxBodyContentInputsReference CardanoApi.TxBodyContent{..} =
            let txInsReferenceToPlutusTxIns CardanoApi.TxInsReferenceNone = []
                txInsReferenceToPlutusTxIns (CardanoApi.TxInsReference _ txIns'') =
                    fmap ((`LedgerTx.TxIn` Nothing) . LedgerTxCardanoAPIInternal.fromCardanoTxIn) txIns''
             in txInsReferenceToPlutusTxIns txInsReference
        getTxBodyContentSignatures :: CardanoApi.TxBodyContent ctx era -> [LedgerApiV2.PubKeyHash]
        getTxBodyContentSignatures CardanoApi.TxBodyContent{..} =
            case txExtraKeyWits of
                CardanoApi.TxExtraKeyWitnessesNone ->
                    []
                CardanoApi.TxExtraKeyWitnesses _ xs ->
                    LedgerTxCardanoAPIInternal.fromCardanoPaymentKeyHash <$> xs
        ----------------------
        !txIns' = LedgerTx.getCardanoTxInputs balanceTx -- CardanoApi.txIns
        !txInsReference' = getTxBodyContentInputsReference txBodyContent -- CardanoApi.txInsReference
        !txOuts' = LedgerTx.getCardanoTxOutRefs balanceTx -- CardanoApi.txOuts
        !txFee' = LedgerTx.getCardanoTxFee balanceTx -- LedgerTxCardanoAPIInternal.fromCardanoFee $ CardanoApi.txFee txBodyContent  -- LedgerTx.txFee emTx
        !txValidityRange' = LedgerTx.getCardanoTxValidityRange balanceTx -- CardanoApi.txValidityRange txBodyContent -- CardanoNodeEmulatorTimeSlot.slotRangeToPOSIXTimeRange slotConfig (LedgerTx.txValidRange emTx)
        !txWithdrawals' = LedgerApiV2.fromList [] -- (LedgerApiV2.StakingHash $ LedgerTx.withdrawalCredential w, LedgerTx.withdrawalAmount w) | w <- txWithdrawals'] -- LedgerShelleyTxBody.unWdrl _wdrls -- CardanoApi.txWithdrawals txBodyContent -- _wdrls -- CardanoApi.txWithdrawals txBodyContent -- LedgerTx.txWithdrawals emTx
        !txCertificates' = [] -- _certs -- CardanoApi.txCertificates txBodyContent -- LedgerTx.certificateDcert <$> LedgerTx.txCertificates emTx
        !txMintValue' = LedgerTx.getCardanoTxMint balanceTx -- CardanoApi.txMintValue txBodyContent --LedgerTx.txMint emTx -- _mint
        -- txInfoRedeemers' = [] -- LedgerTx.getCardanoTxRedeemers balanceTx -- LedgerTx.txRedeemers emTx -- [(red', LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData () )]
        !txInfoRedeemers'' = mapM (BabbageTxInfo.transRedeemerPtr bodyTx) allRedeemers
        -- filter only left from either redeemer
        !txInfoRedeemers' = (\case Left _ -> []; Right x -> x) txInfoRedeemers''
        !txInfoData' = LedgerTx.getCardanoTxData balanceTx -- LedgerApiV2.fromList $ DataMap.toList $ LedgerTx.txData emTx
        !txInfoId' = LedgerTx.getCardanoTxId balanceTx -- LedgerTx.txId emTx
        !txInfoSignatories' = getTxBodyContentSignatures txBodyContent -- BabbageTxBody.reqSignerHashes txBodyContent --_reqSignerHashes -- LedgerTx.txSignatures emTx
        -- CardanoApi.txInsCollateral = TxInsCollateralNone
        -- CardanoApi.txTotalCollateral
        -- CardanoApi.txReturnCollateral
        -- CardanoApi.txMetadata
        -- CardanoApi.txAuxScripts
        -- CardanoApi.txExtraKeyWits
        -- CardanoApi.txProtocolParams = BuildTxWith Nothing
        -- CardanoApi.txUpdateProposal
        -- CardanoApi.txScriptValidity
        ----------------------
        getDecoratedTxOut :: LedgerApiV2.TxOutRef -> PlutusContract.Contract w s DataText.Text LedgerTx.DecoratedTxOut
        getDecoratedTxOut txInput' = do
            decoratedTxOut' <- PlutusContract.txOutFromRef txInput'
            let decoratedTxOut = OnChainHelpers.fromJust decoratedTxOut'
            return decoratedTxOut
        ----------------------
        -- getValueFromTxOutRef :: LedgerApiV2.TxOutRef -> PlutusContract.Contract w s DataText.Text LedgerApiV2.Value
        -- getValueFromTxOutRef txOutRef = do
        --     decoratedTxOut' <- PlutusContract.txOutFromRef txOutRef
        --     let decoratedTxOut = OnChainHelpers.fromJust decoratedTxOut'
        --     let value = OffChainHelpers.getValueFromDecoratedTxOut decoratedTxOut
        --     return value
        ----------------------
        getOutputDatumFromTxOutRef :: LedgerApiV2.TxOutRef -> PlutusContract.Contract w s DataText.Text LedgerApiV2.OutputDatum
        getOutputDatumFromTxOutRef txOutRef = do
            decoratedTxOut' <- PlutusContract.txOutFromRef txOutRef
            let decoratedTxOut = OnChainHelpers.fromJust decoratedTxOut'
                ----------------------
                toPlutusOutputDatum :: Maybe (LedgerApiV2.DatumHash, LedgerTx.DatumFromQuery) -> LedgerApiV2.OutputDatum
                toPlutusOutputDatum Nothing = LedgerApiV2.NoOutputDatum
                toPlutusOutputDatum (Just (_, LedgerTx.DatumInline d)) = LedgerApiV2.OutputDatum d
                toPlutusOutputDatum (Just (_, LedgerTx.DatumInBody d)) = LedgerApiV2.OutputDatum d
                toPlutusOutputDatum (Just (dh, _)) = LedgerApiV2.OutputDatumHash dh
            ----------------------
            let datum = toPlutusOutputDatum $ decoratedTxOut ControlLens.^? LedgerTx.decoratedTxOutDatum
            return datum
        --------------------
        getReferenceScriptHashFromTxOutRef :: LedgerApiV2.TxOutRef -> PlutusContract.Contract w s DataText.Text (Maybe LedgerApiV2.ScriptHash)
        getReferenceScriptHashFromTxOutRef txOutRef = do
            decoratedTxOut' <- PlutusContract.txOutFromRef txOutRef
            let decoratedTxOut = OnChainHelpers.fromJust decoratedTxOut'
            let script' = decoratedTxOut ControlLens.^? LedgerTx.decoratedTxOutReferenceScript
            let script = OnChainHelpers.fromJust script'
            -- TODO: reemplaze esto: return $ P.maybe Nothing (Just . Ledger.scriptHash) script
            return ((Just . Ledger.scriptHash) P.=<< script)
        --------------------
        getReferenceScriptHashFromLedgerTxOut :: Ledger.TxOut -> PlutusContract.Contract w s DataText.Text (Maybe LedgerApiV2.ScriptHash)
        getReferenceScriptHashFromLedgerTxOut txOut = do
            let (CardanoApi.TxOut _ _ _ refScript') = LedgerTx.getTxOut txOut
            let refScript = PlutusContractCardanoAPI.fromCardanoTxOutRefScript refScript'
            let script' = ChainIndex.fromReferenceScript refScript
            return ((Just . Ledger.scriptHash) P.=<< script')
        --------------------
        getTxInInfoFromLedgerTxInput :: Ledger.TxIn -> PlutusContract.Contract w s DataText.Text LedgerApiV2.TxInInfo
        getTxInInfoFromLedgerTxInput txInput = do
            let txOutRef = LedgerTx.txInRef txInput
            decoratedTxOut <- getDecoratedTxOut txOutRef
            let address = LedgerTx._decoratedTxOutAddress decoratedTxOut -- ControlLens.^? LedgerTx.decoratedTxOutAddress
            let value = OffChainHelpers.getValueFromDecoratedTxOut decoratedTxOut
            datum <- getOutputDatumFromTxOutRef txOutRef
            refScriptHash <- getReferenceScriptHashFromTxOutRef txOutRef
            return $
                LedgerApiV2.TxInInfo
                    (LedgerTx.txInRef txInput)
                    (LedgerApiV2.TxOut address value datum refScriptHash)
        --------------------
        getTxOutFromLedgerTxOut :: (Ledger.TxOut, Ledger.TxOutRef) -> PlutusContract.Contract w s DataText.Text LedgerApiV2.TxOut
        getTxOutFromLedgerTxOut (txOut, _) =
            return $ LedgerTxCardanoAPIInternal.fromCardanoTxOutToPV2TxInfoTxOut $ LedgerTx.getTxOut txOut
        --------------------
        slotConfig :: CardanoNodeEmulatorTimeSlot.SlotConfig
        !slotConfig = DataDefault.def
    -- CardanoNodeEmulatorTimeSlot.SlotConfig
    --     { scSlotLength = 1,
    --       scSlotZeroTime = 0-- slotZeroTime
    --     }
    --------------------
    !mockTxInfoInputs <- traverse getTxInInfoFromLedgerTxInput txIns' -- :: [LedgerApiV2.TxInInfo]
    !mockTxReferenceInputs <- traverse getTxInInfoFromLedgerTxInput txInsReference' -- :: [LedgerApiV2.TxInInfo]
    !mockTxInfoOutputs <- traverse getTxOutFromLedgerTxOut txOuts' -- :: [LedgerApiV2.TxOut]
    ------------------
    let showExMemory :: LedgerApiV2.ExMemory -> P.String
        showExMemory (LedgerApiV2.ExMemory mem) = showStrNumbers mem
        ----------------------
        showExCPU :: LedgerApiV2.ExCPU -> P.String
        showExCPU (LedgerApiV2.ExCPU cpu) = showStrNumbers cpu
        ----------------------
        showStrNumbers :: (P.Show a) => a -> P.String
        showStrNumbers = OffChainHelpers.addSeparatorEachN '_' 3 . P.show
    ----------------------
    let showDatumType_ :: [LedgerApiV2.BuiltinData -> Maybe P.String] -> LedgerApiV2.BuiltinData -> Maybe P.String
        showDatumType_ [] bs = Just $ P.show bs
        showDatumType_ (f : fs) datum =
            case f datum of
                Nothing -> showDatumType_ fs datum
                Just d  -> Just d -- Just (LedgerApiV2.Datum $ PlutusTx.toBuiltinData d)
                ------------------
        showDatumType :: LedgerApiV2.OutputDatum -> Maybe P.String
        showDatumType (LedgerApiV2.OutputDatum (LedgerApiV2.Datum d)) = showDatumType_ showDatum d
        showDatumType (LedgerApiV2.OutputDatumHash _) = Nothing
        showDatumType LedgerApiV2.NoOutputDatum = Nothing
        ------------------
        getDatumFromOutputDatum :: LedgerApiV2.OutputDatum -> Maybe LedgerApiV2.Datum
        getDatumFromOutputDatum (LedgerApiV2.OutputDatum d)     = Just d
        getDatumFromOutputDatum (LedgerApiV2.OutputDatumHash _) = Nothing
        getDatumFromOutputDatum LedgerApiV2.NoOutputDatum       = Nothing
        ------------------
        getUnsafeDatumFromTxOutRef :: LedgerApiV2.TxOutRef -> PlutusContract.Contract w s DataText.Text LedgerApiV2.Datum
        getUnsafeDatumFromTxOutRef txOutRef = do
            outputDatum <- getOutputDatumFromTxOutRef txOutRef
            case getDatumFromOutputDatum outputDatum of
                Nothing -> PlutusContract.throwError "getUnsafeDatumFromTxOutRef: No Datum"
                Just d -> return d
        ----------------------
        fromCardanoTxOutDatum :: CardanoApi.TxOutDatum CardanoApi.CtxTx era -> LedgerApiV2.OutputDatum
        fromCardanoTxOutDatum CardanoApi.TxOutDatumNone =
            LedgerApiV2.NoOutputDatum
        fromCardanoTxOutDatum (CardanoApi.TxOutDatumHash _ h) =
            LedgerApiV2.OutputDatumHash $ LedgerApiV2.DatumHash $ toBuiltin (CardanoApi.serialiseToRawBytes h)
        fromCardanoTxOutDatum (CardanoApi.TxOutDatumInTx _ d) =
            LedgerApiV2.OutputDatum $ LedgerApiV2.Datum $ PlutusContractCardanoAPI.fromCardanoScriptData d
        -- LedgerApiV2.OutputDatumHash $ LedgerApiV2.DatumHash $ PlutusTxPrelude.toBuiltin (CardanoApi.serialiseToRawBytes (CardanoApi.hashScriptData d))
        fromCardanoTxOutDatum (CardanoApi.TxOutDatumInline _ d) =
            LedgerApiV2.OutputDatum $ LedgerApiV2.Datum $ PlutusContractCardanoAPI.fromCardanoScriptData d
        ------------------
        formatValues value = [P.show v | v <- OnChainHelpers.flattenValue value]
        ------------------
        formatTxInInfo :: P.String -> (Integer, LedgerApiV2.TxInInfo) -> PlutusContract.Contract w s DataText.Text [P.String]
        formatTxInInfo title (n, txInInfo) = do
            let txOutRef = LedgerApiV2.txInInfoOutRef txInInfo
            decoratedTxOut <- getDecoratedTxOut txOutRef
            let address = LedgerTx._decoratedTxOutAddress decoratedTxOut -- ControlLens.^? LedgerTx.decoratedTxOutAddress
            let value = OffChainHelpers.getValueFromDecoratedTxOut decoratedTxOut
            datum <- getOutputDatumFromTxOutRef txOutRef
            let datumTypeStr = showDatumType datum
            refScriptHash <- getReferenceScriptHashFromTxOutRef txOutRef
            let sep = case n of
                    0 -> []
                    _ -> ["--------------------------------"]
            return $
                sep
                    ++ [ title ++ ": " ++ P.show (n + 1 :: Integer)
                       , "TxOutRef: " ++ P.show txOutRef
                       , "Address: " ++ P.show address
                       , "Values: "
                       ]
                    ++ formatValues value
                    ++ [
                         -- "Datum: " ++ P.show datum,
                         "Datum: "
                       , P.show datumTypeStr
                       , "ReferenceScript: " ++ P.show refScriptHash -- TODO
                       ]
        ------------------
        formatTxInInfos :: P.String -> [LedgerApiV2.TxInInfo] -> PlutusContract.Contract w s DataText.Text [P.String]
        formatTxInInfos _ [] = return []
        formatTxInInfos title txInsInfo = do
            formatList <- mapM (formatTxInInfo title) (OnChainHelpers.enumerate txInsInfo)
            return $ concat formatList
        ------------------
        formatTxOut :: (Integer, (Ledger.TxOut, Ledger.TxOutRef)) -> PlutusContract.Contract w s DataText.Text [P.String]
        formatTxOut (n, (txOut, txOutRef)) = do
            let (CardanoApi.TxOut address' value' datum' _) = LedgerTx.getTxOut txOut
            let address = Ledger.toPlutusAddress address'
            let value = LedgerTxCardanoAPIInternal.fromCardanoTxOutValue value'
            let datum = fromCardanoTxOutDatum datum'
            let datumTypeStr = showDatumType datum
            refScriptHash <- getReferenceScriptHashFromLedgerTxOut txOut
            let sep = case n of
                    0 -> []
                    _ -> ["--------------------------------"]
            return $
                sep
                    ++ [ "Out: " ++ P.show (n + 1 :: Integer)
                       , "TxOutRef: " ++ P.show txOutRef
                       , "Address: " ++ P.show address
                       , "Values: "
                       ]
                    ++ formatValues value
                    ++ [
                         -- "Datum': " ++ P.show datum',
                         -- "Datum: " ++ P.show datum,
                         "Datum: "
                       , P.show datumTypeStr
                       , "ReferenceScript: " ++ P.show refScriptHash -- TODO
                       ]
        ------------------
        formatTxOuts :: [(Ledger.TxOut, Ledger.TxOutRef)] -> PlutusContract.Contract w s DataText.Text [P.String]
        formatTxOuts [] = return []
        formatTxOuts txOuts = do
            formatList <- mapM formatTxOut (OnChainHelpers.enumerate txOuts)
            return $ concat formatList
    ---------------------
    OffChainHelpers.printTitle (nameEndPoint ++ " : Inputs & Outputs")
    ---------------------
    OffChainHelpers.printSubTitle "Reference Inputs"
    formatMockTxReferenceInputs <- formatTxInInfos "Ref" mockTxReferenceInputs
    mapM_ (PlutusContract.logInfo @P.String) formatMockTxReferenceInputs
    ---------------------
    OffChainHelpers.printSubTitle "Inputs"
    formatMockTxInfoInputs <- formatTxInInfos "In" mockTxInfoInputs
    mapM_ (PlutusContract.logInfo @P.String) formatMockTxInfoInputs
    ---------------------
    OffChainHelpers.printSubTitle "Outputs"
    formatTxOuts' <- formatTxOuts txOuts'
    mapM_ (PlutusContract.logInfo @P.String) formatTxOuts'
    ---------------------
    OffChainHelpers.printTitle (nameEndPoint ++ " : Memory & Size")
    ---------------------
    ControlMonad.when (sizeTx > 16000) P.$
        PlutusContract.throwError @DataText.Text $ OffChainHelpers.stringToStrictText $ TextPrintf.printf "Tx is too big: %s" (showStrNumbers sizeTx)
    -- if sizeTx > 16000
    --     then PlutusContract.throwError @DataText.Text $ OffChainHelpers.stringToStrictText $ TextPrintf.printf "Tx is too big: %s" (showStrNumbers sizeTx)
    --     else return ()
    ------------------------
    let !allUnits = map (\(_, (_, AlonzoScripts.ExUnits mem steps)) -> (mem, steps)) allRedeemers
        !sumUnitsMem = foldl (\acc (mem, _) -> acc `GHCNatural.plusNatural` mem) 0 allUnits
        !sumUnitsSteps = foldl (\acc (_, steps) -> acc `GHCNatural.plusNatural` steps) 0 allUnits
        formatUnits !list = concat [["ExMemory: " ++ showStrNumbers mem ++ ", ExCPU: " ++ showStrNumbers step] | (mem, step) <- list]
    ---------------------
    mapM_ (PlutusContract.logInfo @P.String) (formatUnits allUnits)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Total ExMemory: %s, Total ExCPU: %s, Total Size: %s" (showStrNumbers sumUnitsMem) (showStrNumbers sumUnitsSteps) (showStrNumbers sizeTx)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Valid Tx: %s" (P.show validTx)
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "------------------------------------------------------------"
    ------------------------
    let mockTxInfoFee :: LedgerApiV2.Value
        mockTxInfoFee = txFee'
        ----------------------
        mockTxInfoMint :: LedgerApiV2.Value
        mockTxInfoMint = txMintValue'
        ----------------------
        mockTxInfoDCert :: [LedgerApiV2.DCert]
        mockTxInfoDCert = txCertificates'
        ----------------------
        mockTxInfoWdrl :: LedgerApiV2.Map LedgerApiV2.StakingCredential Integer
        mockTxInfoWdrl = txWithdrawals'
        ----------------------
        mockTxInfoValidRange :: LedgerApiV2.POSIXTimeRange
        mockTxInfoValidRange = CardanoNodeEmulatorTimeSlot.slotRangeToPOSIXTimeRange slotConfig txValidityRange'
        ----------------------
        mockTxInfoSignatories :: [LedgerApiV2.PubKeyHash]
        mockTxInfoSignatories = txInfoSignatories'
        ----------------------
        mockTxInfoRedeemers :: LedgerApiV2.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
        mockTxInfoRedeemers = LedgerApiV2.fromList txInfoRedeemers'
        ----------------------
        mockTxInfoData :: LedgerApiV2.Map LedgerApiV2.DatumHash LedgerApiV2.Datum
        mockTxInfoData = LedgerApiV2.fromList $ DataMap.toList txInfoData'
        ----------------------
        mockTxInfoId :: LedgerApiV2.TxId
        mockTxInfoId = txInfoId'
        ----------------------
        getTxInfo :: LedgerApiV2.TxInfo
        getTxInfo =
            LedgerApiV2.TxInfo
                mockTxInfoInputs
                mockTxReferenceInputs
                mockTxInfoOutputs
                mockTxInfoFee
                mockTxInfoMint
                mockTxInfoDCert
                mockTxInfoWdrl
                mockTxInfoValidRange
                mockTxInfoSignatories
                mockTxInfoRedeemers
                mockTxInfoData
                mockTxInfoId
        ----------------------
        evalRedeemer :: (LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer) -> PlutusContract.Contract w s DataText.Text (LedgerApiV2.ExMemory, LedgerApiV2.ExCPU, P.Integer)
        evalRedeemer (scriptPurpose, redeemer) = do
            case scriptPurpose of
                LedgerApiV2.Minting mintingHash -> do
                    OffChainHelpers.printSubTitle "EvalRedeemer Minting"
                    let policy' = DataMap.lookup mintingHash $ DataMap.fromList listOfMintingScripts
                    case policy' of
                        Nothing -> do
                            PlutusContract.logInfo @P.String $ "Redeemer: Minting Policy to Eval not found"
                            return (0, 0, 0)
                        Just policy -> do
                            let mockCtx :: LedgerApiV2.ScriptContext
                                mockCtx = LedgerApiV2.ScriptContext getTxInfo scriptPurpose
                                ----------------------
                                !paramsData = [] :: [LedgerApiV2.Data]
                                !redeemerData = LedgerApiV2.toData redeemer
                                !ctxData = LedgerApiV2.toData mockCtx
                                (eval_log, eval_err, eval_size) = evaluateScriptPolicy policy paramsData redeemerData ctxData
                            --------------------------------
                            PlutusContract.logInfo @P.String $ "ScriptPurpose: " ++ P.show scriptPurpose
                            PlutusContract.logInfo @P.String $ "--------------------------------"
                            -- PlutusContract.logInfo @P.String $ "import Helpers.OffChainEval"
                            -- PlutusContract.logInfo @P.String $ "evaluateScriptPolicy_With_StringParams "
                            -- PlutusContract.logInfo @P.String $ "{\"epHash\":\"" ++ P.show mintingHash ++ "\",\"epRdeemer\":\""++ P.show (OffChainHelpers.getEncodedJsonFromData redeemerData)++"\",\"epCtx\":\""++ P.show (OffChainHelpers.getEncodedJsonFromData ctxData)++"\"}"
                            -- PlutusContract.logInfo @P.String $ "--------------------------------"
                            PlutusContract.logInfo @P.String $ "Redeemer: " ++ P.show redeemer
                            PlutusContract.logInfo @P.String $ "Log: " ++ P.show eval_log
                            PlutusContract.logInfo @P.String $ "--------------------------------"
                            case eval_err of
                                Left _ -> do
                                    PlutusContract.logInfo @P.String $ "Eval Error , Size: " ++ showStrNumbers eval_size
                                    return (0, 0, eval_size)
                                Right exbudget -> do
                                    PlutusContract.logInfo @P.String $ "ExMemory: " ++ showExMemory (LedgerApiV2.exBudgetMemory exbudget) ++ ", ExCPU: " ++ showExCPU (LedgerApiV2.exBudgetCPU exbudget) ++ ", " ++ "Size: " ++ showStrNumbers eval_size
                                    return (LedgerApiV2.exBudgetMemory exbudget, LedgerApiV2.exBudgetCPU exbudget, eval_size)
                -- PlutusContract.logInfo @P.String $ TextPrintf.printf "------------------------------------------------------------"
                LedgerApiV2.Spending txOutRef -> do
                    OffChainHelpers.printSubTitle "EvalRedeemer Spending"
                    let mockCtx :: LedgerApiV2.ScriptContext
                        mockCtx = LedgerApiV2.ScriptContext getTxInfo scriptPurpose
                    ----------------------
                    dec <- getDecoratedTxOut txOutRef
                    let address = LedgerTx._decoratedTxOutAddress dec
                        validator' = DataMap.lookup address $ DataMap.fromList listOfValidators
                    ----------------------
                    case validator' of
                        Nothing -> do
                            PlutusContract.logInfo @P.String $ "Redeemer: Validator to Eval not found"
                            return (0, 0, 0)
                        Just validator -> do
                            datum <- getUnsafeDatumFromTxOutRef txOutRef
                            let datumTypeStr = showDatumType_ showDatum (LedgerApiV2.getDatum datum)
                            let !paramsData = [] :: [LedgerApiV2.Data]
                                !datumData = LedgerApiV2.toData datum
                                !redeemerData = LedgerApiV2.toData redeemer
                                !ctxData = LedgerApiV2.toData mockCtx
                                (eval_log, eval_err, eval_size) = evaluateScriptValidator validator paramsData datumData redeemerData ctxData
                            --------------------------------
                            PlutusContract.logInfo @P.String $ "ScriptPurpose: " ++ P.show scriptPurpose
                            PlutusContract.logInfo @P.String $ "--------------------------------"
                            PlutusContract.logInfo @P.String $ "import Helpers.OffChainEval"
                            PlutusContract.logInfo @P.String $ "evaluateScriptValidator_With_StringParams "
                            PlutusContract.logInfo @P.String $ "{\"evHash\":\"" ++ P.show (OffChainHelpers.hashValidator validator) ++ "\",\"evDatum\":\"" ++ P.show (OffChainHelpers.getEncodedJsonFromData datumData) ++ "\",\"evRedeemer\":\"" ++ P.show (OffChainHelpers.getEncodedJsonFromData redeemerData) ++ "\",\"evCtx\":\"" ++ P.show (OffChainHelpers.getEncodedJsonFromData ctxData) ++ "\"}"
                            PlutusContract.logInfo @P.String $ "--------------------------------"
                            PlutusContract.logInfo @P.String $ ":set -XOverloadedStrings"
                            PlutusContract.logInfo @P.String $ "import PlutusTx"
                            PlutusContract.logInfo @P.String $ "import qualified Plutus.V2.Ledger.Api                          as LedgerApiV2"
                            PlutusContract.logInfo @P.String $ "import qualified Ledger.Value                                  as LedgerValue"
                            PlutusContract.logInfo @P.String $ "import qualified Prelude                                       as P"
                            PlutusContract.logInfo @P.String $ "import Helpers.OffChain"
                            PlutusContract.logInfo @P.String $ "import Helpers.OffChainEval"
                            PlutusContract.logInfo @P.String $ "import Protocol.xxxxx.OnChain"
                            PlutusContract.logInfo @P.String $ "import Protocol.xxxxx.Types"
                            PlutusContract.logInfo @P.String $ "validatorParams = ValidatorParams { xxx = xxx} "
                            PlutusContract.logInfo @P.String $ "validatorWithParams = validator validatorParams"
                            PlutusContract.logInfo @P.String $ "paramsData = [] :: [LedgerApiV2.Data]"
                            PlutusContract.logInfo @P.String $ "datumEncodedJson = " ++ P.show (OffChainHelpers.getEncodedJsonFromData datumData)
                            PlutusContract.logInfo @P.String $ "datumData = getDataFromEncodedJson datumEncodedJson"
                            PlutusContract.logInfo @P.String $ "redeemerEncodedJson = " ++ P.show (OffChainHelpers.getEncodedJsonFromData redeemerData)
                            PlutusContract.logInfo @P.String $ "redeemerData = getDataFromEncodedJson redeemerEncodedJson"
                            PlutusContract.logInfo @P.String $ "ctxEncodedJson = " ++ P.show (OffChainHelpers.getEncodedJsonFromData ctxData)
                            PlutusContract.logInfo @P.String $ "ctxData = getDataFromEncodedJson ctxEncodedJson"
                            PlutusContract.logInfo @P.String $ "evaluateScriptValidator validatorWithParams paramsData datumData redeemerData ctx"
                            PlutusContract.logInfo @P.String $ "--------------------------------"
                            PlutusContract.logInfo @P.String $ "Script Hash: " ++ P.show (OffChainHelpers.hashValidator validator)
                            PlutusContract.logInfo @P.String $ "Script Address: " ++ P.show address
                            PlutusContract.logInfo @P.String $ "Datum: " ++ P.show datumTypeStr
                            PlutusContract.logInfo @P.String $ "Redeemer: " ++ P.show redeemer
                            PlutusContract.logInfo @P.String $ "Log: " ++ P.show eval_log
                            PlutusContract.logInfo @P.String $ "--------------------------------"
                            case eval_err of
                                Left _ -> do
                                    PlutusContract.logInfo @P.String $ "Eval Error, Size: " ++ showStrNumbers eval_size
                                    return (0, 0, eval_size)
                                Right exbudget -> do
                                    PlutusContract.logInfo @P.String $ "ExMemory: " ++ showExMemory (LedgerApiV2.exBudgetMemory exbudget) ++ ", ExCPU: " ++ showExCPU (LedgerApiV2.exBudgetCPU exbudget) ++ ", " ++ "Size: " ++ showStrNumbers eval_size
                                    return (LedgerApiV2.exBudgetMemory exbudget, LedgerApiV2.exBudgetCPU exbudget, eval_size)
                LedgerApiV2.Rewarding _ ->
                    return (0, 0, 0)
                LedgerApiV2.Certifying _ ->
                    return (0, 0, 0)
    --------------------
    resultEvalRedemeers <- mapM evalRedeemer txInfoRedeemers'
    let !sumsEvals = foldl (\(mem', steps', size') (mem, steps, size) -> (mem' P.+ mem, steps' P.+ steps, size' + size)) (0, 0, 0) resultEvalRedemeers
    OffChainHelpers.printSubTitle "EvalRedeemer Totals"
    let getFst (fst', _, _) = fst'
        getSnd (_, snd', _) = snd'
        getThd (_, _, thd') = thd'
    PlutusContract.logInfo @P.String $ "--------------------------------"
    PlutusContract.logInfo @P.String $ "--------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Total ExMemory: %s, Total ExCPU: %s, Total Size: %s" (showExMemory (getFst sumsEvals)) (showExCPU (getSnd sumsEvals)) (showStrNumbers (getThd sumsEvals))
    PlutusContract.logInfo @P.String $ "--------------------------------"
    PlutusContract.logInfo @P.String $ "--------------------------------"
    --------------------
    OffChainHelpers.printTitle (nameEndPoint ++ " : Submit Tx")
    !submittedTx <- PlutusContract.submitBalancedTx balanceTx
    PlutusContract.logInfo @P.String $ TextPrintf.printf "SubmitedTx (txId: %s)" (P.show $ Ledger.getCardanoTxId submittedTx)
    !cSlot <- PlutusContract.currentNodeClientSlot
    !params <- PlutusContract.getParams
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "params: %s" (P.show params)
    --------------------
    let -- getCUtxoIndex :: LedgerTx.UnbalancedTx -> PlutusContract.Contract w s DataText.Text LedgerTx.SomeCardanoApiTx
        getcUtxoIndex (Right cUtxoIndex') = return cUtxoIndex'
        getcUtxoIndex (Left _) = PlutusContract.throwError "Cant get cUtxoIndex"
    -- cUtxoIndex <- getCUtxoIndex txUnBalanced
    cUtxoIndex <- getcUtxoIndex $ LedgerTxCardanoAPI.fromPlutusIndex $ Ledger.UtxoIndex $ LedgerTxConstraints.unBalancedTxUtxoIndex txUnBalanced
    --------------------
    -- Ledger.UtxoIndex DataMap.empty --mempty  -- Ledger.UtxoIndex DataMap.empty --LedgerTxCardanoAPI.fromPlutusIndex mempty -- (Ledger.UtxoIndex utxo)
    let validateCardanoTx = CardanoNodeEmulatorValidation.validateCardanoTx params cSlot cUtxoIndex submittedTx
    case validateCardanoTx of
        -- Left (validationPhase, validationError) ->
        Left err ->
            -- PlutusContract.throwError @DataText.Text $ OffChainHelpers.stringToStrictText $ TextPrintf.printf "validateCardanoTx (txId: %s) - ERROR: %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show err)
            -- PlutusContract.logError @P.String $ P.show err
            PlutusContract.throwError @DataText.Text $ OffChainHelpers.stringToStrictText $ TextPrintf.printf "ERROR: %s" (P.show err)
        Right mp -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "validateCardanoTx (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show mp)
            !txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
            PlutusContract.logInfo @P.String $ TextPrintf.printf "ConfirmedTx (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)
    OffChainHelpers.printSeparator

------------------------------------------------------------------------------
-- HELPERS evalAndSubmitTx
------------------------------------------------------------------------------

findHashAndReadMintingPolicy :: P.FilePath -> P.String -> P.IO (Maybe LedgerApiV2.MintingPolicy)
findHashAndReadMintingPolicy directory valueToSearch = do
    files <- listDirectoryRecursive directory ".symbol"
    P.putStrLn $ "Searching Minting Hash: " ++ valueToSearch ++ " in files: " ++ P.show files
    matchingFile <- findValueInFiles valueToSearch files
    case matchingFile of
        Just fileName -> readPlutusFileAsMintingPolicy fileName
        Nothing       -> return Nothing

findHashAndReadValidator :: P.FilePath -> P.String -> P.IO (Maybe LedgerApiV2.Validator)
findHashAndReadValidator directory valueToSearch = do
    files <- listDirectoryRecursive directory ".hash"
    P.putStrLn $ "Searching Script Hash: " ++ valueToSearch ++ " in files: " ++ P.show files
    matchingFile <- findValueInFiles valueToSearch files
    case matchingFile of
        Just fileName -> readPlutusFileAsValidator fileName
        Nothing       -> return Nothing

isCorrectFile :: P.FilePath -> P.FilePath -> Bool
isCorrectFile filePath filterFile = SystemFilePath.takeExtension filePath P.== filterFile

listDirectoryRecursive :: P.FilePath -> P.FilePath -> P.IO [P.FilePath]
listDirectoryRecursive path filterFile = do
    isDir <- SystemDirectory.doesDirectoryExist path
    if isDir
        then do
            contents <- SystemDirectory.listDirectory path
            let paths = map (path SystemFilePath.</>) contents
            Monad.foldM
                ( \acc path' -> do
                    subP <- listDirectoryRecursive path' filterFile
                    return $ subP ++ acc
                )
                []
                paths
        else
            if isCorrectFile path filterFile
                then return [path]
                else return []

findValueInFiles :: P.String -> [P.FilePath] -> P.IO (Maybe P.FilePath)
findValueInFiles _ [] = return Nothing
findValueInFiles valueToSearch (filePath : rest) = do
    contents <- LBS.readFile filePath
    P.putStrLn $ "Searching file: " ++ P.show filePath
    if OffChainHelpers.stringContains valueToSearch (OffChainHelpers.lazyByteStringToString contents)
        then do
            return $ Just filePath
        else findValueInFiles valueToSearch rest

decodeSymbolFile :: LBS.ByteString -> P.String
decodeSymbolFile = OffChainHelpers.lazyByteStringToString

extractPathAndFilename :: P.FilePath -> (P.FilePath, P.FilePath)
extractPathAndFilename filePath = (SystemFilePath.takeDirectory filePath, SystemFilePath.takeFileName filePath)

readPlutusFileAsMintingPolicy :: P.FilePath -> P.IO (Maybe LedgerApiV2.MintingPolicy)
readPlutusFileAsMintingPolicy filePath = do
    let plutusfilePath = SystemFilePath.dropExtension filePath ++ ".plutus"
    fileExists <- SystemDirectory.doesFileExist plutusfilePath
    if fileExists
        then do
            let (path, fileName) = extractPathAndFilename plutusfilePath
            (Right policy) <- OffChainHelpers.readMintingPolicy path fileName
            return $ Just policy
        else return Nothing

readPlutusFileAsValidator :: P.FilePath -> P.IO (Maybe LedgerApiV2.Validator)
readPlutusFileAsValidator filePath = do
    let plutusfilePath = SystemFilePath.dropExtension filePath ++ ".plutus"
    fileExists <- SystemDirectory.doesFileExist plutusfilePath
    if fileExists
        then do
            let (path, fileName) = extractPathAndFilename plutusfilePath
            (Right policy) <- OffChainHelpers.readValidator path fileName
            return $ Just policy
        else return Nothing

replaceQuotes :: P.String -> P.String
replaceQuotes = DataText.unpack . DataText.replace "\"\"" "\"" . DataText.pack

--------------------------------------------------------------------------------2

-- Define the testContext function to validate all redeemers using the needed scripts, validators or policies
testContext :: (LedgerApiV2.Address -> Maybe LedgerApiV2.Validator) -> (LedgerApiV2.CurrencySymbol -> Maybe LedgerApiV2.MintingPolicy) -> LedgerApiV2.ScriptContext -> (LedgerApiV2.LogOutput, P.Either LedgerApiV2.EvaluationError LedgerApiV2.ExBudget, P.Either Integer Integer)
testContext getValidator getPolicy ctx =
    --     concatMap (testRedeemer getValidator getPolicy ctx) redeemers
    let redeemers = TxAssocMap.toList $ LedgerApiV2.txInfoRedeemers $ LedgerApiV2.scriptContextTxInfo ctx
        -- !_ = DebugTrace.trace ("Redeemers: " ++ show redeemers) ()

        testRedeemerFor (purpose, redeemer) = testRedeemer getValidator getPolicy ctx (purpose, redeemer)

        results = map testRedeemerFor redeemers
        combineLogs = foldMap (\(logOutput, _, _) -> logOutput) results

        combineExBudgets exs =
            let lefts = [e | P.Left e <- exs]
                rights = [b | P.Right b <- exs]
                sumExBudget = foldr (\(LedgerApiV2.ExBudget mem cpu) (LedgerApiV2.ExBudget accMem accCpu) -> LedgerApiV2.ExBudget (accMem P.+ mem) (accCpu P.+ cpu)) (LedgerApiV2.ExBudget 0 0)
             in if not (null lefts)
                    then P.Left (head lefts)
                    else P.Right (sumExBudget rights)

        combineIntegers ints =
            let lefts = [e | P.Left e <- ints]
                rights = [i | P.Right i <- ints]
             in if not (null lefts)
                    then P.Left (head lefts)
                    else P.Right (sum rights)

        combinedExBudget = combineExBudgets [ex | (_, ex, _) <- results]
        combinedInteger = combineIntegers [int | (_, _, int) <- results]
     in -- results ++
        (combineLogs, combinedExBudget, combinedInteger)

--------------------------------------------------------------------------------2

-- test a redeemer with validator or minting policy
testRedeemer :: (LedgerApiV2.Address -> Maybe LedgerApiV2.Validator) -> (LedgerApiV2.CurrencySymbol -> Maybe LedgerApiV2.MintingPolicy) -> LedgerApiV2.ScriptContext -> (LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer) -> (LedgerApiV2.LogOutput, P.Either LedgerApiV2.EvaluationError LedgerApiV2.ExBudget, P.Either Integer Integer)
testRedeemer getValidator _ ctx (LedgerApiV2.Spending txOutRef, redeemer) =
    let !txInfoInputs = LedgerApiV2.txInfoInputs $ LedgerApiV2.scriptContextTxInfo ctx
        -- !_ = DebugTrace.trace ("txInfoInputs: " ++ show txInfoInputs) ()
        -- !_ = DebugTrace.trace ("Finding TxInInfo for: " ++ show txOutRef) ()
        !txInInfo = findTxInInfo txOutRef txInfoInputs
     in case txInInfo of
            Just !info ->
                let -- -- !_ = DebugTrace.trace ("TxInInfo Found: " ++ show info) ()
                    !txOut = LedgerApiV2.txInInfoResolved info
                    !address = LedgerApiV2.txOutAddress txOut
                    -- !_ = DebugTrace.trace ("Address: " ++ show address) ()
                    !datum = OffChainHelpers.getUnsafe_LedgerApiV2Datum_From_TxOutOutputDatum txOut
                    -- !_ = DebugTrace.trace ("Datum: " ++ show datum) ()
                    !validator = getValidator address
                    -- !_ = DebugTrace.trace ("Validator: " ++ show validator  ++ " - address: " ++ show address) ()
                    !purpose = LedgerApiV2.Spending txOutRef
                 in case validator of
                        Just !v -> evaluateScriptValidatorEX v datum redeemer (ctx{LedgerApiV2.scriptContextPurpose = purpose})
                        Nothing -> (["NO VALIDATOR FOUND"], Left P.undefined, Left 0)
            Nothing -> (["NO TxOut FOUND"], Left P.undefined, Left 0)
testRedeemer _ getPolicy ctx (LedgerApiV2.Minting cs, redeemer) =
    let -- !_ = DebugTrace.trace ("Finding MintingPolicy for: " ++ show cs) ()
        !policy = getPolicy cs
        -- !_ = DebugTrace.trace ("MintingPolicy: " ++ show policy ++ " - CS: " ++ show cs) ()
        !purpose = LedgerApiV2.Minting cs
     in -- !txInfoMint = LedgerApiV2.txInfoMint $ LedgerApiV2.scriptContextTxInfo ctx
        -- !_ = DebugTrace.trace ("txInfoMint: " ++ show txInfoMint) ()

        case policy of
            Just !mp -> evaluateScriptPolicyEX mp redeemer (ctx{LedgerApiV2.scriptContextPurpose = purpose})
            Nothing -> (["NO MINTING POLICY FOUND"], Left P.undefined, Left 0)
testRedeemer _ _ _ _ = (["UNKNOWN SCRIPT PURPOSE"], Left P.undefined, Left 0)

------------------------------------------------------------------------------
-- HELPERS testContext
------------------------------------------------------------------------------

findValidator :: [LedgerApiV2.Validator] -> LedgerApiV2.Address -> Maybe LedgerApiV2.Validator
findValidator validators address =
    find (\validator -> OffChainHelpers.addressValidator (OffChainHelpers.hashValidator validator) == address) validators

findMintingPolicy :: [LedgerApiV2.MintingPolicy] -> LedgerApiV2.CurrencySymbol -> Maybe LedgerApiV2.MintingPolicy
findMintingPolicy policies cs =
    find (\policy -> OffChainHelpers.getCurSymbolOfPolicy policy == cs) policies

findTxInInfo :: LedgerApiV2.TxOutRef -> [LedgerApiV2.TxInInfo] -> Maybe LedgerApiV2.TxInInfo
findTxInInfo txOutRef list =
    let filtered = filter (\txInInfo -> LedgerApiV2.txInInfoOutRef txInInfo == txOutRef) list
     in case filtered of
            (x : _) -> Just x
            []      -> Nothing

---------------------------------------------------------------------------------

{- | Represents a ScriptContext that is configured for successful validation.

Note: This context is primarily used for test cases simulating successful
validations. Adjustments may be needed to fit specific test case scenarios.
-}
mkBaseValidatorContext :: [LedgerApiV2.TxOut] -> [LedgerApiV2.TxOut] -> Integer -> LedgerApiV2.ScriptContext
mkBaseValidatorContext inUTxOs outUTxOs consumeInputIndex =
    mkValidatorSimpleContext inUTxOs outUTxOs consumeInputIndex (LedgerApiV2.fromList [])

mkBaseValidMintingPolicyContext :: [LedgerApiV2.TxOut] -> [LedgerApiV2.TxOut] -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.ScriptContext
mkBaseValidMintingPolicyContext inUTxOs outUTxOs cs =
    mkMintingPolicySimpleContext inUTxOs cs outUTxOs (LedgerApiV2.fromList [])

-- | Pipe operator.
(|>) :: a -> (a -> b) -> b
x |> f = f x

createValidRange :: LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTimeRange
createValidRange date' = LedgerInterval.interval (date' - 1) (date' + 1)

createInValidRange :: LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTimeRange
createInValidRange date' validTimeRange = LedgerInterval.interval (date' - 1) (date' + 1 + (validTimeRange P.* 2))

---------------------------------------------------------------------------------

{- | Helper to easily build simple but useful script contexts.
 Use the consume index to create the context purpose spending the UTxO at that index.
-}
mkValidatorSimpleContext ::
    -- | Spent UTxOs
    [LedgerApiV2.TxOut] ->
    -- | Produced UTxOs
    [LedgerApiV2.TxOut] ->
    Integer ->
    -- | Included datums
    LedgerApiV2.Map LedgerApiV2.DatumHash LedgerApiV2.Datum ->
    LedgerApiV2.ScriptContext
mkValidatorSimpleContext txIns txOuts consumeInputIndex txData = LedgerApiV2.ScriptContext txInfo purpose
  where
    txInfo :: LedgerApiV2.TxInfo
    txInfo =
        LedgerApiV2.TxInfo
            { LedgerApiV2.txInfoInputs = txInInfos
            , LedgerApiV2.txInfoReferenceInputs = []
            , LedgerApiV2.txInfoOutputs = txOuts
            , txInfoFee = mempty
            , LedgerApiV2.txInfoMint = mempty
            , txInfoDCert = []
            , txInfoWdrl = LedgerApiV2.fromList []
            , LedgerApiV2.txInfoValidRange = LedgerApiV2.always
            , LedgerApiV2.txInfoSignatories = []
            , LedgerApiV2.txInfoRedeemers = LedgerApiV2.fromList []
            , txInfoData = txData
            , txInfoId = LedgerApiV2.TxId "ff"
            }

    txInInfos :: [LedgerApiV2.TxInInfo]
    txInInfos =
        [ LedgerApiV2.TxInInfo (LedgerApiV2.TxOutRef someTxId i) txIn
        | (i, txIn) <- zip [0 ..] txIns
        ]

    purpose :: LedgerApiV2.ScriptPurpose
    purpose = LedgerApiV2.Spending $ LedgerApiV2.TxOutRef someTxId consumeInputIndex

---------------------------------------------------------------------------------

mkMintingPolicySimpleContext ::
    -- | Spent UTxOs
    [LedgerApiV2.TxOut] ->
    -- | Currency being minted
    LedgerApiV2.CurrencySymbol ->
    -- | Produced UTxOs
    [LedgerApiV2.TxOut] ->
    -- | Included datums
    LedgerApiV2.Map LedgerApiV2.DatumHash LedgerApiV2.Datum ->
    LedgerApiV2.ScriptContext
mkMintingPolicySimpleContext txIns cs txOuts txData = LedgerApiV2.ScriptContext txInfo purpose
  where
    txInfo :: LedgerApiV2.TxInfo
    txInfo =
        LedgerApiV2.TxInfo
            { LedgerApiV2.txInfoInputs = txInInfos
            , LedgerApiV2.txInfoReferenceInputs = []
            , LedgerApiV2.txInfoOutputs = txOuts
            , txInfoFee = mempty
            , LedgerApiV2.txInfoMint = mempty
            , txInfoDCert = []
            , txInfoWdrl = LedgerApiV2.fromList []
            , LedgerApiV2.txInfoValidRange = LedgerApiV2.always
            , LedgerApiV2.txInfoSignatories = []
            , LedgerApiV2.txInfoRedeemers = LedgerApiV2.fromList []
            , txInfoData = txData
            , txInfoId = LedgerApiV2.TxId "ff"
            }

    txInInfos :: [LedgerApiV2.TxInInfo]
    txInInfos =
        [ LedgerApiV2.TxInInfo (LedgerApiV2.TxOutRef someTxId i) txIn
        | (i, txIn) <- zip [0 ..] txIns
        ]

    purpose :: LedgerApiV2.ScriptPurpose
    purpose = LedgerApiV2.Minting cs



--------------------------------------------------------------------------------
-- Setters
--------------------------------------------------------------------------------

-- | Set the reference inputs of the transaction
setRefInputs :: [LedgerApiV2.TxOut] -> LedgerApiV2.ScriptContext -> LedgerApiV2.ScriptContext
setRefInputs inputs sc =
    sc
        { LedgerApiV2.scriptContextTxInfo =
            (LedgerApiV2.scriptContextTxInfo sc){LedgerApiV2.txInfoReferenceInputs = txInInfos}
        }
  where
    txInInfos :: [LedgerApiV2.TxInInfo]
    txInInfos =
        [ LedgerApiV2.TxInInfo (LedgerApiV2.TxOutRef someTxId i) txIn
        | (i, txIn) <- zip [0 ..] inputs
        ]

setInputs :: [LedgerApiV2.TxOut] -> LedgerApiV2.ScriptContext -> LedgerApiV2.ScriptContext
setInputs inputs sc =
    sc
        { LedgerApiV2.scriptContextTxInfo =
            (LedgerApiV2.scriptContextTxInfo sc){LedgerApiV2.txInfoInputs = txInInfos}
        }
  where
    txInInfos :: [LedgerApiV2.TxInInfo]
    txInInfos =
        [ LedgerApiV2.TxInInfo (LedgerApiV2.TxOutRef someTxId i) txIn
        | (i, txIn) <- zip [0 ..] inputs
        ]

setInputsAndAddRedeemers :: [(LedgerApiV2.TxOut, LedgerApiV2.Redeemer)] -> LedgerApiV2.ScriptContext -> LedgerApiV2.ScriptContext
setInputsAndAddRedeemers inputsWithRedeemer sc =
    sc
        { LedgerApiV2.scriptContextTxInfo =
            (LedgerApiV2.scriptContextTxInfo sc)
                { LedgerApiV2.txInfoInputs = txInInfos
                , LedgerApiV2.txInfoRedeemers = combinedRedeemers
                }
        }
  where
    txInInfos :: [LedgerApiV2.TxInInfo]
    txInInfos =
        [ LedgerApiV2.TxInInfo (LedgerApiV2.TxOutRef someTxId' i) txOut
        | (i, (txOut, _)) <- zip [0 ..] inputsWithRedeemer
        ]

    newRedeemersMap :: TxAssocMap.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
    newRedeemersMap =
        TxAssocMap.fromList
            [ (LedgerApiV2.Spending (LedgerApiV2.TxOutRef someTxId' i), redeemer)
            | (i, (_, redeemer)) <- zip [0 ..] inputsWithRedeemer
            ]

    existingRedeemers :: TxAssocMap.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
    existingRedeemers = LedgerApiV2.txInfoRedeemers $ LedgerApiV2.scriptContextTxInfo sc

    combinedRedeemers :: TxAssocMap.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
    combinedRedeemers = TxAssocMap.unionWith (\_ new -> new) existingRedeemers newRedeemersMap

    someTxId' = LedgerApiV2.TxId "0000000000000000000000000000000000000000000000000000000000000000"

addInputsAndAddRedeemers :: [(LedgerApiV2.TxOut, LedgerApiV2.Redeemer)] -> LedgerApiV2.ScriptContext -> LedgerApiV2.ScriptContext
addInputsAndAddRedeemers inputsWithRedeemer sc =
    sc
        { LedgerApiV2.scriptContextTxInfo =
            (LedgerApiV2.scriptContextTxInfo sc)
                { LedgerApiV2.txInfoInputs = combinedInputs
                , LedgerApiV2.txInfoRedeemers = combinedRedeemers
                }
        }
  where
    txInInfos :: [LedgerApiV2.TxInInfo]
    txInInfos =
        [ LedgerApiV2.TxInInfo (LedgerApiV2.TxOutRef someTxId' i) txOut
        | (i, (txOut, _)) <- zip [0 ..] inputsWithRedeemer
        ]

    existingInputs :: [LedgerApiV2.TxInInfo]
    existingInputs = LedgerApiV2.txInfoInputs $ LedgerApiV2.scriptContextTxInfo sc

    combinedInputs :: [LedgerApiV2.TxInInfo]
    combinedInputs = existingInputs ++ txInInfos

    newRedeemersMap :: TxAssocMap.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
    newRedeemersMap =
        TxAssocMap.fromList
            [ (LedgerApiV2.Spending (LedgerApiV2.TxOutRef someTxId' i), redeemer)
            | (i, (_, redeemer)) <- zip [0 ..] inputsWithRedeemer
            ]

    existingRedeemers :: TxAssocMap.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
    existingRedeemers = LedgerApiV2.txInfoRedeemers $ LedgerApiV2.scriptContextTxInfo sc

    combinedRedeemers :: TxAssocMap.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
    combinedRedeemers = TxAssocMap.unionWith (\_ new -> new) existingRedeemers newRedeemersMap

    someTxId' = LedgerApiV2.TxId "0000000000000000000000000000000000000000000000000000000000000000"

setInputWithRef :: (LedgerApiV2.TxOut, LedgerApiV2.TxOutRef) -> LedgerApiV2.ScriptContext -> LedgerApiV2.ScriptContext
setInputWithRef (input, ref) sc =
    sc
        { LedgerApiV2.scriptContextTxInfo =
            (LedgerApiV2.scriptContextTxInfo sc){LedgerApiV2.txInfoInputs = [LedgerApiV2.TxInInfo ref input]}
        }

setInfoRedeemers :: [LedgerApiV2.ScriptPurpose] -> [LedgerApiV2.Redeemer] -> LedgerApiV2.ScriptContext -> LedgerApiV2.ScriptContext
setInfoRedeemers purposes redeemers sc =
    sc
        { LedgerApiV2.scriptContextTxInfo =
            (LedgerApiV2.scriptContextTxInfo sc)
                { LedgerApiV2.txInfoRedeemers = LedgerApiV2.fromList $ zip purposes redeemers
                }
        }

setOutputs :: [LedgerApiV2.TxOut] -> LedgerApiV2.ScriptContext -> LedgerApiV2.ScriptContext
setOutputs outputs sc =
    sc
        { LedgerApiV2.scriptContextTxInfo =
            (LedgerApiV2.scriptContextTxInfo sc){LedgerApiV2.txInfoOutputs = outputs}
        }

-- | Set the minting value of the transaction
setMint :: LedgerApiV2.Value -> LedgerApiV2.ScriptContext -> LedgerApiV2.ScriptContext
setMint val sc =
    sc
        { LedgerApiV2.scriptContextTxInfo = (LedgerApiV2.scriptContextTxInfo sc){LedgerApiV2.txInfoMint = val}
        }

-- | Set the minting value of the transaction and add the redeemers
setMintAndRedeemers :: [(LedgerApiV2.Value, LedgerApiV2.Redeemer)] -> LedgerApiV2.ScriptContext -> LedgerApiV2.ScriptContext
setMintAndRedeemers valsAndRedeemers sc =
    sc
        { LedgerApiV2.scriptContextTxInfo =
            (LedgerApiV2.scriptContextTxInfo sc)
                { LedgerApiV2.txInfoMint = combinedValue
                , LedgerApiV2.txInfoRedeemers = combinedRedeemers
                }
        }
  where
    -- Function to flatten the value and extract the single currency symbol
    extractSingleCurrencySymbol :: LedgerApiV2.Value -> LedgerApiV2.CurrencySymbol
    extractSingleCurrencySymbol val =
        let flattened = OnChainHelpers.flattenValueWithoutZeros val
            csList = nub [cs | (cs, _, _) <- flattened]
         in case csList of
                [cs] -> cs
                _ -> P.error "All elements must have the same currency symbol"

    -- Ensure each value has exactly one currency symbol and create redeemers for each value
    newRedeemersList :: [(LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer)]
    newRedeemersList =
        [ (LedgerApiV2.Minting (extractSingleCurrencySymbol val), redeemer)
        | (val, redeemer) <- valsAndRedeemers
        ]

    -- Create a map from the list of redeemers
    newRedeemersMap :: TxAssocMap.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
    newRedeemersMap = TxAssocMap.fromList newRedeemersList

    -- Get the existing redeemers from the context
    existingRedeemers :: TxAssocMap.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
    existingRedeemers = LedgerApiV2.txInfoRedeemers $ LedgerApiV2.scriptContextTxInfo sc

    -- Combine the new redeemers with the existing ones
    combinedRedeemers :: TxAssocMap.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
    combinedRedeemers = TxAssocMap.unionWith (\_ new -> new) existingRedeemers newRedeemersMap

    -- Combine all values into one
    combinedValue :: LedgerApiV2.Value
    combinedValue = LedgerAda.lovelaceValueOf 0 <> mconcat (map fst valsAndRedeemers)

addMintAndRedeemers :: [(LedgerApiV2.Value, LedgerApiV2.Redeemer)] -> LedgerApiV2.ScriptContext -> LedgerApiV2.ScriptContext
addMintAndRedeemers valsAndRedeemers sc =
    sc
        { LedgerApiV2.scriptContextTxInfo =
            (LedgerApiV2.scriptContextTxInfo sc)
                { LedgerApiV2.txInfoMint = combinedValue
                , LedgerApiV2.txInfoRedeemers = combinedRedeemers
                }
        }
  where
    -- Function to flatten the value and extract the single currency symbol
    extractSingleCurrencySymbol :: LedgerApiV2.Value -> LedgerApiV2.CurrencySymbol
    extractSingleCurrencySymbol val =
        let flattened = OnChainHelpers.flattenValueWithoutZeros val
            csList = nub [cs | (cs, _, _) <- flattened]
         in case csList of
                [cs] -> cs
                _ -> P.error "All elements must have the same currency symbol"

    -- Ensure each value has exactly one currency symbol and create redeemers for each value
    newRedeemersList :: [(LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer)]
    newRedeemersList =
        [ (LedgerApiV2.Minting (extractSingleCurrencySymbol val), redeemer)
        | (val, redeemer) <- valsAndRedeemers
        ]

    -- Create a map from the list of redeemers
    newRedeemersMap :: TxAssocMap.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
    newRedeemersMap = TxAssocMap.fromList newRedeemersList

    -- Get the existing redeemers from the context
    existingRedeemers :: TxAssocMap.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
    existingRedeemers = LedgerApiV2.txInfoRedeemers $ LedgerApiV2.scriptContextTxInfo sc

    -- Combine the new redeemers with the existing ones
    combinedRedeemers :: TxAssocMap.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
    combinedRedeemers = TxAssocMap.unionWith (\_ new -> new) existingRedeemers newRedeemersMap

    -- Combine all values into one, including the existing mint value
    combinedValue :: LedgerApiV2.Value
    combinedValue = LedgerApiV2.txInfoMint (LedgerApiV2.scriptContextTxInfo sc) <> mconcat (map fst valsAndRedeemers)

setValidRange :: LedgerApiV2.POSIXTimeRange -> LedgerApiV2.ScriptContext -> LedgerApiV2.ScriptContext
setValidRange range sc@LedgerApiV2.ScriptContext{..} =
    sc{LedgerApiV2.scriptContextTxInfo = scriptContextTxInfo{LedgerApiV2.txInfoValidRange = range}}

setSignatories :: [LedgerApiV2.PubKeyHash] -> LedgerApiV2.ScriptContext -> LedgerApiV2.ScriptContext
setSignatories signers sc =
    sc
        { LedgerApiV2.scriptContextTxInfo =
            (LedgerApiV2.scriptContextTxInfo sc){LedgerApiV2.txInfoSignatories = signers}
        }

-- | Add a signer to the transaction signers lists
addSignatory :: LedgerApiV2.PubKeyHash -> LedgerApiV2.ScriptContext -> LedgerApiV2.ScriptContext
addSignatory pkh sc =
    sc
        { LedgerApiV2.scriptContextTxInfo =
            txInfo{LedgerApiV2.txInfoSignatories = pkh : LedgerApiV2.txInfoSignatories txInfo}
        }
  where
    txInfo :: LedgerApiV2.TxInfo
    txInfo = LedgerApiV2.scriptContextTxInfo sc

setSpendPurpose :: Integer -> LedgerApiV2.ScriptContext -> LedgerApiV2.ScriptContext
setSpendPurpose consumeInputIndex ctx =
    let !txInfoInputs = LedgerApiV2.txInfoInputs $ LedgerApiV2.scriptContextTxInfo ctx
        !txInInfo = txInfoInputs !! consumeInputIndex
        purpose :: LedgerApiV2.ScriptPurpose
        purpose = LedgerApiV2.Spending $ LedgerApiV2.txInInfoOutRef txInInfo
     in ctx{LedgerApiV2.scriptContextPurpose = purpose}

--------------------------------------------------------------------------------

-- Custom function to check if the expected error is in the list of errors
assertContainsAnyOf :: (P.Eq a, P.Show a, GHC.HasCallStack) => [a] -> [a] -> Tasty.Assertion
assertContainsAnyOf xs searchAny =
    case searchAny of
        [] ->
            Tasty.assertBool
                ("Unexpected errors found. Expected none. Found: " ++ P.show xs)
                (null xs)
        _ ->
            Tasty.assertBool
                ("None of the expected errors found. Expected one of: " ++ P.show searchAny ++ ". Found: " ++ P.show xs)
                -- ++ ".\nLocation: " ++ GHC.prettyCallStack GHC.callStack
                (any (`P.elem` xs) searchAny)

resultContainsAnyOf :: (P.Eq a, P.Show a, GHC.HasCallStack) => [a] -> [a] -> QC.Property
resultContainsAnyOf xs searchAny =
    case searchAny of
        [] -> QC.property $ null xs || DebugTrace.trace ("Unexpected errors found. Expected none. Found: " ++ P.show xs ++ ".\nLocation: " ++ GHC.prettyCallStack GHC.callStack) False
        _ -> QC.property $ any (`P.elem` xs) searchAny || DebugTrace.trace ("None2 of the expected errors found. Expected one of: " ++ P.show searchAny ++ ". Found: " ++ P.show xs ++ ".\nLocation: " ++ GHC.prettyCallStack GHC.callStack) False

resultContainsAnyOfIfCondition :: (P.Eq a, P.Show a, GHC.HasCallStack) => [a] -> [a] -> Bool -> QC.Property
resultContainsAnyOfIfCondition xs searchAny ifCondition =
    if ifCondition
        then case searchAny of
            [] -> QC.property $ null xs || DebugTrace.trace ("Unexpected errors found. Expected none. Found: " ++ P.show xs ++ ".\nLocation: " ++ GHC.prettyCallStack GHC.callStack) False
            _ -> QC.property $ any (`P.elem` xs) searchAny || DebugTrace.trace ("None3 of the expected errors found. Expected one of: " ++ P.show searchAny ++ ". Found: " ++ P.show xs ++ ".\nLocation: " ++ GHC.prettyCallStack GHC.callStack) False
        else QC.property $ null xs || DebugTrace.trace ("Unexpected errors found. Expected none. Found: " ++ P.show xs ++ ".\nLocation: " ++ GHC.prettyCallStack GHC.callStack) False

-- Custom function to check budget and size conditions
assertBudgetAndSize :: GHC.HasCallStack =>
    P.Either LedgerApiV2.EvaluationError LedgerApiV2.ExBudget ->
    P.Either Integer Integer ->
    Integer -> -- Max memory
    Integer -> -- Max CPU
    Integer -> -- Max size
    Tasty.Assertion
assertBudgetAndSize eval_err eval_size maxMem' maxCPU' maxSize' = do
    (memVal, cpuVal) <- case eval_err of
        P.Left _ -> return (Nothing, Nothing)
        P.Right (LedgerApiV2.ExBudget (LedgerApiV2.ExCPU cpu) (LedgerApiV2.ExMemory mem)) -> do
            let memInt = TextRead.readMaybe (P.show mem) :: Maybe Integer
            let cpuInt = TextRead.readMaybe (P.show cpu) :: Maybe Integer
            return (memInt, cpuInt)

    sizeVal <- case eval_size of
        P.Left _ -> return Nothing
        P.Right size -> return (Just size)

    -- Helper to calculate percentage
    let percentageOverMax :: Integer -> Integer -> P.Double
        percentageOverMax current maxVal = (P.fromIntegral current P./ P.fromIntegral maxVal) P.* 100

    -- Generate a message string with all values
    let allValuesMsg = "Memory usage: current = " ++ maybe "N/A" P.show memVal ++ ", max = " ++ P.show maxMem'
                       ++ " (" ++ maybe "N/A" (P.show . (`percentageOverMax` maxMem')) memVal ++ "% over max), "
                       ++ "CPU usage: current = " ++ maybe "N/A" P.show cpuVal ++ ", max = " ++ P.show maxCPU'
                       ++ " (" ++ maybe "N/A" (P.show . (`percentageOverMax` maxCPU')) cpuVal ++ "% over max), "
                       ++ "Size usage: current = " ++ maybe "N/A" P.show sizeVal ++ ", max = " ++ P.show maxSize'
                       ++ " (" ++ maybe "N/A" (P.show . (`percentageOverMax` maxSize')) sizeVal ++ "% over max)."

    
    P.putStrLn allValuesMsg

    -- Perform assertions and include the detailed message in every one
    case memVal of
        Just mem -> Tasty.assertBool ("Memory usage exceeds limit.") (mem < maxMem')
        -- Just mem -> Tasty.assertBool (allValuesMsg ++ " Memory usage exceeds limit.") (mem < maxMem')
        _ -> return ()

    case cpuVal of
        Just cpu -> Tasty.assertBool ("CPU usage exceeds limit.") (cpu < maxCPU')
        -- Just cpu -> Tasty.assertBool (allValuesMsg ++ " CPU usage exceeds limit.") (cpu < maxCPU')
        _ -> return ()

    case sizeVal of
        Just size -> Tasty.assertBool ("Size usage exceeds limit.") (size < maxSize')
        -- Just size -> Tasty.assertBool (allValuesMsg ++ " Size usage exceeds limit.") (size < maxSize')
        _ -> return ()