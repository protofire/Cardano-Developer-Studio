{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module Helpers.Deploy where

--------------------------------------------------------------------------------2
-- External Imports
--------------------------------------------------------------------------------2

import qualified Cardano.Api                              as CardanoApi
import qualified Cardano.Api.Shelley                      as CardanoApiShelley
import qualified Codec.Serialise                          as CodecSerialise 
import qualified Data.Aeson                               as Aeson
import qualified Data.ByteString                          as BS
import qualified Data.ByteString.Lazy                     as BSL
import qualified Data.ByteString.Lazy.Char8               as BL8
import qualified Data.ByteString.Short                    as BSS
import qualified Data.Map                                 as Map
import qualified Flat                                     
import qualified Ledger.Address                           as LedgerAddress (Address)
import qualified Ledger.Scripts                           as LedgerScripts
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api                     as LedgerApiV2
import qualified PlutusCore                               as PLC
import qualified PlutusCore.DeBruijn                      as PLC
import qualified PlutusTx
import qualified PlutusTx.Builtins                        as TxBuiltins
import qualified PlutusTx.Code                            as PlutusTx
import qualified PlutusTx.Code                            as PlutusTxCode
import qualified PlutusTx.Coverage                        as Coverage
import           PlutusTx.Prelude                         hiding (unless)
import qualified Prelude                                  as P
import qualified System.FilePath                          as SystemFilePath
import qualified System.FilePath.Posix                    as SystemFilePathPosix
import qualified UntypedPlutusCore                        as UPLC

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Helpers.OffChain                  as OffChainHelpers

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

deployValidator :: P.String -> P.String -> LedgerApiV2.Validator -> P.IO (Either (CardanoApi.FileError ()) ())
deployValidator path file validator = do
    OffChainHelpers.writeValidator   path (file ++ ".plutus") validator

deployValidatorHash :: P.String -> P.String -> LedgerApiV2.ValidatorHash -> P.IO ()
deployValidatorHash path file hash = do
    OffChainHelpers.writePlutusDataToFile (path SystemFilePathPosix.</> file ++ ".hash") hash

deployValidatorAddress :: P.String -> P.String -> LedgerAddress.Address -> P.IO ()
deployValidatorAddress path file address = do
    _ <- OffChainHelpers.writeEncodedToFile (path SystemFilePathPosix.</> file ++ "-HEX.addr") address
    _ <- OffChainHelpers.writeFile (path SystemFilePathPosix.</> file ++ "-Mainnet.addr") $ OffChainHelpers.validatorAddrToAddrBech32Mainnet address
    _ <- OffChainHelpers.writeFile (path SystemFilePathPosix.</> file ++ "-Testnet.addr") $ OffChainHelpers.validatorAddrToAddrBech32Testnet address
    P.putStrLn $ "Addr: " ++ P.show address

--------------------------------------------------------------------------------2

deployMintingPolicy :: P.String -> P.String -> LedgerApiV2.MintingPolicy -> LedgerApiV2.CurrencySymbol -> P.IO ()
deployMintingPolicy path file policy curSymbol = do
    _ <- OffChainHelpers.writeMintingPolicy path (file ++ ".plutus") policy
    OffChainHelpers.writePlutusDataToFile (path SystemFilePathPosix.</> file ++ ".symbol") curSymbol
    P.putStrLn $ "Policy_CS: " ++ P.show curSymbol

--------------------------------------------------------------------------------2

readMintingPolicy :: P.String -> P.String -> P.IO (LedgerApiV2.MintingPolicy, LedgerApiV2.CurrencySymbol)
readMintingPolicy path file = do
    policy' <- OffChainHelpers.readMintingPolicy path (file ++ ".plutus")
    case policy' of
        Left _ -> P.putStrLn "Policy not found" >> error ()
        Right policy -> do
            let curSymbol = OffChainHelpers.getCurSymbolOfPolicy policy
            return (policy, curSymbol)


--------------------------------------------------------------------------------2
-- ESCRIBE Y LEE COMPILED CODE A ARCHIVO
--------------------------------------------------------------------------------2

-- TODO mover a offchain helpers
serializableToScript :: CodecSerialise.Serialise a => a -> CardanoApi.PlutusScript CardanoApi.PlutusScriptV2
serializableToScript = CardanoApiShelley.PlutusScriptSerialised . BSS.toShort . BSL.toStrict . CodecSerialise.serialise

-- Serialize compiled code
compiledCodeToScript :: PlutusTx.CompiledCode a -> CardanoApi.PlutusScript CardanoApi.PlutusScriptV2
compiledCodeToScript = serializableToScript . LedgerApiV2.fromCompiledCode

-- Create file with Plutus script
writeScriptToFile :: SystemFilePath.FilePath -> CardanoApi.PlutusScript CardanoApi.PlutusScriptV2 -> P.IO ()
writeScriptToFile filePath script =
  CardanoApiShelley.writeFileTextEnvelope filePath Nothing script >>= \case
    Left err -> P.print $ CardanoApiShelley.displayError err
    Right () -> P.putStrLn $ "Serialized script to: " ++ filePath

-- Create file with compiled code
writeCompiledCodeToJsonFile :: SystemFilePath.FilePath -> PlutusTx.CompiledCode a -> P.IO ()
writeCompiledCodeToJsonFile filePath = writeScriptToFile filePath . compiledCodeToScript

-----------------------------

-- Deserialize Plutus script
scriptFromSerializable :: CodecSerialise.Serialise a => CardanoApi.PlutusScript CardanoApi.PlutusScriptV2 -> a
scriptFromSerializable (CardanoApiShelley.PlutusScriptSerialised shortBS) =
    CodecSerialise.deserialise . BSL.fromStrict . BSS.fromShort $ shortBS

createDefaultCoverageIndex :: Coverage.CoverageIndex
createDefaultCoverageIndex = Coverage.CoverageIndex Map.empty

-- Helper function to convert PLC.DeBruijn to PLC.NamedDeBruijn
nameDeBruijn :: PLC.DeBruijn -> PLC.NamedDeBruijn
nameDeBruijn = PLC.fakeNameDeBruijn
-- nameDeBruijn (PLC.DeBruijn ix) = PLC.NamedDeBruijn (PLC.NamedDeBruijn (BS.pack "debruijn") ix)

termDeBruijnToNamed :: UPLC.Term PLC.DeBruijn PLC.DefaultUni PLC.DefaultFun () -> UPLC.Term PLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun ()
termDeBruijnToNamed = UPLC.termMapNames nameDeBruijn

-- Helper function to convert UPLC.Program PLC.DeBruijn to UPLC.Program PLC.NamedDeBruijn
programDeBruijnToNamed :: UPLC.Program PLC.DeBruijn PLC.DefaultUni PLC.DefaultFun () -> UPLC.Program PLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun ()
programDeBruijnToNamed (UPLC.Program a b term) = UPLC.Program a b (termDeBruijnToNamed term)

-- Deserialize Plutus script to compiled code
compiledCodeFromScript :: CardanoApi.PlutusScript CardanoApi.PlutusScriptV2 -> PlutusTx.CompiledCode a
compiledCodeFromScript script =
  case scriptFromSerializable script of
    LedgerScripts.Script program ->   PlutusTx.DeserializedCode (programDeBruijnToNamed program) Nothing createDefaultCoverageIndex

-- Read Plutus script from file
readScriptFromJsonFile :: SystemFilePath.FilePath -> P.IO (Either (CardanoApiShelley.FileError
                         CardanoApiShelley.TextEnvelopeError) (CardanoApi.PlutusScript CardanoApi.PlutusScriptV2))
readScriptFromJsonFile = CardanoApiShelley.readFileTextEnvelope (CardanoApi.AsPlutusScript CardanoApi.AsPlutusScriptV2)

-- Read compiled code from file
readCompiledCodeFromJsonFile :: SystemFilePath.FilePath -> P.IO (PlutusTx.CompiledCode a)
readCompiledCodeFromJsonFile filePath = do
    result <- readScriptFromJsonFile filePath
    P.return $ case result of
        Left err     -> P.error $ "readCompiledCodeFromJsonFile error: " ++ P.show err -- Throws an error
        Right script -> compiledCodeFromScript script

--------------------------------------------------------------------------------2




-- Function to parse JSON string to TextView
jsonStringToTextEnvelope :: P.String -> CardanoApiShelley.TextEnvelope
jsonStringToTextEnvelope jsonWithCborHex =
    case Aeson.eitherDecode (BL8.pack jsonWithCborHex) of
        Left err       -> P.error $ "jsonStringToTextEnvelope error: " ++ P.show err -- Throws an error
        Right textView -> textView

readScriptFromJsonString :: P.String -> CardanoApi.PlutusScript CardanoApi.PlutusScriptV2
readScriptFromJsonString jsonWithCborHex = do
        let textEnvelope =jsonStringToTextEnvelope jsonWithCborHex
        case CardanoApiShelley.deserialiseFromTextEnvelope (CardanoApi.AsPlutusScript CardanoApi.AsPlutusScriptV2) textEnvelope of
            Left err     -> P.error $ "readScriptFromJsonString error: " ++ P.show err -- Throws an error
            Right script -> script

readCompiledCodeFromJsonString  ::  P.String  -> PlutusTx.CompiledCode a
readCompiledCodeFromJsonString  jsonWithCborHex = do
    let
        result = readScriptFromJsonString jsonWithCborHex
    compiledCodeFromScript result


--------------------------------------------------------------------------------2
-- SAME AS ABOVE BUT USING FLAT
--------------------------------------------------------------------------------2

-- Create file with compiled code
writeCompiledCodeToBinaryFile
  :: SystemFilePath.FilePath
  -> PlutusTxCode.CompiledCode a
  -> P.IO ()
writeCompiledCodeToBinaryFile filePath code =
  let plcProgram = PlutusTxCode.getPlc code
      serialized = Flat.flat plcProgram
  in do
    BS.writeFile filePath serialized
    P.putStrLn $ "writeCompiledCodeToBinaryFile in: " ++ P.show filePath

readCompiledCodeFromBinaryFile
  :: SystemFilePath.FilePath
  -> P.IO (PlutusTxCode.CompiledCode a)
readCompiledCodeFromBinaryFile filePath = do
    serialized <- BS.readFile filePath
    case Flat.unflat serialized of
        Left err   -> P.error $ "readCompiledCodeFromBinaryFile error: " ++ P.show err -- Throws an error
        Right code -> return code

--------------------------------------------------------------------------------2

compiledCodeToProgramTerm :: PlutusTxCode.CompiledCode a -> UPLC.Term UPLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun ()
compiledCodeToProgramTerm = UPLC._progTerm . PlutusTxCode.getPlc

--------------------------------------------------------------------------------2

{-# INLINEABLE applyPlutononyToValidatorCode #-}
applyPlutononyToValidatorCode :: PlutusTxCode.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ()) -> LedgerApiV2.Validator
applyPlutononyToValidatorCode code  =
    Plutonomy.optimizeUPLC $
        Plutonomy.validatorToPlutus $
            Plutonomy.mkValidatorScript code

{-# INLINEABLE applyPlutononyToMintingPolicyCode #-}
applyPlutononyToMintingPolicyCode :: PlutusTxCode.CompiledCode (BuiltinData -> BuiltinData -> ()) -> LedgerApiV2.MintingPolicy
applyPlutononyToMintingPolicyCode code  =
    Plutonomy.optimizeUPLC $
        Plutonomy.mintingPolicyToPlutus $
            Plutonomy.mkMintingPolicyScript code

{-# INLINEABLE applyPlutononyToValidator #-}
applyPlutononyToValidator ::  (BuiltinData -> BuiltinData -> BuiltinData -> ()) -> LedgerApiV2.Validator
applyPlutononyToValidator validator  =
    Plutonomy.optimizeUPLC $
        Plutonomy.validatorToPlutus $
            Plutonomy.mkValidatorScript $$( PlutusTx.compile [|| validator ||])
    -- LedgerApiV2.mkValidatorScript $$( PlutusTx.compile [|| validator ||])

{-# INLINEABLE applyPlutononyToMintingPolicy #-}
applyPlutononyToMintingPolicy ::  (BuiltinData -> BuiltinData -> ()) -> LedgerApiV2.MintingPolicy
applyPlutononyToMintingPolicy policy  =
    Plutonomy.optimizeUPLC $
        Plutonomy.mintingPolicyToPlutus $
            Plutonomy.mkMintingPolicyScript $$( PlutusTx.compile [|| policy ||])

--------------------------------------------------------------------------------2

dataToBuiltinData :: PlutusTx.Data -> BuiltinData
dataToBuiltinData  = TxBuiltins.dataToBuiltinData

stringHexToBuiltinData :: P.String -> BuiltinData
stringHexToBuiltinData params  =
    TxBuiltins.mkB $ OffChainHelpers.builtinByteStringFromHexString $ OffChainHelpers.bytesFromHex $ OffChainHelpers.stringToStrictByteString params



intToBuiltinData :: Integer -> BuiltinData
intToBuiltinData   = TxBuiltins.mkI

paramsIndexToBuiltinData :: [P.String] -> Integer -> BuiltinData
paramsIndexToBuiltinData params index =
    let (paramType, paramValue) = P.span (P./= ':') (params!!index) in
    case paramType of
        "string" ->  stringHexToBuiltinData $ P.drop 1 paramValue
        "bigint" ->  intToBuiltinData $ P.read $ P.drop 1 paramValue
        _        -> P.error "Unsupported type"

paramsToDataShow :: [P.String] -> Integer -> P.IO ()
paramsToDataShow params index =
    let (paramType, paramValue) = P.span (P./= ':') (params!!index) in
    case paramType of
        "string" -> P.putStrLn $ P.show index ++ "params string: " ++ P.drop 1 paramValue
        "bigint" ->  P.putStrLn $ P.show index ++ "params bigint: " ++ P.drop 1 paramValue
        _        -> P.error "Unsupported type"

optimizeValidator :: SystemFilePath.FilePath -> SystemFilePath.FilePath -> [P.String] -> P.IO ()
optimizeValidator contractFilePath optimizedContractSaveFileName scriptParams = do
    let dirPath = SystemFilePath.takeDirectory contractFilePath
    P.putStrLn $ "readCompiledCodeFromBinaryFile: " ++ contractFilePath

    compileCodeWithParams <- case length scriptParams of
        0 -> readCompiledCodeFromBinaryFile @(BuiltinData -> BuiltinData -> BuiltinData -> ()) contractFilePath
        1 -> do
            code <- readCompiledCodeFromBinaryFile @(BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()) contractFilePath
            paramsToDataShow scriptParams 0
            return $ code `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 0)
        2 -> do
            code <- readCompiledCodeFromBinaryFile @(BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()) contractFilePath
            _ <- paramsToDataShow scriptParams 0
            _ <- paramsToDataShow scriptParams 1
            return $ code
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 0)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 1)
        3 -> do
            code <- readCompiledCodeFromBinaryFile @(BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()) contractFilePath
            _ <- paramsToDataShow scriptParams 0
            _ <- paramsToDataShow scriptParams 1
            _ <- paramsToDataShow scriptParams 2

            return $ code
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 0)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 1)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 2)
        4 -> do
            code <- readCompiledCodeFromBinaryFile @(BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()) contractFilePath
            _ <- paramsToDataShow scriptParams 0
            _ <- paramsToDataShow scriptParams 1
            _ <- paramsToDataShow scriptParams 2
            _ <- paramsToDataShow scriptParams 3

            return $ code
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 0)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 1)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 2)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 3)
        5 -> do
            code <- readCompiledCodeFromBinaryFile @(BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()) contractFilePath
            _ <- paramsToDataShow scriptParams 0
            _ <- paramsToDataShow scriptParams 1
            _ <- paramsToDataShow scriptParams 2
            _ <- paramsToDataShow scriptParams 3
            _ <- paramsToDataShow scriptParams 4
            return $ code
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 0)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 1)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 2)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 3)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 4)
        _ -> P.error "Too Many Params"

    P.putStrLn "applyPlutononyToValidatorCode..."
    let validator = applyPlutononyToValidatorCode compileCodeWithParams
    P.putStrLn $ "writeValidator: " ++ (dirPath SystemFilePathPosix.</> optimizedContractSaveFileName)
    _ <- OffChainHelpers.writeValidator dirPath optimizedContractSaveFileName validator
    return ()

--------------------------------------------------------------------------------2

optimizeMintingPolicy :: SystemFilePath.FilePath -> SystemFilePath.FilePath -> [P.String] -> P.IO ()
optimizeMintingPolicy contractFilePath optimizedContractSaveFileName scriptParams = do
    let dirPath = SystemFilePath.takeDirectory contractFilePath
    P.putStrLn $ "readCompiledCodeFromBinaryFile: " ++ contractFilePath

    compileCodeWithParams <- case length scriptParams of
        0 -> readCompiledCodeFromBinaryFile @(BuiltinData ->  BuiltinData -> ()) contractFilePath
        1 -> do
            code <- readCompiledCodeFromBinaryFile @(BuiltinData ->  BuiltinData -> BuiltinData -> ()) contractFilePath
            paramsToDataShow scriptParams 0
            return $ code `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 0)
        2 -> do
            code <- readCompiledCodeFromBinaryFile @(BuiltinData ->  BuiltinData -> BuiltinData -> BuiltinData -> ()) contractFilePath
            _ <- paramsToDataShow scriptParams 0
            _ <- paramsToDataShow scriptParams 1
            return $ code
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 0)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 1)
        3 -> do
            code <- readCompiledCodeFromBinaryFile @(BuiltinData ->  BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()) contractFilePath
            _ <- paramsToDataShow scriptParams 0
            _ <- paramsToDataShow scriptParams 1
            _ <- paramsToDataShow scriptParams 2

            return $ code
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 0)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 1)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 2)
        4 -> do
            code <- readCompiledCodeFromBinaryFile @(BuiltinData ->  BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()) contractFilePath
            _ <- paramsToDataShow scriptParams 0
            _ <- paramsToDataShow scriptParams 1
            _ <- paramsToDataShow scriptParams 2
            _ <- paramsToDataShow scriptParams 3

            return $ code
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 0)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 1)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 2)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 3)
        5 -> do
            code <- readCompiledCodeFromBinaryFile @(BuiltinData -> BuiltinData -> BuiltinData ->  BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()) contractFilePath
            _ <- paramsToDataShow scriptParams 0
            _ <- paramsToDataShow scriptParams 1
            _ <- paramsToDataShow scriptParams 2
            _ <- paramsToDataShow scriptParams 3
            _ <- paramsToDataShow scriptParams 4
            return $ code
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 0)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 1)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 2)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 3)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 4)
        _ -> P.error "Too Many Params"

    P.putStrLn "applyPlutononyToMintingPolicyCode..."
    let policy = applyPlutononyToMintingPolicyCode compileCodeWithParams
    P.putStrLn $ "writeMintingPolicy: " ++ (dirPath SystemFilePathPosix.</> optimizedContractSaveFileName)
    _ <- OffChainHelpers.writeMintingPolicy dirPath optimizedContractSaveFileName policy
    return ()

--------------------------------------------------------------------------------2

