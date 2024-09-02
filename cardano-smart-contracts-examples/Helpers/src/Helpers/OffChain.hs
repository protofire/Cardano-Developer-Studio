{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication" -}
--------------------------------------------------------------------------------2

module Helpers.OffChain where

--------------------------------------------------------------------------------2
-- External Imports
--------------------------------------------------------------------------------

import qualified Cardano.Api                                     as CardanoApi
import qualified Cardano.Api.Shelley                             as ApiShelley
import qualified Cardano.Codec.Bech32.Prefixes                   as Bench32Prefixes (addr, addr_test)
import qualified Cardano.Crypto.Hash.Class                       as CryptoHashClass (hashToBytes)
import qualified Cardano.Ledger.BaseTypes                        as LedgerBaseTypes (certIxToInt, txIxToInt)
import qualified Cardano.Ledger.Credential                       as LedgerCredential
import qualified Cardano.Ledger.Crypto                           as LedgerCrypto (StandardCrypto)
import qualified Cardano.Ledger.Hashes                           as LedgerHashes (ScriptHash (..))
import qualified Cardano.Ledger.Keys                             as LedgerKeys (KeyHash (..))
import qualified Codec.Binary.Bech32                             as CodecBinaryBech32
import qualified Codec.Serialise                                 as CodecSerialise
import qualified Control.Lens                                    as ControlLens
import qualified Data.Aeson                                      as DataAeson (decode, encode, FromJSON)
import qualified Data.ByteString                                 as DataByteString
import qualified Data.ByteString.Base16                          as DataByteStringBase16
import qualified Data.ByteString.Base64.Lazy                     as DataByteStringLazyBase64
import qualified Data.ByteString.Char8                           as DataByteStringChar8
import qualified Data.ByteString.Lazy                            as DataByteStringLazy
import qualified Data.ByteString.Lazy.Char8                      as DataByteStringLazyChar8
import qualified Data.ByteString.Short                           as DataByteStringShort
import qualified Data.ByteString.UTF8                            as DataByteStringUTF8
import qualified Data.Char                                       as DataChar
import qualified Data.List                                       as DataList (isInfixOf, nub)
import qualified Data.Map                                        as DataMap
import qualified Data.Maybe                                      as DataMaybe (fromJust, fromMaybe)
import qualified Data.String                                     as DataString
import qualified Data.Text                                       as DataText
import qualified Data.Text.Encoding                              as DataTextEncoding
import qualified Data.Text.Lazy                                  as DataTextLazy
import qualified Data.Text.Lazy.Encoding                         as DataTextLazyEncoding
import qualified Data.Void                                       as DataVoid (Void)
import qualified Ledger
import qualified Ledger.Ada                                      as LedgerAda
import qualified Ledger.Address                                  as LedgerAddress
import qualified Ledger.Bytes                                    as LedgerBytes
import qualified Ledger.Constraints                              as LedgerConstraints
import qualified Ledger.Constraints.TxConstraints                as LedgerTxConstraints
import qualified Ledger.Crypto                                   as Crypto
import qualified Ledger.Tx                                       as LedgerTx
import qualified Ledger.Tx.CardanoAPI                            as LedgerTxCardanoAPI
import qualified Ledger.Value                                    as LedgerValue
import qualified Plutus.ChainIndex.Types                         as ChainIndexTypes (RollbackState (Committed, TentativelyConfirmed, Unknown), TxStatus, TxValidity (TxInvalid, TxValid, UnknownValidity))
import qualified Plutus.Contract                                 as PlutusContract
import qualified Plutus.Script.Utils.V1.Typed.Scripts.Validators as UtilsTypedScriptsValidatorsV1 (DatumType, RedeemerType)
import qualified Plutus.Script.Utils.V2.Scripts                  as UtilsScriptsV2
import qualified Plutus.V1.Ledger.Api                            as LedgerApiV1
import qualified Plutus.V1.Ledger.Credential                     as LedgerCredentialV1
import qualified Plutus.V1.Ledger.Scripts                        as LedgerScriptsV1
import qualified Plutus.V1.Ledger.Value                          as LedgerValueV1 (TokenName (..))
import qualified Plutus.V2.Ledger.Api                            as LedgerApiV2
import qualified PlutusTx
import qualified PlutusTx.Builtins                               as TxBuiltins (toBuiltin)
import qualified PlutusTx.Builtins.Class                         as TxBuiltinsClass
import qualified PlutusTx.Builtins.Internal                      as TxBuiltinsInternal (BuiltinByteString (..))
import           PlutusTx.Prelude
import qualified PlutusTx.Ratio                                  as TxRatio
import qualified Prelude                                         as P
import qualified System.Directory                                as SystemDirectory
import qualified System.FilePath.Posix                           as SystemFilePathPosix
import qualified Text.Hex                                        as TextHex
import qualified Text.Printf                                     as TextPrintf (printf)
import qualified Wallet.Emulator.Wallet                          as WalletEmulator (WalletId (..))
import qualified Wallet.Types                                    as WalletTypes (ContractInstanceId (..))

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Helpers.OnChain                          as OnChainHelpers

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

addSeparatorEachN :: a -> P.Integer -> [a] -> [a]
addSeparatorEachN sep n xs = reverse $ addSepHelper n (reverse xs)
  where
    addSepHelper _ [] = []
    addSepHelper _ [x] = [x]
    addSepHelper count (x:xs')
      | count == 1 = x : sep : addSepHelper n xs'
      | otherwise = x : addSepHelper (count - 1) xs'

--------------------------------------------------------------------------------2

displayRational :: Integer -> Rational -> P.String
displayRational len rat = (if num < 0 then "-" else "")  ++ P.shows d ("." ++ take len (go next))
    where
        (d, next) = abs num `quotRem` den
        num = TxRatio.numerator rat
        den = TxRatio.denominator rat
    ------------------
        go 0 = ""
        go x = let (d', next') = (10 * x) `quotRem` den
               in P.shows d' (go next')


--------------------------------------------------------------------------------2

removeDuplicates :: (P.Eq a) => [a] -> [a]
removeDuplicates = DataList.nub

--------------------------------------------------------------------------------2

stringContains :: P.String -> P.String -> Bool
stringContains subString mainString = subString `DataList.isInfixOf` mainString

--------------------------------------------------------------------------------2

stringToStrictByteString :: P.String -> DataByteString.ByteString
stringToStrictByteString = DataTextEncoding.encodeUtf8 . DataText.pack
stringToLazyByteString :: P.String -> DataByteStringLazy.ByteString
stringToLazyByteString = DataTextLazyEncoding.encodeUtf8 . DataTextLazy.pack
stringToStrictText :: P.String -> DataText.Text
stringToStrictText = DataText.pack
stringToLazyText :: P.String -> DataTextLazy.Text
stringToLazyText = DataTextLazy.pack

--------------------------------------------------------------------------------2

strictByteStringToString :: DataByteString.ByteString -> P.String
strictByteStringToString = DataText.unpack . DataTextEncoding.decodeUtf8
strictByteStringToLazyByteString :: DataByteString.ByteString -> DataByteStringLazy.ByteString
strictByteStringToLazyByteString = DataByteStringLazy.fromChunks . return
strictByteStringToStrictText :: DataByteString.ByteString -> DataText.Text
strictByteStringToStrictText = DataTextEncoding.decodeUtf8
strictByteStringToLazyText :: DataByteString.ByteString -> DataTextLazy.Text
strictByteStringToLazyText = DataTextLazy.fromStrict . DataTextEncoding.decodeUtf8

--------------------------------------------------------------------------------2

lazyByteStringToString :: DataByteStringLazy.ByteString -> P.String
lazyByteStringToString = DataTextLazy.unpack . DataTextLazyEncoding.decodeUtf8
lazyByteStringToStrictByteString :: DataByteStringLazy.ByteString -> DataByteString.ByteString
lazyByteStringToStrictByteString = DataByteString.concat . DataByteStringLazy.toChunks
lazyByteStringToStrictText :: DataByteStringLazy.ByteString -> DataText.Text
lazyByteStringToStrictText = DataTextEncoding.decodeUtf8 . DataByteString.concat . DataByteStringLazy.toChunks
lazyByteStringToLazyText :: DataByteStringLazy.ByteString -> DataTextLazy.Text
lazyByteStringToLazyText = DataTextLazyEncoding.decodeUtf8


lazyByteStringToBase64String :: DataByteStringLazy.ByteString -> P.String
lazyByteStringToBase64String = DataByteStringLazyChar8.unpack . DataByteStringLazyBase64.encode
base64StringToLazyByteString :: P.String -> DataByteStringLazy.ByteString
base64StringToLazyByteString str =
  case DataByteStringLazyBase64.decode (DataByteStringLazyChar8.pack str) of
    Left err -> P.error err
    Right bs -> bs
--------------------------------------------------------------------------------2

strictTextToString :: DataText.Text -> P.String
strictTextToString = DataText.unpack
strictTextToStrictByteString :: DataText.Text -> DataByteString.ByteString
strictTextToStrictByteString = DataTextEncoding.encodeUtf8
strictTextToLazyByteString :: DataText.Text -> DataByteStringLazy.ByteString
strictTextToLazyByteString = DataByteStringLazy.fromChunks . return . DataTextEncoding.encodeUtf8
strictTextToLazyText :: DataText.Text -> DataTextLazy.Text
strictTextToLazyText = DataTextLazy.fromStrict

--------------------------------------------------------------------------------2

lazyTextToString :: DataTextLazy.Text -> P.String
lazyTextToString = DataTextLazy.unpack
lazyTextTostrictByteString :: DataTextLazy.Text -> DataByteString.ByteString
lazyTextTostrictByteString = DataTextEncoding.encodeUtf8 . DataTextLazy.toStrict
lazyTextToLazyByteString :: DataTextLazy.Text -> DataByteStringLazy.ByteString
lazyTextToLazyByteString = DataTextLazyEncoding.encodeUtf8
lazyTextToStrictText :: DataTextLazy.Text -> DataText.Text
lazyTextToStrictText = DataTextLazy.toStrict

--------------------------------------------------------------------------------2

stringToBuiltinByteString :: P.String -> TxBuiltinsInternal.BuiltinByteString
stringToBuiltinByteString = TxBuiltinsInternal.BuiltinByteString . stringToStrictByteString

-- builtinByteStringToString :: TxBuiltinsInternal.BuiltinByteString -> P.String
-- builtinByteStringToString = TxBuiltinsInternal.BuiltinByteString

--------------------------------------------------------------------------------2

-- | Treat string of hexidecimal bytes literally, without encoding. Useful for hashes.
-- bytesFromHex "00" == "\NUL" ///"\x00"
-- bytesFromHex "01" == "\SOH" ///"\x01"
bytesFromHex :: DataByteString.ByteString -> DataByteString.ByteString
bytesFromHex = LedgerBytes.bytes . fromEither . LedgerBytes.fromHex
  where
    fromEither (Left e)  = P.error P.$ P.show e
    fromEither (Right b) = b

stringFromHexString :: P.String -> DataByteString.ByteString
stringFromHexString =  DataMaybe.fromJust . TextHex.decodeHex . stringToStrictText

builtinByteStringFromHexString :: DataByteString.ByteString -> TxBuiltinsInternal.BuiltinByteString
builtinByteStringFromHexString =  TxBuiltinsClass.toBuiltin

builtinByteStringToHexString :: TxBuiltinsInternal.BuiltinByteString -> DataByteString.ByteString
builtinByteStringToHexString =  TxBuiltinsClass.fromBuiltin

-- hexStringFromBytes "\NUL" == "00"
-- hexStringFromBytes "\SOH" == "01"
hexStringFromBytes :: P.String -> P.String
hexStringFromBytes  = P.show . LedgerBytes.fromBytes . stringToStrictByteString

hexStringFromBytesString :: DataByteString.ByteString -> P.String
hexStringFromBytesString  = P.show . LedgerBytes.fromBytes

stringToHex :: P.String -> P.String
stringToHex str = P.show $ DataByteStringBase16.encode $ DataByteString.pack $ map (P.fromIntegral . DataChar.ord) str

--------------------------------------------------------------------------------2

redeemerToBuiltinData :: forall d. LedgerApiV2.ToData d => d -> LedgerApiV2.Redeemer
redeemerToBuiltinData redeemer = LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData redeemer

--------------------------------------------------------------------------------2

dataToScriptData :: PlutusTx.Data -> CardanoApi.ScriptData
dataToScriptData (PlutusTx.Constr n xs) = CardanoApi.ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (PlutusTx.Map xs)      = CardanoApi.ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (PlutusTx.List xs)     = CardanoApi.ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (PlutusTx.I n)         = CardanoApi.ScriptDataNumber n
dataToScriptData (PlutusTx.B bs)        = CardanoApi.ScriptDataBytes bs

scriptDataToData :: CardanoApi.ScriptData -> PlutusTx.Data
scriptDataToData (CardanoApi.ScriptDataConstructor int xs) = PlutusTx.Constr int [scriptDataToData x | x <- xs]
scriptDataToData (CardanoApi.ScriptDataMap kvs)            = PlutusTx.Map [(scriptDataToData k, scriptDataToData v) | (k, v) <- kvs]
scriptDataToData (CardanoApi.ScriptDataList xs)            = PlutusTx.List [scriptDataToData x | x <- xs]
scriptDataToData (CardanoApi.ScriptDataNumber n)           = PlutusTx.I n
scriptDataToData (CardanoApi.ScriptDataBytes bs)           = PlutusTx.B bs

--------------------------------------------------------------------------------2

writeFile :: P.FilePath -> DataByteStringLazy.ByteString -> P.IO ()
writeFile = DataByteStringLazy.writeFile

readFile :: P.String -> P.IO DataByteStringLazy.ByteString
readFile = DataByteStringLazy.readFile

--------------------------------------------------------------------------------2

writeEncodedToFile :: ApiShelley.ToJSON a => P.FilePath -> a -> P.IO ()
writeEncodedToFile filepath dataToWrite =
    writeFile filepath (DataAeson.encode dataToWrite)

-- Generic function to read and decode JSON-encoded data from a file
readDecodedFromFile :: DataAeson.FromJSON a => P.FilePath -> P.IO a
readDecodedFromFile filePath = do
    fileExists <- SystemDirectory.doesFileExist filePath
    if fileExists
        then do
            fileContent <- DataByteStringLazy.readFile filePath
            case DataAeson.decode fileContent of
                Just dataValue -> return dataValue
                Nothing -> P.error "Could not decode JSON"
        else P.error "File does not exist"
        
--------------------------------------------------------------------------------2

-- lee datums o redeemers desde archivos exportados con OffChainHelpers.writeEncodedToFile
-- :set -XOverloadedStrings -XTypeApplications
-- readFileDecodedAsDatum "datum-HEX.json"
-- contents of file: {"getDatum":"d87a9fd8799fd8799f581c9925b87688572c90085d2a6bcaa7e8f4d1e631fc18a4439ea998ce3f5820d793c0edc6b088fbd76e749658a27a7cad242ec4fb63f9547bffccf2bfcbda41ff9fd8799f581cabfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e1a002dc96600ffff800000ffff"}
-- readFileDecodedAsRedeemer
-- contents of file: {"getRedeemer":"d87a9fd8799fd8799f581c9925b87688572c90085d2a6bcaa7e8f4d1e631fc18a4439ea998ce3f5820d793c0edc6b088fbd76e749658a27a7cad242ec4fb63f9547bffccf2bfcbda41ff9fd8799f581cabfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e1a002dc96600ffff800000ffff"}

readFileDecodedAsDatum :: P.String -> P.IO LedgerApiV2.Datum
readFileDecodedAsDatum filepath = do
    !file <- readFile filepath
    readStringDecodedAsDatum (lazyByteStringToString file)

readFileDecodedAsRedeemer :: P.String -> P.IO LedgerApiV2.Redeemer
readFileDecodedAsRedeemer filepath = do
    !file <- readFile filepath
    readStringDecodedAsRedeemer (lazyByteStringToString file)

--------------------------------------------------------------------------------2

-- lee cualquier tipo de datum o redeemer desde archivos exportados con OffChainHelpers.writeEncodedToFile
-- :set -XOverloadedStrings -XTypeApplications
-- @TIPO puede ser @LedgerApiV2.Datum o cualquiera creado internamente
-- readFileDecodedAsTypedDatum @LedgerApiV2.Datum "datum-HEX.json"
-- contents of file: {"getDatum":"d87a9fd8799fd8799f581c9925b87688572c90085d2a6bcaa7e8f4d1e631fc18a4439ea998ce3f5820d793c0edc6b088fbd76e749658a27a7cad242ec4fb63f9547bffccf2bfcbda41ff9fd8799f581cabfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e1a002dc96600ffff800000ffff"}

readFileDecodedAsTypedDatum :: forall a. (LedgerApiV2.UnsafeFromData a, P.Show a) => P.String -> P.IO a
readFileDecodedAsTypedDatum filepath = do
    !file <- readFile filepath
    readStringDecodedAsTypedDatum @a (lazyByteStringToString file)

readFileDecodedAsTypedRedeemer :: forall a. (LedgerApiV2.UnsafeFromData a, P.Show a) => P.String -> P.IO a
readFileDecodedAsTypedRedeemer filepath = do
    !file <- readFile filepath
    readStringDecodedAsTypedRedeemer @a (lazyByteStringToString file)

--------------------------------------------------------------------------------2

-- :set -XOverloadedStrings -XTypeApplications
-- readStringDecodedAsDatum "{\"getDatum\":\"d87a9fd8799fd8799f581c9925b87688572c90085d2a6bcaa7e8f4d1e631fc18a4439ea998ce3f5820d793c0edc6b088fbd76e749658a27a7cad242ec4fb63f9547bffccf2bfcbda41ff9fd8799f581cabfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e1a002dc96600ffff800000ffff\"}"
-- readStringDecodedAsDatum "{\"getDatum\":\"d905019fd8799f0180ffff\"}"
-- readStringDecodedAsDatum "{\"getDatum\":\"d905019fd8799f0180581c786139173b79832ace8b3a55c04d8e43586724fcd497cd286bf27345581c3afcf1924a5ee2cfe4c11c642dcdcf97ed7436648f431175ba3664a10000ffff\"}"
-- readStringDecodedAsDatum "{\"getDatum\":\"d8799fd8799f0180581c786139173b79832ace8b3a55c04d8e43586724fcd497cd286bf27345581c3afcf1924a5ee2cfe4c11c642dcdcf97ed7436648f431175ba3664a100009f581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cabff9fd8799f1901bc183719029affffffff\"}"
-- readStringDecodedAsDatum "{\"getDatum\":\"d8799fd8799f0180581c786139173b79832ace8b3a55c04d8e43586724fcd497cd286bf27345581c3afcf1924a5ee2cfe4c11c642dcdcf97ed7436648f431175ba3664a100009f581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cabff9fd8799f030405ffffd8799f000000ff0000d8799f000000ffd8799f000000ffd8799f000000ff0000008000ffff\"}"


readStringDecodedAsDatum :: P.String -> P.IO LedgerApiV1.Datum
readStringDecodedAsDatum encoded = do
    P.putStrLn $ "encoded: " ++ P.show encoded
    case DataAeson.decode (stringToLazyByteString encoded) :: Maybe LedgerApiV1.Datum of
        Nothing      -> P.error $ "Could not decode As Datum " ++ encoded
        Just decoded -> return decoded

-- :set -XOverloadedStrings -XTypeApplications
-- readStringDecodedAsRedeemer "{\"getRdeemer\":\"d87a9fd8799fd8799f581c9925b87688572c90085d2a6bcaa7e8f4d1e631fc18a4439ea998ce3f5820d793c0edc6b088fbd76e749658a27a7cad242ec4fb63f9547bffccf2bfcbda41ff9fd8799f581cabfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e1a002dc96600ffff800000ffff\"}"

readStringDecodedAsRedeemer :: P.String -> P.IO LedgerApiV2.Redeemer
readStringDecodedAsRedeemer encoded = do
    P.putStrLn $ "encoded: " ++ P.show encoded
    case DataAeson.decode (stringToLazyByteString encoded) :: Maybe LedgerApiV2.Redeemer of
        Nothing      -> P.error $ "Could not decode As Redeemer " ++ encoded
        Just decoded -> return decoded

--------------------------------------------------------------------------------2

-- :set -XOverloadedStrings -XTypeApplications
-- @TIPO puede ser @LedgerApiV2.Datum o cualquiera creado internamente
-- readStringDecodedAsTypedDatum @TIPO "{\"getDatum\":\"d87b9fd8799f581cabfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16ed8799f581c026c6fbd39eae18dabbe11021b5b9901635e015bf3df6f83798fde09ff011b000001869f0b4afc0000d87a801a003e81cdffff\"}"
-- readStringDecodedAsTypedDatum @LedgerApiV2.Datum "{\"getDatum\":\"d87b9fd8799f581cabfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16ed8799f581c026c6fbd39eae18dabbe11021b5b9901635e015bf3df6f83798fde09ff011b000001869f0b4afc0000d87a801a003e81cdffff\"}"

readStringDecodedAsTypedDatum :: forall a. (LedgerApiV2.UnsafeFromData a, P.Show a) => P.String -> P.IO a
readStringDecodedAsTypedDatum stringCbor = do
    !raw <- readStringDecodedAsDatum stringCbor
    P.putStrLn $ "Raw: " ++ P.show raw
    let !result = LedgerApiV2.unsafeFromBuiltinData @a (LedgerApiV2.getDatum raw)
    P.putStrLn $ "Result: " ++ P.show result
    return result

--------------------------------------------------------------------------------2

-- :set -XOverloadedStrings -XTypeApplications

-- @TIPO puede ser LedgerApiV2.Redeemer o cualquiera creado internamente
-- readEncodedStringAsRedeemer @TIPO "{\"getRedeemer\":\"d87a9fd8799fd8799f581cabfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16ed8799fd8799f43aaccffff0affffffff\"}"
-- readEncodedStringAsRedeemer @LedgerApiV2.Redeemer  IPO "{\"getRedeemer\":\"d87a9fd8799fd8799f581cabfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16ed8799fd8799f43aaccffff0affffffff\"}"

readStringDecodedAsTypedRedeemer :: forall a. (LedgerApiV2.UnsafeFromData a, P.Show a) => P.String -> P.IO a
readStringDecodedAsTypedRedeemer stringCbor = do
    !raw <- readStringDecodedAsRedeemer stringCbor
    P.putStrLn $ "Raw: " ++ P.show raw
    let !result = LedgerApiV2.unsafeFromBuiltinData @a (LedgerApiV2.getRedeemer raw)
    P.putStrLn $ "Result: " ++ P.show result
    return result

--------------------------------------------------------------------------------2

-- | Genera {"constructor":0,"fields":[{"bytes":"abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e"},{"constructor":0,"fields":[{"bytes":"4353"},{"bytes":"4d616e75"}]},{"int":5000000}]}
getEncodedJsonFromData :: PlutusTx.Data -> DataByteStringLazy.ByteString
getEncodedJsonFromData dataToEncode =  DataAeson.encode $ (CardanoApi.scriptDataToJson CardanoApi.ScriptDataJsonDetailedSchema . dataToScriptData ) dataToEncode

-- | Lee {"constructor":0,"fields":[{"bytes":"abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e"},{"constructor":0,"fields":[{"bytes":"4353"},{"bytes":"4d616e75"}]},{"int":5000000}]}
getDataFromEncodedJson :: DataByteStringLazy.ByteString -> PlutusTx.Data
getDataFromEncodedJson dataToEncode =
        case DataAeson.decode dataToEncode of
                Nothing -> P.error "Could not decode"
                Just decoded -> case CardanoApi.scriptDataFromJson CardanoApi.ScriptDataJsonDetailedSchema decoded of
                    Right scriptData -> scriptDataToData scriptData
                    _                -> P.error "Could not scriptDataFromJson"

--------------------------------------------------------------------------------2

-- | Guarda {"constructor":0,"fields":[{"bytes":"abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e"},{"constructor":0,"fields":[{"bytes":"4353"},{"bytes":"4d616e75"}]},{"int":5000000}]}
writePlutusDataToFile :: PlutusTx.ToData a => P.FilePath -> a -> P.IO ()
writePlutusDataToFile filepath dataToWrite = do
    let !toWrite = getEncodedJsonFromData (PlutusTx.toData dataToWrite)
    writeFile filepath toWrite

--------------------------------------------------------------------------------2

-- lee archivos exportados con OffChainHelpers.writePlutusDataToFile

-- :set -XOverloadedStrings -XTypeApplications
-- readFileToPlutusData "datum.json"
-- contents of file: {"constructor":0,"fields":[{"bytes":"abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e"},{"constructor":0,"fields":[{"bytes":"4353"},{"bytes":"4d616e75"}]},{"int":5000000}]}

readFileToPlutusData :: P.String -> P.IO PlutusTx.Data
readFileToPlutusData filepath = do
    !file <- readFile filepath
    return $ getDataFromEncodedJson file

--------------------------------------------------------------------------------2

-- lee archivos exportados con OffChainHelpers.writePlutusDataToFile
-- :set -XOverloadedStrings -XTypeApplications

-- @TIPO puede ser LedgerApiV2.Datum o LedgerApiV2.Redeemer o cualquiera creado internamente
-- readFileToPlutusDataAsTyped @TIPO "datum.json"
-- readFileToPlutusDataAsTyped @LedgerApiV2.Datum "datum.json"
-- contents of file: {"constructor":0,"fields":[{"bytes":"abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e"},{"constructor":0,"fields":[{"bytes":"4353"},{"bytes":"4d616e75"}]},{"int":5000000}]}

readFileToPlutusDataAsTyped :: forall a. (LedgerApiV2.UnsafeFromData a, P.Show a) => P.String -> P.IO a
readFileToPlutusDataAsTyped filepath = do
    !raw <- readFileToPlutusData filepath
    P.putStrLn $ "Raw: " ++ P.show raw
    let !datum = LedgerApiV2.unsafeFromBuiltinData @a (LedgerApiV2.BuiltinData raw)
    P.putStrLn $ "Result: " ++ P.show datum
    return datum

--------------------------------------------------------------------------------2

writeUnit :: P.String -> P.IO ()
writeUnit path = writePlutusDataToFile (path ++ "/unit.json") ()

--------------------------------------------------------------------------------2

tryReadWalletId :: P.String -> Maybe WalletEmulator.WalletId
tryReadWalletId = DataAeson.decode . DataAeson.encode

--------------------------------------------------------------------------------2

unsafeReadWalletId :: P.String -> WalletEmulator.WalletId
unsafeReadWalletId s = DataMaybe.fromMaybe (P.error $ "can't parse " ++ s ++ " as a WalletId") $ tryReadWalletId s

--------------------------------------------------------------------------------2

credentialLedgerToPlutus :: LedgerCredential.Credential a LedgerCrypto.StandardCrypto -> LedgerCredentialV1.Credential
credentialLedgerToPlutus (LedgerCredential.ScriptHashObj (LedgerHashes.ScriptHash h)) = LedgerApiV1.ScriptCredential $ LedgerApiV1.ValidatorHash $ TxBuiltins.toBuiltin $ CryptoHashClass.hashToBytes h
credentialLedgerToPlutus (LedgerCredential.KeyHashObj (LedgerKeys.KeyHash h))         = LedgerApiV1.PubKeyCredential $ LedgerApiV1.PubKeyHash $ TxBuiltins.toBuiltin $ CryptoHashClass.hashToBytes h

--------------------------------------------------------------------------------2

stakeReferenceLedgerToPlutus :: LedgerCredential.StakeReference LedgerCrypto.StandardCrypto -> Maybe LedgerCredentialV1.StakingCredential
stakeReferenceLedgerToPlutus (LedgerCredential.StakeRefBase x) = Just $ LedgerApiV1.StakingHash $ credentialLedgerToPlutus x
stakeReferenceLedgerToPlutus (LedgerCredential.StakeRefPtr (LedgerCredential.Ptr (CardanoApi.SlotNo x) txIx certIx)) =
    let !txIxInteger = P.toInteger (LedgerBaseTypes.txIxToInt txIx)
        !certIxInteger = P.toInteger (LedgerBaseTypes.certIxToInt certIx)
    in  Just $ LedgerApiV1.StakingPtr (P.fromIntegral x) txIxInteger certIxInteger
stakeReferenceLedgerToPlutus LedgerCredential.StakeRefNull = Nothing

--------------------------------------------------------------------------------2

getRight :: P.Either a b -> b
getRight (P.Right x) = x
getRight (P.Left _)  = P.error "getRight: Left"

--------------------------------------------------------------------------------2

-- TODO: Usa plutus-1.1.0
addressToCardanoAddress :: Ledger.NetworkId -> LedgerAddress.Address -> LedgerAddress.CardanoAddress
addressToCardanoAddress network add = getRight $ LedgerTxCardanoAPI.toCardanoAddressInEra network add

-- TODO: Usa plutus-1.1.0
cardanoAddressToAddress :: LedgerAddress.CardanoAddress -> LedgerAddress.Address
cardanoAddressToAddress = Ledger.toPlutusAddress

-- cardanoAddressToAddress :: LedgerAddress.Address -> LedgerAddress.Address
-- cardanoAddressToAddress x = x

--------------------------------------------------------------------------------2

tryReadAddress :: P.String -> Maybe LedgerApiV1.Address
tryReadAddress x =
    case CardanoApi.deserialiseAddress CardanoApi.AsAddressAny $ DataText.pack x of
        Nothing -> Nothing
        Just (ApiShelley.AddressByron _) -> Nothing
        Just (ApiShelley.AddressShelley (ApiShelley.ShelleyAddress _ p s)) ->
            Just
                LedgerApiV1.Address
                    { LedgerApiV1.addressCredential = credentialLedgerToPlutus p,
                      LedgerApiV1.addressStakingCredential = stakeReferenceLedgerToPlutus s
                    }

--------------------------------------------------------------------------------2

unsafeReadAddress :: P.String -> LedgerApiV1.Address
unsafeReadAddress s = DataMaybe.fromMaybe (P.error $ "can't parse " ++ s ++ " as an address") $ tryReadAddress s

--------------------------------------------------------------------------------2

unsafeReadTxOutRef :: P.String -> LedgerApiV1.TxOutRef
unsafeReadTxOutRef s =
    let !(x, y) = case P.span (P./= '#') s of
            (x', _ : y') -> (x', y')
            _            -> P.error $ "can't parse " ++ s ++ " as a TxOutRef"
    in  LedgerApiV1.TxOutRef
            { LedgerApiV1.txOutRefId = DataString.fromString x,
              LedgerApiV1.txOutRefIdx = P.read y
            }

--------------------------------------------------------------------------------2

getCredentials :: LedgerApiV1.Address -> Maybe (Ledger.PaymentPubKeyHash, Maybe Ledger.StakePubKeyHash)
getCredentials (LedgerApiV1.Address x y) =
    case x of
        LedgerApiV1.ScriptCredential _ -> Nothing
        LedgerApiV1.PubKeyCredential pkh ->
            let !ppkh = Ledger.PaymentPubKeyHash pkh
            in  case y of
                    Nothing -> Just (ppkh, Nothing)
                    Just LedgerApiV1.StakingPtr {} -> Nothing
                    Just (LedgerApiV1.StakingHash h) ->
                        case h of
                            LedgerApiV1.ScriptCredential _    -> Nothing
                            LedgerApiV1.PubKeyCredential pkh' -> Just (ppkh, Just $ Ledger.StakePubKeyHash pkh')

--------------------------------------------------------------------------------2

unsafeGetPaymentPubKeyHash :: LedgerApiV1.Address -> Ledger.PaymentPubKeyHash
unsafeGetPaymentPubKeyHash addr = maybe (P.error $ "script address " ++ P.show addr ++ " does not contain a payment key") fst $ getCredentials addr

unsafeGetStakePubKeyHash :: LedgerApiV1.Address -> Ledger.StakePubKeyHash
unsafeGetStakePubKeyHash addr =
    case getCredentials addr of
        Nothing           -> P.error $ "unexpected script address " ++ P.show addr
        Just (_, Nothing) -> P.error $ "addres " ++ P.show addr ++ " contains no stake component"
        Just (_, Just x)  -> x

getStakePubKeyHash :: LedgerApiV1.Address -> Maybe Ledger.StakePubKeyHash
getStakePubKeyHash addr =
    case getCredentials addr of
        Nothing     -> Nothing
        Just (_, x) -> x

--------------------------------------------------------------------------------2

pubKeyToPubKeyHash :: Ledger.PubKey -> LedgerApiV2.PubKeyHash
pubKeyToPubKeyHash = Ledger.pubKeyHash

pubKeyHashToPaymentPubKey ::LedgerApiV2.PubKeyHash -> Ledger.PaymentPubKey
pubKeyHashToPaymentPubKey pph = LedgerAddress.PaymentPubKey $ Crypto.PubKey $ LedgerApiV2.LedgerBytes $ Ledger.getPubKeyHash pph

seedToPaymentPubKey :: DataByteString.ByteString -> Ledger.PaymentPubKey
seedToPaymentPubKey seed =
    let
        wallet_XPriv =  Crypto.generateFromSeed' seed
        wallet_PubKey = Crypto.toPublicKey wallet_XPriv
        wallet_PaymentPubKey = Ledger.PaymentPubKey wallet_PubKey
    in
        wallet_PaymentPubKey

--------------------------------------------------------------------------------2

cidToString :: WalletTypes.ContractInstanceId -> P.String
cidToString = P.show . WalletTypes.unContractInstanceId

--------------------------------------------------------------------------------2

unsafeTokenNameToHex :: LedgerValueV1.TokenName -> P.String
unsafeTokenNameToHex = DataByteStringChar8.unpack . CardanoApi.serialiseToRawBytesHex . DataMaybe.fromJust . CardanoApi.deserialiseFromRawBytes CardanoApi.AsAssetName . getByteString . LedgerValueV1.unTokenName
    where
        getByteString (TxBuiltinsInternal.BuiltinByteString bs) = bs

--------------------------------------------------------------------------------2

-- for testing the sha_256 hash in the tokenName and comparting with the one i was making in Javascript
concatenateBBS :: P.String -> P.String -> BuiltinByteString
concatenateBBS bbs1 bbs2 = TxBuiltinsClass.stringToBuiltinByteString bbs1 <> TxBuiltinsClass.stringToBuiltinByteString bbs2

--------------------------------------------------------------------------------2

-- for testing the sha_256 hash in the tokenName and comparting with the one i was making in Javascript
sha2_256Str :: P.String -> BuiltinByteString
sha2_256Str bbs1 = sha2_256 $ TxBuiltinsClass.stringToBuiltinByteString bbs1

--------------------------------------------------------------------------------2

-- for testing the sha_256 hash in the tokenName and comparting with the one i was making in Javascript
sha2_256HexStr :: P.String -> BuiltinByteString
sha2_256HexStr bbs1 =
    let r1 = case LedgerBytes.fromHex $ DataByteStringUTF8.fromString bbs1 of
            Right r -> r
            Left _  -> P.error $ "Could not convert from hex to bytes: " ++ bbs1
    in  sha2_256 $ LedgerApiV1.getLedgerBytes r1

--------------------------------------------------------------------------------2

-- for testing the sha_256 hash in the tokenName and comparting with the one i was making in Javascript
sha2_256BBS :: BuiltinByteString -> BuiltinByteString
sha2_256BBS = sha2_256

--------------------------------------------------------------------------------2

-- for testing the sha_256 hash in the tokenName and comparting with the one i was making in Javascript
sha2_256HexBBS :: BuiltinByteString -> BuiltinByteString
sha2_256HexBBS bbs1 =
    let r1 = case LedgerBytes.fromHex $ DataByteStringUTF8.fromString $ P.show bbs1 of
            Right r -> r
            Left _  -> P.error $ "Could not convert from hex to bytes: " ++ P.show bbs1
    in  sha2_256 $ LedgerApiV1.getLedgerBytes r1

--------------------------------------------------------------------------------2

-- for testing the sha_256 hash in the tokenName and comparting with the one i was making in Javascript
bbsToTokenName :: BuiltinByteString -> LedgerApiV1.TokenName
bbsToTokenName = LedgerApiV1.TokenName

--------------------------------------------------------------------------------2

pkhFromStr :: P.String -> LedgerApiV1.PubKeyHash
pkhFromStr s =
    case LedgerBytes.fromHex (DataString.fromString s) of
        Right (LedgerBytes.LedgerBytes bytes) -> LedgerApiV1.PubKeyHash bytes
        Left msg                              -> P.error $ "Could not convert from hex to bytes: " ++ P.show msg



--------------------------------------------------------------------------------2

hashValidator :: LedgerApiV2.Validator -> LedgerApiV2.ValidatorHash
hashValidator = UtilsScriptsV2.validatorHash

hashScriptValidator :: LedgerApiV2.Validator -> LedgerApiV2.ScriptHash
hashScriptValidator = UtilsScriptsV2.scriptHash . LedgerApiV2.getValidator

-- addressValidator :: LedgerApiV2.ValidatorHash -> LedgerAddress.Address
-- addressValidator = Ledger.scriptHashAddress

addressValidator :: LedgerApiV2.ValidatorHash -> LedgerAddress.Address
addressValidator = Ledger.scriptHashAddress

--------------------------------------------------------------------------------2

getCurSymbolOfPolicy :: LedgerApiV2.MintingPolicy -> LedgerApiV2.CurrencySymbol
getCurSymbolOfPolicy = UtilsScriptsV2.scriptCurrencySymbol

hashScriptMinting :: LedgerApiV2.MintingPolicy -> LedgerApiV2.ScriptHash
hashScriptMinting = UtilsScriptsV2.scriptHash . LedgerApiV2.getMintingPolicy

getPolicyScript :: LedgerApiV2.MintingPolicy -> LedgerApiV2.Script
getPolicyScript = LedgerApiV2.unMintingPolicyScript

--------------------------------------------------------------------------------2

getScriptUnValidatorV1 :: LedgerScriptsV1.Validator -> LedgerScriptsV1.Script
getScriptUnValidatorV1 = LedgerScriptsV1.unValidatorScript

getScriptShortBsV1 :: LedgerScriptsV1.Script -> DataByteStringShort.ShortByteString
getScriptShortBsV1 = DataByteStringShort.toShort . DataByteStringLazy.toStrict . CodecSerialise.serialise

getScriptSerialisedV1 :: DataByteStringShort.ShortByteString -> ApiShelley.PlutusScript ApiShelley.PlutusScriptV1
getScriptSerialisedV1 = ApiShelley.PlutusScriptSerialised

writeValidatorV1 :: P.String -> P.String -> LedgerScriptsV1.Validator -> P.IO (Either (CardanoApi.FileError ()) ())
writeValidatorV1 path file codeValidator = do
    let -- v1dir = "V1"
        !scriptUnValidatorV1 = getScriptUnValidatorV1 codeValidator
        !scriptShortBsV1 = getScriptShortBsV1 scriptUnValidatorV1
        !scriptSerialisedV1 = getScriptSerialisedV1 scriptShortBsV1
    SystemDirectory.createDirectoryIfMissing True path -- SystemFilePathPosix.</> v1dir
    CardanoApi.writeFileTextEnvelope (path SystemFilePathPosix.</> file) Nothing scriptSerialisedV1

--------------------------------------------------------------------------------2

getScriptUnValidator :: LedgerApiV2.Validator -> LedgerApiV2.Script
getScriptUnValidator = LedgerApiV2.unValidatorScript

getScriptShortBs :: LedgerApiV2.Script -> DataByteStringShort.ShortByteString
getScriptShortBs = DataByteStringShort.toShort . DataByteStringLazy.toStrict . CodecSerialise.serialise

getScriptSerialised :: DataByteStringShort.ShortByteString -> ApiShelley.PlutusScript ApiShelley.PlutusScriptV2
getScriptSerialised = ApiShelley.PlutusScriptSerialised

--------------------------------------------------------------------------------2

writeValidator :: P.String -> P.String -> LedgerScriptsV1.Validator -> P.IO (Either (CardanoApi.FileError ()) ())
writeValidator path file codeValidator = do
    let -- v1dir = "V2"
        !scriptUnValidatorV2 = getScriptUnValidator codeValidator
        !scriptShortBsV2 = getScriptShortBs scriptUnValidatorV2
        !scriptSerialisedV2 = getScriptSerialised scriptShortBsV2
    SystemDirectory.createDirectoryIfMissing True path -- SystemFilePathPosix.</> V2dir
    CardanoApi.writeFileTextEnvelope (path SystemFilePathPosix.</> file) Nothing scriptSerialisedV2

--------------------------------------------------------------------------------2

readValidator  :: P.String -> P.String -> P.IO (Either (CardanoApi.FileError ApiShelley.TextEnvelopeError) LedgerApiV2.Validator)
readValidator  path file = do
    CardanoApi.readFileTextEnvelope (ApiShelley.AsPlutusScript ApiShelley.AsPlutusScriptV2) (path SystemFilePathPosix.</> file)
        >>= \case
            Right key' -> do
                -- let des = ApiShelley.deserialiseFromRawBytes (ApiShelley.AsPlutusScript ApiShelley.AsPlutusScriptV2) key' -- CodecSerialise.deserialise  key'
                let des = LedgerApiV2.Validator $ LedgerTxCardanoAPI.fromCardanoPlutusScript key'
                return $ Right des
            Left message -> return $ Left message
--------------------------------------------------------------------------------2

getScriptMintingPolicyV1 :: LedgerScriptsV1.MintingPolicy -> LedgerScriptsV1.Script
getScriptMintingPolicyV1 = LedgerScriptsV1.getMintingPolicy

writeMintingPolicyV1 :: P.String -> P.String -> LedgerScriptsV1.MintingPolicy -> P.IO (Either (CardanoApi.FileError ()) ())
writeMintingPolicyV1 path file policy = do
    let -- v1dir = "V1"
        !scriptMintingPolicyV1 = getScriptMintingPolicyV1 policy
        !scriptShortBsV1 = getScriptShortBsV1 scriptMintingPolicyV1
        !scriptSerialisedV1 = getScriptSerialisedV1 scriptShortBsV1
    SystemDirectory.createDirectoryIfMissing True path -- SystemFilePathPosix.</> v1dir
    CardanoApi.writeFileTextEnvelope (path SystemFilePathPosix.</> file) Nothing scriptSerialisedV1

--------------------------------------------------------------------------------2

getScriptMintingPolicy :: LedgerApiV2.MintingPolicy -> LedgerApiV2.Script
getScriptMintingPolicy = LedgerApiV2.getMintingPolicy

writeMintingPolicy :: P.String -> P.String -> LedgerApiV2.MintingPolicy -> P.IO (Either (CardanoApi.FileError ()) ())
writeMintingPolicy path file policy = do
    let -- v1dir = "V1"
        !scriptMintingPolicyV2 = getScriptMintingPolicy policy
        !scriptShortBsV2 = getScriptShortBs scriptMintingPolicyV2
        !scriptSerialisedV2 = getScriptSerialised scriptShortBsV2
    SystemDirectory.createDirectoryIfMissing True path -- SystemFilePathPosix.</> v1dir
    CardanoApi.writeFileTextEnvelope (path SystemFilePathPosix.</> file) Nothing scriptSerialisedV2

--------------------------------------------------------------------------------2

readMintingPolicy :: P.String -> P.String -> P.IO (Either (CardanoApi.FileError ApiShelley.TextEnvelopeError) LedgerApiV2.MintingPolicy)
readMintingPolicy path file = do
    CardanoApi.readFileTextEnvelope (ApiShelley.AsPlutusScript ApiShelley.AsPlutusScriptV2) (path SystemFilePathPosix.</> file)
        >>= \case
            Right key' -> do
                -- let des = ApiShelley.deserialiseFromRawBytes (ApiShelley.AsPlutusScript ApiShelley.AsPlutusScriptV2) key' -- CodecSerialise.deserialise  key'
                let des = LedgerApiV2.MintingPolicy $ LedgerTxCardanoAPI.fromCardanoPlutusScript key'
                return $ Right des
            Left message -> return $ Left message

--------------------------------------------------------------------------------2
-- Create Script Address
--------------------------------------------------------------------------------2

{-
Header for Testnet is "70" and for Mainnet "71"
To work out why you need to go read CIP19:
https://cips.cardano.org/cips/cip19/#shelley-addresses

And then look up the
- Header Bits '011100000' for Testnet or
- Header Bits '011100001' for Mainnet
In a binary to Hex table, such as here:
https://www.rapidtables.com/convert/number/binary-to-ascii.html

The header is in Base16 format (aka Hexadecimal)
-}

addrBech32AddHeader :: P.String -> P.String -> DataText.Text
addrBech32AddHeader h v = DataText.pack (h P.++ v)

addrBech32HeaderTestnet :: P.String
addrBech32HeaderTestnet = "70"

addrBech32HeaderMainnet :: P.String
addrBech32HeaderMainnet = "71"

validatorHashToHex :: LedgerApiV2.ValidatorHash -> P.String
validatorHashToHex = P.show

validatorHashHexWithHeader :: P.String -> LedgerApiV2.ValidatorHash -> DataByteString.ByteString
validatorHashHexWithHeader headerNetworkTag vhash = DataTextEncoding.encodeUtf8 (addrBech32AddHeader headerNetworkTag (validatorHashToHex vhash))

validatorHashToBinary :: P.String -> LedgerApiV2.ValidatorHash -> DataByteString.ByteString
validatorHashToBinary headerNetworkTag vhash = do
    let decoded = case DataByteStringBase16.decode (validatorHashHexWithHeader headerNetworkTag vhash) of
            Left err -> P.error $ "Could not decode validatorHashToBinary: " ++ P.show err
            Right r  -> r
    decoded

{-
Prefix for Testnet: "addr_test" and Mainnet: "addr"
These prefixes are imported from Cardano.Codec.Bech32.Prefixes
-}

addrBech32DataPart :: P.String -> LedgerApiV2.ValidatorHash -> CodecBinaryBech32.DataPart
addrBech32DataPart headerNetworkTag vhash = CodecBinaryBech32.dataPartFromBytes (validatorHashToBinary headerNetworkTag vhash)

validatorAddrToHash :: LedgerAddress.Address -> LedgerApiV2.ValidatorHash
validatorAddrToHash addr = DataMaybe.fromJust (Ledger.toValidatorHash addr)

{-
The bech32 representation is derived in 2 steps:
- bech32 = prefix + dataPart
- and where dataPart = header + validatorHash
-}

validatorAddrToAddrBech32Testnet :: LedgerAddress.Address -> DataByteStringLazy.ByteString
validatorAddrToAddrBech32Testnet addr = do
    let !vhash = validatorAddrToHash addr
        !headerNetworkTag = addrBech32HeaderTestnet
        encoded = case CodecBinaryBech32.encode Bench32Prefixes.addr_test (addrBech32DataPart headerNetworkTag vhash) of
            Left err -> P.error $ "Could not encode validatorAddrToAddrBech32Testnet: " ++ P.show err
            Right r  -> r
    DataByteStringLazy.fromStrict $ DataTextEncoding.encodeUtf8 encoded

validatorAddrToAddrBech32Mainnet :: LedgerAddress.Address -> DataByteStringLazy.ByteString
validatorAddrToAddrBech32Mainnet addr = do
    let !vhash = validatorAddrToHash addr
        !headerNetworkTag = addrBech32HeaderMainnet
        encoded = case CodecBinaryBech32.encode Bench32Prefixes.addr (addrBech32DataPart headerNetworkTag vhash) of
            Left err -> P.error $ "Could not encode validatorAddrToAddrBech32Mainnet: " ++ P.show err
            Right r  -> r
    DataByteStringLazy.fromStrict $ DataTextEncoding.encodeUtf8 encoded

--------------------------------------------------------------------------------2

-- | Try to get the Type from a DecoratedTxOut.
getDatumFromDecoratedTxOut :: forall datum. PlutusTx.FromData datum => LedgerTx.DecoratedTxOut -> Maybe datum
getDatumFromDecoratedTxOut decoratedTxOut =
    -- lens: puedo hacer directamente el getter y luego no tengo que chekear por maybe... ya este simbolo lo hace todo
    -- en lugar de esto:
    -- DataMaybe.fromJust $ decoratedTxOut ControlLens.^? LedgerTx.decoratedTxOutDatum
    -- pongo esto:
    -- (decoratedTxOut ControlLens.^?! LedgerTx.decoratedTxOutDatum)
    let (_, mdatum) = (decoratedTxOut ControlLens.^?! LedgerTx.decoratedTxOutDatum)
        datum = (mdatum ControlLens.^?! LedgerTx.datumInDatumFromQuery)
        LedgerApiV2.Datum datumBuiltinData = datum
    in  LedgerApiV2.fromBuiltinData @datum datumBuiltinData

--------------------------------------------------------------------------------2

-- | Try to get the Datum from a DecoratedTxOut.
getUnsafeDatumFromDecoratedTxOut :: forall datum. PlutusTx.UnsafeFromData datum => LedgerTx.DecoratedTxOut -> datum
getUnsafeDatumFromDecoratedTxOut decoratedTxOut =
    -- lens: puedo hacer directamente el getter y luego no tengo que chekear por maybe... ya este simbolo lo hace todo
    -- en lugar de esto:
    -- DataMaybe.fromJust $ decoratedTxOut ControlLens.^? LedgerTx.decoratedTxOutDatum
    -- pongo esto:
    -- (decoratedTxOut ControlLens.^?! LedgerTx.decoratedTxOutDatum)
    let (_, mdatum) = (decoratedTxOut ControlLens.^?! LedgerTx.decoratedTxOutDatum)
        datum = (mdatum ControlLens.^?! LedgerTx.datumInDatumFromQuery)
        LedgerApiV2.Datum datumBuiltinData = datum
    in  LedgerApiV2.unsafeFromBuiltinData @datum datumBuiltinData

--------------------------------------------------------------------------------2

getValueFromDecoratedTxOut :: LedgerTx.DecoratedTxOut -> LedgerValue.Value
getValueFromDecoratedTxOut decoratedTxOut =
    let value = case decoratedTxOut ControlLens.^? LedgerTx.decoratedTxOutValue of
            Nothing -> P.error "getValueFromDecoratedTxOut: decoratedTxOutValue is Nothing"
            Just v  -> v
    in  value

--------------------------------------------------------------------------------2

isNFTInDecoratedTxOut :: LedgerTx.DecoratedTxOut -> Ledger.AssetClass -> Bool
isNFTInDecoratedTxOut ciTxOut = OnChainHelpers.isNFT_With_AC_InValue (getValueFromDecoratedTxOut ciTxOut)

--------------------------------------------------------------------------------2

isTokenInDecoratedTxOut :: LedgerTx.DecoratedTxOut -> Ledger.AssetClass -> Bool
isTokenInDecoratedTxOut ciTxOut = OnChainHelpers.isToken_With_AC_InValue (getValueFromDecoratedTxOut ciTxOut)

--------------------------------------------------------------------------------2

-- Function to extract datum unsafely from TxOut
getUnsafe_LedgerApiV2Datum_From_TxOutOutputDatum :: LedgerApiV2.TxOut -> LedgerApiV2.Datum
getUnsafe_LedgerApiV2Datum_From_TxOutOutputDatum !txOut =
    case LedgerApiV2.txOutDatum txOut of
            LedgerApiV2.OutputDatum datum -> datum
            _ -> P.error "getUnsafe_LedgerApiV2Datum_From_TxOutOutputDatum"

-- Function to extract datum unsafely from TxOut
getUnsafe_Datum_From_TxOutOutputDatum :: forall datum. PlutusTx.UnsafeFromData datum => LedgerApiV2.TxOut -> datum
getUnsafe_Datum_From_TxOutOutputDatum !txOut = LedgerApiV2.unsafeFromBuiltinData @datum $ LedgerApiV2.getDatum $ getUnsafe_LedgerApiV2Datum_From_TxOutOutputDatum txOut

-- Function to extract datum type unsafely from TxOut using a transformation function
getUnsafe_DatumType_From_TxOutOutputDatum :: forall datum datumType. PlutusTx.UnsafeFromData datum => LedgerApiV2.TxOut -> (datum -> datumType) -> datumType
getUnsafe_DatumType_From_TxOutOutputDatum !txOut !getDatumTypeFromDatum = getDatumTypeFromDatum  (getUnsafe_Datum_From_TxOutOutputDatum @datum  txOut)

--------------------------------------------------------------------------------


isTxValid :: ChainIndexTypes.TxStatus -> Bool
isTxValid txStatus =
    case txStatus of
        ChainIndexTypes.Unknown                                                  -> False
        ChainIndexTypes.Committed ChainIndexTypes.UnknownValidity _              -> False
        ChainIndexTypes.Committed ChainIndexTypes.TxInvalid _                    -> False
        ChainIndexTypes.Committed ChainIndexTypes.TxValid _                      -> True
        ChainIndexTypes.TentativelyConfirmed _ ChainIndexTypes.UnknownValidity _ -> False
        ChainIndexTypes.TentativelyConfirmed _ ChainIndexTypes.TxInvalid _       -> False
        ChainIndexTypes.TentativelyConfirmed _ ChainIndexTypes.TxValid _         -> True

--------------------------------------------------------------------------------2

mintToken_With_RefPolicyOrAttachedPolicy :: (PlutusTx.ToData redeemer) =>
    LedgerValue.Value ->
    Maybe (LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut) ->
    Maybe redeemer ->
    Maybe (LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut) ->
    Maybe LedgerApiV2.MintingPolicy ->
    (LedgerConstraints.ScriptLookups a0, LedgerTxConstraints.TxConstraints (UtilsTypedScriptsValidatorsV1.RedeemerType DataVoid.Void) (UtilsTypedScriptsValidatorsV1.DatumType DataVoid.Void))
mintToken_With_RefPolicyOrAttachedPolicy valueForMint txOut' redeemerMint' scriptRef' policy' = do

    let (lookupsTxMint, txMint) = case scriptRef' of
                Nothing ->
                    mintToken_With_Policy valueForMint txOut' redeemerMint' policy'
                Just scriptRef ->
                    mintToken_With_RefPolicy valueForMint txOut' redeemerMint' scriptRef
    (lookupsTxMint, txMint)

--------------------------------------------------------------------------------2

mintToken_With_Policy :: (PlutusTx.ToData redeemer) =>
    LedgerValue.Value ->
    Maybe (LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut) ->
    Maybe redeemer ->
    Maybe LedgerApiV2.MintingPolicy ->
    (LedgerConstraints.ScriptLookups a0, LedgerTxConstraints.TxConstraints (UtilsTypedScriptsValidatorsV1.RedeemerType DataVoid.Void) (UtilsTypedScriptsValidatorsV1.DatumType DataVoid.Void))
mintToken_With_Policy valueForMint txOut'  redeemerMint' policy' = do

    let utxoList =  case txOut' of
            Nothing    -> []
            Just txOut -> [txOut]

    let lookupsTxMint' = LedgerConstraints.unspentOutputs (DataMap.fromList utxoList)

        lookupsTxMint = case policy' of
            Just policy -> lookupsTxMint' P.<> LedgerConstraints.plutusV2MintingPolicy policy
            Nothing     -> lookupsTxMint'

    let txMint' = case redeemerMint' of
                Nothing           -> LedgerConstraints.mustMintValue valueForMint
                Just redeemerMint -> LedgerConstraints.mustMintValueWithRedeemer (LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData redeemerMint) valueForMint

        txMint = case  txOut' of
            Nothing    -> txMint'
            Just txOut -> txMint' P.<> LedgerConstraints.mustSpendPubKeyOutput (fst txOut)


    (lookupsTxMint, txMint)

--------------------------------------------------------------------------------2

mintToken_With_RefPolicy ::  (PlutusTx.ToData redeemer) =>
    LedgerValue.Value ->
    Maybe (LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut) ->
    Maybe redeemer ->
    (LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut) ->
    (LedgerConstraints.ScriptLookups a0, LedgerTxConstraints.TxConstraints (UtilsTypedScriptsValidatorsV1.RedeemerType DataVoid.Void) (UtilsTypedScriptsValidatorsV1.DatumType DataVoid.Void))
mintToken_With_RefPolicy valueForMint txOut' redeemerMint' scriptRef   = do
    let utxoList =  case txOut' of
            Nothing    -> [scriptRef]
            Just txOut -> [txOut, scriptRef]

    let lookupsTxMint = LedgerConstraints.unspentOutputs (DataMap.fromList utxoList)

    let txMint' = case redeemerMint' of
                    Nothing           -> LedgerTxConstraints.mustMintValueWithReference (fst scriptRef) valueForMint
                    Just redeemerMint -> LedgerTxConstraints.mustMintValueWithRedeemerAndReference (LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData redeemerMint) (Just $ fst scriptRef) valueForMint

        txMint = case  txOut' of
            Nothing    -> txMint'
            Just txOut -> txMint' P.<> LedgerConstraints.mustSpendPubKeyOutput (fst txOut)

    (lookupsTxMint, txMint)

--------------------------------------------------------------------------------2

mustSpendScriptOutput_With_RefPolicyOrAttachedPolicy :: (PlutusTx.ToData redeemer) =>
    (LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut) ->
    redeemer ->
    Maybe (LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut) ->
    Maybe LedgerApiV2.Validator ->
    (LedgerConstraints.ScriptLookups a0, LedgerTxConstraints.TxConstraints (UtilsTypedScriptsValidatorsV1.RedeemerType DataVoid.Void) (UtilsTypedScriptsValidatorsV1.DatumType DataVoid.Void))
mustSpendScriptOutput_With_RefPolicyOrAttachedPolicy uTxO_To_Consume redeemer scriptRef' validator' = do
    let (lookupsTx, tx) =
            case scriptRef' of
                Nothing ->
                    mustSpendScriptOutput_With_Policy uTxO_To_Consume redeemer validator'
                Just scriptRef ->
                    mustSpendScriptOutput_With_RefPolicy uTxO_To_Consume redeemer scriptRef
    (lookupsTx, tx)

--------------------------------------------------------------------------------2

mustSpendScriptOutput_With_Policy :: (PlutusTx.ToData redeemer) =>
    (LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut) ->
    redeemer ->
    Maybe LedgerApiV2.Validator ->
    (LedgerConstraints.ScriptLookups a0, LedgerTxConstraints.TxConstraints (UtilsTypedScriptsValidatorsV1.RedeemerType DataVoid.Void) (UtilsTypedScriptsValidatorsV1.DatumType DataVoid.Void))
mustSpendScriptOutput_With_Policy uTxO_To_Consume redeemer validator'= do
    let lookupsTx' = LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_To_Consume])
        lookupsTx = case validator' of
            Just validator -> lookupsTx' P.<> LedgerConstraints.plutusV2OtherScript validator
            Nothing        -> lookupsTx'
    let tx = LedgerConstraints.mustSpendScriptOutput (P.fst uTxO_To_Consume) (LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData redeemer)
    (lookupsTx, tx)

--------------------------------------------------------------------------------2

mustSpendScriptOutput_With_RefPolicy :: (PlutusTx.ToData redeemer) =>
    (LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut) ->
    redeemer ->
    (LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut) ->
    (LedgerConstraints.ScriptLookups a0, LedgerTxConstraints.TxConstraints (UtilsTypedScriptsValidatorsV1.RedeemerType DataVoid.Void) (UtilsTypedScriptsValidatorsV1.DatumType DataVoid.Void))
mustSpendScriptOutput_With_RefPolicy uTxO_To_Consume redeemer scriptRef = do
    let lookupsTx = LedgerConstraints.unspentOutputs (DataMap.fromList [scriptRef, uTxO_To_Consume])
    let tx = LedgerConstraints.mustSpendScriptOutputWithReference (P.fst uTxO_To_Consume) (LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData redeemer) (P.fst scriptRef)
    (lookupsTx, tx)

--------------------------------------------------------------------------------2

createValueAddingTokensOfCurrencySymbolOffChain :: (DataString.IsString e) => LedgerValue.AssetClass -> LedgerApiV2.CurrencySymbol -> Bool -> LedgerApiV2.Value -> Integer -> PlutusContract.Contract w s e LedgerApiV2.Value
createValueAddingTokensOfCurrencySymbolOffChain ac cs acIsWithoutTokenName value cantidad = do
    if not acIsWithoutTokenName
        then do
            return $ LedgerValue.assetClassValue ac cantidad
        else do
            -- si la unidad es un token, pero no tiene nombre,
            -- significa que estoy usando tokens con misma currency symbol pero diferente token name.
            -- voy a tomar en orden la cantidad de tokens de la currency symbol sin importar el token name
            let !tokenOfCurrencySymbol = [(tn, am) | (cs', tn, am) <- OnChainHelpers.flattenValue value, cs' == cs]
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Currency Symbol: %s" (P.show cs)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Token Of Currency Symbol: %s" (P.show tokenOfCurrencySymbol)
            let compareTokenName :: (LedgerApiV2.TokenName, Integer) -> (LedgerApiV2.TokenName, Integer) -> P.Ordering
                compareTokenName (tn1, _) (tn2, _)
                    | tn1 < tn2 = LT
                    | otherwise = GT

                !tokenOfCurrencySymbol_Ordered = sortBy compareTokenName tokenOfCurrencySymbol

                sumarTokens :: (DataString.IsString e) => [(LedgerApiV2.TokenName, Integer)] -> Integer -> PlutusContract.Contract w s e LedgerApiV2.Value
                sumarTokens [] left = do
                    if left > 0
                        then do
                            PlutusContract.throwError "Can't find enough tokens with that Currency Symbol in Value"
                        else return $ LedgerAda.lovelaceValueOf 0
                sumarTokens list left =
                    let (tn, am) = head list
                        !harvest_AC = LedgerValue.AssetClass (cs, tn)
                    in  if am > left
                            then return $ LedgerValue.assetClassValue harvest_AC left
                            else do
                                tailSum <- sumarTokens (tail list) (left - am)
                                return $ LedgerValue.assetClassValue harvest_AC am <> tailSum

            sumarTokens tokenOfCurrencySymbol_Ordered cantidad

--------------------------------------------------------------------------------2

checkIfThereIsUTxOFreeForCollateral :: DataMap.Map LedgerApiV2.TxOutRef LedgerTx.DecoratedTxOut -> PlutusContract.Contract w s e Bool
checkIfThereIsUTxOFreeForCollateral uTxO = do
    let !uTxOFreeForCollateral =
            [ (ref, out) | (ref, out) <- DataMap.toList uTxO, let value = getValueFromDecoratedTxOut out
                                                              in  LedgerValue.adaOnlyValue value == value
                                                                    && LedgerAda.fromValue value >= 5000000
            ]
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxO Free For Collateral: %s" (P.show uTxOFreeForCollateral)
    if P.not (P.null uTxOFreeForCollateral)
        then return True
        else return False

--------------------------------------------------------------------------------2

getBalanceOfUTXOs :: DataMap.Map LedgerApiV2.TxOutRef LedgerTx.DecoratedTxOut -> PlutusContract.Contract w s e LedgerValue.Value
getBalanceOfUTXOs uTxO = do
    return $ foldl (<>) (LedgerAda.lovelaceValueOf 0) [getValueFromDecoratedTxOut out | (_, out) <- DataMap.toList uTxO]

--------------------------------------------------------------------------------22

printTitle :: P.String -> PlutusContract.Contract w s DataText.Text ()
printTitle variable = do
    let len = 80 :: Integer
    let line = strictTextToString $ DataText.replicate (P.fromIntegral len) "-"
    let variableLine' = DataText.replicate (P.fromIntegral ((len `divide` 2) - 2 - (P.fromIntegral (P.length variable) `divide` 2))) "-"
    let variableLine = strictTextToString variableLine' ++ " " ++ variable ++ " " ++ strictTextToString variableLine'
    PlutusContract.logWarn @P.String $ TextPrintf.printf line
    PlutusContract.logWarn @P.String $ TextPrintf.printf variableLine
    PlutusContract.logWarn @P.String $ TextPrintf.printf line

--------------------------------------------------------------------------------2

printSubTitle :: P.String -> PlutusContract.Contract w s DataText.Text ()
printSubTitle variable = do
    let len = 60 :: Integer
    let variableLine' = DataText.replicate (P.fromIntegral ((len `divide` 2) - 2 - (P.fromIntegral (P.length variable) `divide` 2))) "-"
    let variableLine = strictTextToString variableLine' ++ " " ++ variable ++ " " ++ strictTextToString variableLine'
    PlutusContract.logInfo @P.String $ TextPrintf.printf variableLine

--------------------------------------------------------------------------------2

printSeparator :: PlutusContract.Contract w s DataText.Text ()
printSeparator = do
    let len = 80
    let variableLine' = DataText.replicate len "-"
    let variableLine = strictTextToString variableLine'
    PlutusContract.logInfo @P.String $ TextPrintf.printf variableLine

--------------------------------------------------------------------------------2

printSmallSeparator :: PlutusContract.Contract w s DataText.Text ()
printSmallSeparator = do
    let len = 40
    let variableLine' = DataText.replicate len "-"
    let variableLine = strictTextToString variableLine'
    PlutusContract.logInfo @P.String $ TextPrintf.printf variableLine

--------------------------------------------------------------------------------2

get_TxOutRefs_DecoratedTxOuts_And_DatumsTypes_By_CS :: forall datum datumType w s e. (PlutusTx.FromData datum) => LedgerApiV2.CurrencySymbol -> DataMap.Map LedgerApiV2.TxOutRef LedgerTx.DecoratedTxOut -> (datum -> datumType) -> PlutusContract.Contract w s e [(LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut, Maybe datumType)]
get_TxOutRefs_DecoratedTxOuts_And_DatumsTypes_By_CS !cs !uTxOs !getDatumTypeFromDatum = do
    let uTxOs' = [(txOutRef, decoratedTxOut, (Just . getDatumTypeFromDatum) P.=<< getDatumFromDecoratedTxOut @datum decoratedTxOut) | (txOutRef, decoratedTxOut) <- DataMap.toList uTxOs, OnChainHelpers.isNFT_With_CS_InValue (getValueFromDecoratedTxOut decoratedTxOut) cs && isJust (getDatumFromDecoratedTxOut @datum decoratedTxOut)]
    return uTxOs'
--------------------------------------------------------------------------------2

getUnsafe_TxOutRefs_DecoratedTxOuts_And_DatumsTypes_By_CS :: forall datum datumType w s e. (PlutusTx.UnsafeFromData datum) => LedgerApiV2.CurrencySymbol -> DataMap.Map LedgerApiV2.TxOutRef LedgerTx.DecoratedTxOut -> (datum -> datumType) -> PlutusContract.Contract w s e [(LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut, datumType)]
getUnsafe_TxOutRefs_DecoratedTxOuts_And_DatumsTypes_By_CS !cs !uTxOs !getDatumTypeFromDatum = do
    let uTxOs' = [(txOutRef, decoratedTxOut, getDatumTypeFromDatum $ getUnsafeDatumFromDecoratedTxOut @datum decoratedTxOut) | (txOutRef, decoratedTxOut) <- DataMap.toList uTxOs, OnChainHelpers.isNFT_With_CS_InValue (getValueFromDecoratedTxOut decoratedTxOut) cs ]
    return uTxOs'
--------------------------------------------------------------------------------2

get_TxOutRefs_DecoratedTxOuts_And_DatumsTypes_By_AC :: forall datum datumType w s e. (PlutusTx.FromData datum) => LedgerValue.AssetClass -> DataMap.Map LedgerApiV2.TxOutRef LedgerTx.DecoratedTxOut -> (datum -> datumType) -> PlutusContract.Contract w s e [(LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut, Maybe datumType)]
get_TxOutRefs_DecoratedTxOuts_And_DatumsTypes_By_AC !ac !uTxOs !getDatumTypeFromDatum = do
    let uTxOs' = [(txOutRef, decoratedTxOut, (Just . getDatumTypeFromDatum) P.=<< getDatumFromDecoratedTxOut @datum decoratedTxOut) | (txOutRef, decoratedTxOut) <- DataMap.toList uTxOs, OnChainHelpers.isNFT_With_AC_InValue (getValueFromDecoratedTxOut decoratedTxOut) ac && isJust (getDatumFromDecoratedTxOut @datum decoratedTxOut)]
    return uTxOs'

getUnsafe_TxOutRefs_DecoratedTxOuts_And_DatumsTypes_By_AC :: forall datum datumType w s e. (PlutusTx.UnsafeFromData datum) => LedgerValue.AssetClass -> DataMap.Map LedgerApiV2.TxOutRef LedgerTx.DecoratedTxOut -> (datum -> datumType) -> PlutusContract.Contract w s e [(LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut, datumType)]
getUnsafe_TxOutRefs_DecoratedTxOuts_And_DatumsTypes_By_AC !ac !uTxOs !getDatumTypeFromDatum = do
    let uTxOs' = [(txOutRef, decoratedTxOut, getDatumTypeFromDatum $ getUnsafeDatumFromDecoratedTxOut @datum decoratedTxOut) | (txOutRef, decoratedTxOut) <- DataMap.toList uTxOs, OnChainHelpers.isNFT_With_AC_InValue (getValueFromDecoratedTxOut decoratedTxOut) ac ]
    return uTxOs'

--------------------------------------------------------------------------------2

checkCollateral :: DataMap.Map LedgerApiV2.TxOutRef LedgerTx.DecoratedTxOut  -> PlutusContract.Contract w s DataText.Text ()
checkCollateral uTxOsAtUser = do
    !swCheckCollateral <- checkIfThereIsUTxOFreeForCollateral uTxOsAtUser
    if swCheckCollateral
        then return ()
        else PlutusContract.throwError "There is NOT UTxO free for Collateral"

--------------------------------------------------------------------------------2
