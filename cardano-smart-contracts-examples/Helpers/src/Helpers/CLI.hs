{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module Helpers.CLI where

--------------------------------------------------------------------------------2
-- External Imports
--------------------------------------------------------------------------------2

import qualified Data.Fixed as DataFixed (Fixed (MkFixed), Pico)
import qualified Data.Functor as DataFunctor
import qualified Data.List as DataList
import qualified Data.Maybe as DataMaybe
import qualified Data.String as DataString (IsString (fromString))
import qualified Data.Text as DataText
import qualified Data.Text.Internal.Search as DataTextSearch
import qualified Data.Time.Clock as DataTime
import qualified Data.Time.Clock as DataTimeClock (secondsToNominalDiffTime)
import qualified Data.Time.Clock.POSIX as DataTimeClockPOSIX (posixSecondsToUTCTime)
import qualified Data.Time.Clock.POSIX as DataTimePOSIX
import qualified Data.Time.Clock.POSIX as POSIX (utcTimeToPOSIXSeconds)
import qualified Data.Time.Format as DataTimeFormat (defaultTimeLocale, formatTime)
import qualified Ledger
import qualified Ledger.Bytes as LedgerBytes
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusTx.Builtins.Class as TxBuiltinsClass
import PlutusTx.Prelude hiding (unless)
import qualified System.Directory as SystemDirectory
import qualified System.FilePath.Posix as SystemFilePathPosix
import qualified Data.Text as T
import qualified Text.Hex as TextHex
import qualified Text.Read as TextRead (readMaybe)
import qualified Prelude as P

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Helpers.OffChain as OffChainHelpers
import qualified Helpers.OnChain as OnChainHelpers

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

formatTime :: LedgerApiV2.POSIXTime -> P.String
formatTime posixTime =
    let milisegundosFixedPico :: DataFixed.Pico
        !milisegundosFixedPico = DataFixed.MkFixed (LedgerApiV2.getPOSIXTime posixTime * 1000000000)
        !seconds = DataTimeClock.secondsToNominalDiffTime milisegundosFixedPico
     in DataTimeFormat.formatTime DataTimeFormat.defaultTimeLocale "%c" $ DataTimeClockPOSIX.posixSecondsToUTCTime seconds

-- Convert UTCTime to POSIXTime
uTCTimeToPosixTime :: DataTime.UTCTime -> DataTimePOSIX.POSIXTime
uTCTimeToPosixTime = POSIX.utcTimeToPOSIXSeconds

uTCTimeToLedgerPosixTime :: DataTime.UTCTime -> LedgerApiV2.POSIXTime
uTCTimeToLedgerPosixTime utcTime = LedgerApiV2.POSIXTime $ P.floor $ uTCTimeToPosixTime utcTime

--------------------------------------------------------------------------------2

getAmountWithMax :: P.String -> Ledger.AssetClass -> Integer -> Integer -> P.IO Integer
getAmountWithMax unit_UI unit_AC minAmount maxAmount = do
    let unit_Str = unit_UI
    -- TODO: mostrar el hex bien
    -- ++ " ("
    -- if LedgerApiV2.adaSymbol /= fst (LedgerValue.unAssetClass unit_AC) then
    --     let
    --         --  $ OffChainHelpers.stringToStrictText
    --         cs = P.show (  LedgerValue.unCurrencySymbol $ fst $ LedgerValue.unAssetClass unit_AC)
    --     in
    --         cs ++ "." ++ P.show (LedgerValue.unTokenName $ snd $ LedgerValue.unAssetClass unit_AC)
    -- else
    --     ""
    -- ++ ")"

    P.putStrLn $ "Enter amount of " ++ unit_Str ++ " (min: " ++ P.show minAmount ++ " - max: " ++ P.show maxAmount ++ "): "
    !numberSrt <- P.getLine
    P.putStrLn "--------------------------------"
    case TextRead.readMaybe numberSrt of
        Just x ->
            if x >= minAmount && x <= maxAmount
                then return x
                else do
                    P.putStrLn "Invalid input, try again"
                    P.putStrLn "--------------------------------"
                    getAmountWithMax unit_UI unit_AC minAmount maxAmount
        _ -> do
            P.putStrLn "Invalid input, try again"
            P.putStrLn "--------------------------------"
            getAmountWithMax unit_UI unit_AC minAmount maxAmount

--------------------------------------------------------------------------------2

getAmount :: P.String -> Ledger.AssetClass -> Integer -> P.IO Integer
getAmount unit_UI unit_AC minAmount = do
    let unit_Str = unit_UI
    -- TODO: mostrar el hex bien
    -- ++ " ("
    -- if LedgerApiV2.adaSymbol /= fst (LedgerValue.unAssetClass unit_AC) then
    --     let
    --         --  $ OffChainHelpers.stringToStrictText
    --         cs = P.show (  LedgerValue.unCurrencySymbol $ fst $ LedgerValue.unAssetClass unit_AC)
    --     in
    --         cs ++ "." ++ P.show (LedgerValue.unTokenName $ snd $ LedgerValue.unAssetClass unit_AC)
    -- else
    --     ""
    -- ++ ")"

    P.putStrLn $ "Enter amount of " ++ unit_Str ++ " (min: " ++ P.show minAmount ++ "): "
    !numberSrt <- P.getLine
    P.putStrLn "--------------------------------"
    case TextRead.readMaybe numberSrt of
        Just x ->
            if x >= minAmount
                then return x
                else do
                    P.putStrLn "Invalid input, try again"
                    P.putStrLn "--------------------------------"
                    getAmount unit_UI unit_AC minAmount
        _ -> do
            P.putStrLn "Invalid input, try again"
            P.putStrLn "--------------------------------"
            getAmount unit_UI unit_AC minAmount

--------------------------------------------------------------------------------2

getInt :: P.IO Integer
getInt = do
    P.putStrLn "Enter a number:"
    !numberSrt <- P.getLine
    P.putStrLn "--------------------------------"
    case TextRead.readMaybe numberSrt of
        Just x ->
            if x >= 0
                then return x
                else do
                    P.putStrLn "Invalid input, try again"
                    P.putStrLn "--------------------------------"
                    getInt
        Nothing -> do
            P.putStrLn "Invalid input, try again"
            P.putStrLn "--------------------------------"
            getInt

--------------------------------------------------------------------------------2

getIntWithDefault :: P.String -> Integer -> P.IO Integer
getIntWithDefault fieldName def = do
    P.putStrLn $ "Enter " ++ fieldName ++ " (default=" ++ P.show def ++ "):"
    !numberSrt <- P.getLine
    P.putStrLn "--------------------------------"
    case TextRead.readMaybe numberSrt of
        Just x ->
            if x >= 0
                then return x
                else do
                    P.putStrLn "Invalid input, try again"
                    P.putStrLn "--------------------------------"
                    getIntWithDefault fieldName def
        Nothing -> do
            if P.null numberSrt -- TODO check length numberSrt == 0
                then return def
                else do
                    P.putStrLn "Invalid input, try again"
                    P.putStrLn "--------------------------------"
                    getIntWithDefault fieldName def

--------------------------------------------------------------------------------2

getMaybeInt :: P.IO (Maybe Integer)
getMaybeInt = do
    P.putStrLn "Enter a number or leave empty:"
    !numberSrt <- P.getLine
    P.putStrLn "--------------------------------"
    case TextRead.readMaybe numberSrt of
        Just x ->
            if x >= 0
                then return (Just x)
                else do
                    P.putStrLn "Invalid input, try again"
                    P.putStrLn "--------------------------------"
                    getMaybeInt
        Nothing -> do
            if P.null numberSrt
                then return Nothing
                else do
                    P.putStrLn "Invalid input, try again"
                    P.putStrLn "--------------------------------"
                    getMaybeInt

--------------------------------------------------------------------------------2

getStr :: P.IO P.String
getStr = do
    !str <- P.getLine
    P.putStrLn "--------------------------------"
    if P.null str
        then do
            P.putStrLn "Invalid input, try again"
            P.putStrLn "--------------------------------"
            getStr
        else return str

--------------------------------------------------------------------------------2

getStrWithDefault :: P.String -> P.IO P.String
getStrWithDefault def = do
    !str <- P.getLine
    P.putStrLn "--------------------------------"
    if P.null str
        then return def
        else return str

--------------------------------------------------------------------------------2

getBool :: P.IO Bool
getBool = do
    !str <- P.getLine
    P.putStrLn "--------------------------------"
    if P.null str
        then getBool
        else case str of
            "y" -> return True
            "n" -> return False
            _ -> do
                P.putStrLn "Invalid input, try again"
                P.putStrLn "--------------------------------"
                getBool

--------------------------------------------------------------------------------2

getBoolWithDefault :: Bool -> P.IO Bool
getBoolWithDefault def = do
    !str <- P.getLine
    P.putStrLn "--------------------------------"
    if P.null str
        then return def
        else case str of
            "y" -> return True
            "n" -> return False
            _ -> do
                P.putStrLn "Invalid input, try again"
                P.putStrLn "--------------------------------"
                getBoolWithDefault def

--------------------------------------------------------------------------------2

getTime :: P.String -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> P.IO LedgerApiV2.POSIXTime
getTime fieldName defTime lowerLimit = do
    P.putStrLn $ "Enter " ++ fieldName ++ " (default=" ++ P.show defTime ++ "):"
    str <- P.getLine
    P.putStrLn "--------------------------------"
    if P.null str
        then do
            return defTime
        else case TextRead.readMaybe str :: Maybe Integer of
            Just x ->
                if LedgerApiV2.POSIXTime x >= lowerLimit
                    then do
                        return $ LedgerApiV2.POSIXTime x
                    else do
                        P.putStrLn "Invalid input, try again"
                        P.putStrLn "--------------------------------"
                        getTime fieldName defTime lowerLimit
            _ -> do
                P.putStrLn "Invalid input, try again"
                P.putStrLn "--------------------------------"
                getTime fieldName defTime lowerLimit

--------------------------------------------------------------------------------

-- Function to remove "0x" prefix if present
removeHexPrefix :: P.String -> P.String
removeHexPrefix txt
  | "0x" `DataList.isPrefixOf` txt = P.drop 2 txt
  | otherwise               = txt

getPkh :: P.String -> P.IO LedgerApiV2.PubKeyHash
getPkh fieldName = do
    P.putStrLn $ "Enter " ++ fieldName ++ ":"
    str <- P.getLine
    P.putStrLn "--------------------------------"
    if P.null str
        then do 
            getPkh fieldName 
        else case LedgerBytes.fromHex (DataString.fromString (removeHexPrefix str)) of
              Right (LedgerBytes.LedgerBytes bytes) ->
                  return $ LedgerApiV2.PubKeyHash bytes
              Left _ -> do
                  P.putStrLn "Invalid input, try again"
                  P.putStrLn "--------------------------------"
                  getPkh fieldName 


--------------------------------------------------------------------------------2

getUnitName :: P.IO P.String
getUnitName = do
    P.putStrLn "Enter UI Name:"
    P.putStrLn "Leave empty to use ADA (lovelace)"
    str <- P.getLine
    P.putStrLn "--------------------------------"
    if P.null str
        then return "ADA (loveLace)"
        else return str

--------------------------------------------------------------------------------2

getCurrencySymbol :: P.String -> P.IO LedgerApiV2.CurrencySymbol
getCurrencySymbol defCS_Str = do
    P.putStrLn $ "Enter Currency Symbol (Enter ADA to use lovelace - default=" ++ defCS_Str ++ "):"
    cS_Str <- P.getLine
    P.putStrLn "--------------------------------"

    let isADA = case cS_Str of
            "ADA" -> True
            _ -> False
        hex = TextHex.decodeHex $ OffChainHelpers.stringToStrictText cS_Str
        isHexOk hex' = case hex' of
            Nothing -> False
            _ -> True
        isCS_56HEX_OK = length cS_Str P.== 56 && isHexOk hex

        getCS :: P.IO LedgerApiV2.CurrencySymbol
        getCS
            | P.null cS_Str = do
                let hexDef = TextHex.decodeHex $ OffChainHelpers.stringToStrictText defCS_Str
                    hexDef_Str = DataMaybe.fromJust hexDef
                return $ LedgerApiV2.CurrencySymbol $ TxBuiltinsClass.toBuiltin hexDef_Str
            | isADA = return LedgerApiV2.adaSymbol
            | isCS_56HEX_OK = do
                let hex_Str = DataMaybe.fromJust hex
                return $ LedgerApiV2.CurrencySymbol $ TxBuiltinsClass.toBuiltin hex_Str
            | otherwise = do
                P.putStrLn "Invalid Currency Symbol, try again"
                P.putStrLn "--------------------------------"
                getCurrencySymbol defCS_Str

    getCS

--------------------------------------------------------------------------------2

getTokenName :: P.String -> Bool -> P.IO LedgerApiV2.TokenName
getTokenName defCS_Str canBeEmpty = do
    if canBeEmpty
        then do
            P.putStrLn $ "Enter TokenName (Enter ADA to use lovelace-default=" ++ defCS_Str ++ "):"
            tN_Str <- P.getLine
            P.putStrLn "--------------------------------"
            if P.null tN_Str
                then return $ LedgerApiV2.TokenName $ OffChainHelpers.stringToBuiltinByteString defCS_Str
                else
                    if tN_Str P.== "ADA"
                        then return LedgerApiV2.adaToken
                        else return $ LedgerApiV2.TokenName $ OffChainHelpers.stringToBuiltinByteString tN_Str
        else do
            P.putStrLn $ "Enter TokenName (default=" ++ defCS_Str ++ "):"
            tN_Str <- P.getLine
            P.putStrLn "--------------------------------"
            if P.null tN_Str
                then do
                    return $ LedgerApiV2.TokenName $ OffChainHelpers.stringToBuiltinByteString defCS_Str
                else do
                    return $ LedgerApiV2.TokenName $ OffChainHelpers.stringToBuiltinByteString tN_Str

--------------------------------------------------------------------------------2

selectFolder :: P.String -> P.String -> P.IO P.String
selectFolder path filterFileName = do
    P.putStrLn "Folders:"
    files <- SystemDirectory.listDirectory path
    let !filterFiles =
            case filterFileName of
                "" -> files
                _ ->
                    filter
                        ( \n -> case DataTextSearch.indices (DataString.fromString filterFileName) (DataString.fromString n) of
                            (_ : _) -> True
                            [] -> False
                        )
                        files
        ----------------
        formatList list =
            concat
                [ [P.show (n + 1 :: Integer) ++ ": " ++ P.show item]
                | (n, item) <- OnChainHelpers.enumerate list
                ]
    ----------------
    mapM_ P.putStrLn (formatList filterFiles)
    ----------------
    P.putStrLn "--------------------------------"
    P.putStrLn "0 - Cancel"
    P.putStrLn "--------------------------------"
    P.putStrLn "Enter option:"
    !numeroStr <- P.getLine
    P.putStrLn "--------------------------------"
    case TextRead.readMaybe numeroStr of
        Just 0 -> do
            P.putStrLn "--------------------------------"
            return ""
        Just n -> do
            if n <= length filterFiles && n > 0
                then do
                    let !nombre = filterFiles !! (n - 1)
                    P.putStrLn $ "Folder: " ++ nombre
                    !exist <- SystemDirectory.doesPathExist (path SystemFilePathPosix.</> nombre)
                    if exist
                        then do
                            P.putStrLn "--------------------------------"
                            return nombre
                        else do
                            P.putStrLn "Invalid input, try again"
                            P.putStrLn "--------------------------------"
                            selectFolder path filterFileName
                else do
                    P.putStrLn "Invalid input, try again"
                    P.putStrLn "--------------------------------"
                    selectFolder path filterFileName
        Nothing -> do
            P.putStrLn "Invalid input, try again"
            P.putStrLn "--------------------------------"
            selectFolder path filterFileName

--------------------------------------------------------------------------------2

printTitle :: P.String -> P.IO ()
printTitle variable = do
    let len = 80 :: Integer
    let line = OffChainHelpers.strictTextToString $ DataText.replicate (P.fromIntegral len) "-"
    let variableLine' = DataText.replicate (P.fromIntegral ((len `divide` 2) - 2 - (P.fromIntegral (P.length variable) `divide` 2))) "-"
    let variableLine =
            if P.null $ OffChainHelpers.strictTextToString variableLine'
                then variable
                else OffChainHelpers.strictTextToString variableLine' ++ " " ++ variable ++ " " ++ OffChainHelpers.strictTextToString variableLine'
    P.putStrLn ""
    P.putStrLn line
    P.putStrLn variableLine
    P.putStrLn line

--------------------------------------------------------------------------------2

printSubTitle :: P.String -> P.IO ()
printSubTitle variable = do
    let len = 80 :: Integer
    let line = OffChainHelpers.strictTextToString $ DataText.replicate (P.fromIntegral len) "-"
    P.putStrLn ""
    P.putStrLn line
    P.putStrLn variable
    P.putStrLn line

--------------------------------------------------------------------------------2

printSubSubTitle :: P.String -> P.IO ()
printSubSubTitle variable = do
    let len = 80 :: Integer
    let line = OffChainHelpers.strictTextToString $ DataText.replicate (P.fromIntegral len) "-"
    P.putStrLn ""
    P.putStrLn variable
    P.putStrLn line

--------------------------------------------------------------------------------2

nominalDiffTimeToSeconds :: DataTime.NominalDiffTime -> Integer
nominalDiffTimeToSeconds myNominalDiffTime =
    let (myNominalDiffTimeInSeconds, _) = P.properFraction myNominalDiffTime
     in myNominalDiffTimeInSeconds

--------------------------------------------------------------------------------2

getPOSIXTimeSeconds :: P.IO Integer
getPOSIXTimeSeconds = do
    DataTimePOSIX.getPOSIXTime DataFunctor.<&> nominalDiffTimeToSeconds

--------------------------------------------------------------------------------2

selectFromList :: forall a. (P.Show a, P.Eq a) => [a] -> P.IO (Maybe (Integer, a))
selectFromList list' = do
    case list' of
        [] -> do
            P.putStrLn "There is nothing to select"
            P.putStrLn "--------------------------------"
            return Nothing
        list -> do
            let formatList :: [P.String]
                formatList =
                    concat
                        [ [ "-----"
                          , P.show (1 P.+ OnChainHelpers.fromJust (DataList.elemIndex element list))
                          , P.show element
                          ]
                        | element <- list
                        ]
            ----------------
            P.putStrLn "List:"
            mapM_ P.putStrLn formatList
            P.putStrLn "--------------------------------"
            P.putStrLn "Option (0 to cancel)"
            P.putStrLn "--------------------------------"
            P.putStrLn "Enter option:"
            !opcion <- P.getLine
            P.putStrLn "--------------------------------"
            case TextRead.readMaybe opcion :: Maybe Integer of
                Just 0 -> do
                    return Nothing
                Just x -> do
                    if x >= 1 && x <= length list
                        then do
                            let !new = (x, list !! (x - 1))
                            return (Just new)
                        else do
                            P.putStrLn "Invalid input, try again"
                            P.putStrLn "--------------------------------"
                            selectFromList @a list
                _ -> do
                    P.putStrLn "Invalid input, try again"
                    P.putStrLn "--------------------------------"
                    selectFromList @a list

--------------------------------------------------------------------------------2
