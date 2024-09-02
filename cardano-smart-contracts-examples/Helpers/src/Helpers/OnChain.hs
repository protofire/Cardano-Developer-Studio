{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

--------------------------------------------------------------------------------
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module Helpers.OnChain where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified GHC.Generics              as Generic
import qualified Ledger
import qualified Ledger.Ada                as LedgerAda
import qualified Ledger.Value              as LedgerValue
import qualified Plutus.V1.Ledger.Interval as LedgerIntervalV1 (contains, from, interval, member)
import qualified Plutus.V2.Ledger.Api      as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts as LedgerContextsV2
import qualified Plutus.V2.Ledger.Tx       as LedgerTxV2 (OutputDatum (..), txOutDatum)
import qualified PlutusTx
import qualified PlutusTx.AssocMap         as TxAssocMap
import qualified PlutusTx.Builtins         as TxBuiltins
import qualified PlutusTx.Foldable         as TxFold
import           PlutusTx.Prelude
import qualified PlutusTx.Ratio            as TxRatio
import qualified Prelude                   as P

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

data SignedMessageCheckError
    = SignatureMismatch Ledger.PaymentPubKey Ledger.Signature
    -- ^ The signature did not match the public key
    deriving (Generic.Generic, P.Show)

{-# INLINABLE checkSignature #-}
-- | Verify the signature of a message
checkSignature
  :: Ledger.PaymentPubKey
  -- ^ The public key of the signatory
  -> LedgerApiV2.BuiltinByteString
  -- ^ The message
  -> Ledger.Signature
  -- ^ The signed message
  -> Either SignedMessageCheckError ()
checkSignature  pubKey signedMsg signature =
    let
        -- (LedgerApiV2.PubKeyHash pk) = pubKey
        Ledger.PaymentPubKey (Ledger.PubKey (LedgerApiV2.LedgerBytes pk)) = pubKey
        Ledger.Signature sig = signature
    in if verifyEd25519Signature pk signedMsg sig
        then Right ()
        else Left $ SignatureMismatch  pubKey  signature



--------------------------------------------------------------------------------2

{-# INLINEABLE countDistinct #-}
countDistinct :: Eq a => [a] -> Integer
countDistinct = countDistinct' []
  where
    countDistinct' _ [] = 0
    countDistinct' seen (x:xs)
      | x `elem` seen = countDistinct' seen xs
      | otherwise     = 1 + countDistinct' (x:seen) xs

--------------------------------------------------------------------------------2

{-# INLINEABLE flattenValueAdd #-}
flattenValueAdd :: (Eq a, Eq b) => [(a, b, Integer)] -> [(a, b, Integer)] -> [(a, b, Integer)]
flattenValueAdd original [] = original
flattenValueAdd original (x:xs) = flattenValueAdd (addValue original x) xs
    where
        addValue :: (Eq a, Eq b) => [(a, b, Integer)] -> (a, b, Integer) -> [(a, b, Integer)]
        addValue [] newTuple = [newTuple]
        addValue ((cs, tn, value):xs') newTuple@(cs', tn',  value')
              | cs == cs' && tn == tn' = if value + value' == 0 then xs' else (cs, tn, value +  value') : xs'
              | otherwise = (cs, tn, value) : addValue xs' newTuple


{-# INLINEABLE flattenValueSub #-}
flattenValueSub :: (Eq a, Eq b) => [(a, b, Integer)] -> [(a, b, Integer)] -> [(a, b, Integer)]
flattenValueSub original [] = original
flattenValueSub original (x:xs) = flattenValueSub (subValue original x) xs
    where
        subValue :: (Eq a, Eq b) => [(a, b, Integer)] -> (a, b, Integer) -> [(a, b, Integer)]
        subValue [] newTuple = [newTuple]
        subValue ((cs, tn, value):xs') newTuple@(cs', tn',  value')
              | cs == cs' && tn == tn' = if value - value' == 0 then xs' else (cs, tn, value -  value') : xs'
              | otherwise = (cs, tn, value) : subValue xs' newTuple

--------------------------------------------------------------------------------2

{-# INLINEABLE flattenValueToValue #-}
-- flattenValueToValue :: [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)] -> LedgerValue.Value
-- flattenValueToValue = TxAssocMap.fromList
-- flattenValueToValue [] = LedgerAda.lovelaceValueOf 0
-- flattenValueToValue ((cs, tn, value):xs) = LedgerValue.assetClassValue (LedgerValue.AssetClass (cs , tn)) value <> flattenValueToValue xs

flattenValueToValue :: [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)] -> LedgerValue.Value
flattenValueToValue list =
    -- let insertToInnerMap innerMap (tokenName, value) = TxAssocMap.insert tokenName value innerMap
    --     insertToOuterMap outerMap (currencySymbol, tokenName, value) =
    --         case TxAssocMap.lookup currencySymbol outerMap of
    --         Nothing ->
    --             let innerMap = TxAssocMap.singleton tokenName value
    --             in TxAssocMap.insert currencySymbol innerMap outerMap
    --         Just innerMap ->
    --             let updatedInnerMap = insertToInnerMap innerMap (tokenName, value)
    --             in TxAssocMap.insert currencySymbol updatedInnerMap outerMap
-- foldr insertToOuterMap TxAssocMap.empty list
    let mapElement acc cs tn value =
            case TxAssocMap.lookup cs acc of
                Nothing       -> TxAssocMap.insert cs (TxAssocMap.singleton tn value) acc
                Just innerMap -> TxAssocMap.insert cs (TxAssocMap.insert tn value innerMap) acc
        mapList = foldl (\acc (cs, tn, value) -> mapElement acc cs tn value) TxAssocMap.empty list
    in LedgerValue.Value mapList

--------------------------------------------------------------------------------2

-- {-# INLINEABLE setAndLoosePrecision'Old #-}
-- setAndLoosePrecision'Old :: Rational -> Integer -> Rational
-- setAndLoosePrecision'Old !r !n =
--     let num = (10 `powInteger` n)
--     in TxRatio.unsafeRatio (TxRatio.truncate (r * TxRatio.fromInteger  (10 `powInteger` n))) num  --fromInteger (round (r * (10 `pow` n))) / (10.0 ^^ n)

{-# INLINEABLE setAndLoosePrecision #-}
setAndLoosePrecision :: Rational -> Integer -> Rational
setAndLoosePrecision !r !n
    | TxRatio.denominator r < scaleFactor = r
    | otherwise =
        let
            !scaled = r * scaleFactorRational
            !rounded = TxRatio.truncate scaled
        in TxRatio.unsafeRatio rounded scaleFactor
    where
        !scaleFactor = 10 `powInteger` n
        !scaleFactorRational = TxRatio.fromInteger scaleFactor


{-# INLINEABLE setAndLoosePrecision1e9 #-}
setAndLoosePrecision1e9 :: Rational -> Rational
setAndLoosePrecision1e9 !r
    | TxRatio.denominator r < scaleFactor = r
    | otherwise =
        let
            !scaled = r * scaleFactorRational
            !rounded = TxRatio.truncate scaled
        in TxRatio.unsafeRatio rounded scaleFactor
    where
        !scaleFactor = 1_000_000_000
        !scaleFactorRational = TxRatio.fromInteger scaleFactor

{-# INLINEABLE setAndLoosePrecision1e6 #-}
setAndLoosePrecision1e6 :: Rational -> Rational
setAndLoosePrecision1e6 !r
    | TxRatio.denominator r < scaleFactor = r
    | otherwise =
        let
            !scaled = r * scaleFactorRational
            !rounded = TxRatio.truncate scaled
        in TxRatio.unsafeRatio rounded scaleFactor
    where
        !scaleFactor = 1_000_000
        !scaleFactorRational = TxRatio.fromInteger scaleFactor


{-# INLINEABLE setAndLoosePrecision1e3 #-}
setAndLoosePrecision1e3 :: Rational -> Rational
setAndLoosePrecision1e3 !r
    | TxRatio.denominator r < scaleFactor = r
    | otherwise =
        let
            !scaled = r * scaleFactorRational
            !rounded = TxRatio.truncate scaled
        in TxRatio.unsafeRatio rounded scaleFactor
    where
        !scaleFactor = 1_000
        !scaleFactorRational = TxRatio.fromInteger scaleFactor

{-# INLINEABLE setAndLoosePrecisionGetOnlyNum #-}
setAndLoosePrecisionGetOnlyNum :: Rational -> Integer -> Integer
setAndLoosePrecisionGetOnlyNum !r !n =
    let
        !scaleFactor = 10 `powInteger` n
        !scaleFactorRational = TxRatio.fromInteger scaleFactor
        !scaled = r * scaleFactorRational
    in TxRatio.truncate scaled

{-# INLINEABLE setAndLoosePrecision1e6GetOnlyNumerator #-}
setAndLoosePrecision1e6GetOnlyNumerator :: Rational -> Integer
setAndLoosePrecision1e6GetOnlyNumerator !r =
    let
        !scaleFactor = 1_000_000
        !scaleFactorRational = TxRatio.fromInteger scaleFactor
        !scaled = r * scaleFactorRational
    in TxRatio.truncate scaled


--------------------------------------------------------------------------------2

-- {-# INLINEABLE multiplyScientific #-}
-- multiplyScientific :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer)
-- multiplyScientific (coef1, exp1) (coef2, exp2) =
--   let resultCoef = coef1 * coef2
--       resultExp = exp1 + exp2
--   in (resultCoef, resultExp)

-- {-# INLINEABLE addScientific #-}
-- addScientific :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
-- addScientific (coef1, exp1) (coef2, exp2)
--   | expDiff == 0 = (coef1 + coef2, exp1)
--   | expDiff > 0 = (coef1 * 10 ^ expDiff + coef2, exp1)
--   | otherwise = (coef1 + coef2 * 10 ^ (-expDiff), exp2)
--   where
--     expDiff = exp1 - exp2

-- {-# INLINEABLE subtractScientific #-}
-- subtractScientific :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
-- subtractScientific (coef1, exp1) (coef2, exp2) = addScientific (coef1, exp1) (-coef2, exp2)

-- {-# INLINEABLE divideScientific #-}
-- divideScientific :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
-- divideScientific (coef1, exp1) (coef2, exp2) = (coef1 * 10 ^ (exp1 - exp2), exp1 - exp2)

--------------------------------------------------------------------------------2

{-# INLINEABLE powInteger #-}
powInteger :: Integer -> Integer -> Integer
powInteger num n
    | n == 0    = 1
    | num == 0    = 0
    | even n    = powInteger (num * num) (n `divide` 2)
    | otherwise = num * powInteger (num * num) (n `divide` 2)

{-# INLINEABLE powRational #-}
powRational :: Integer -> Integer -> Integer -> Rational
powRational !num !dem !n
    | n == 0    = TxRatio.fromInteger 1
    | num == 0  = TxRatio.fromInteger 0
    | n < 0     = TxRatio.recip $ powRational num dem (negate n)
    | otherwise = TxRatio.unsafeRatio (num `powInteger` n) (dem `powInteger` n)


-- {-# INLINEABLE powInteger'Old #-}
-- powInteger'Old :: Integer -> Integer -> Integer
-- powInteger'Old !x !n
--     | n == 0  =  1
--     | x == 0  =  0
--     | even n =
--         let !pow' = powInteger'Old x (divide n  2)
--         in pow' * pow'
--     | otherwise =
--         let !pow' = powInteger'Old x (divide (n - 1)  2)
--         in x * pow' * pow'

-- {-# INLINEABLE powRational'Old #-}
-- powRational'Old :: Rational -> Integer -> Rational
-- powRational'Old !x !n
--     | n == 0  =  TxRatio.fromInteger  1
--     | x == TxRatio.fromInteger 0  =  TxRatio.fromInteger  0
--     | even n =
--         let !pow' = powRational'Old x (divide n  2)
--         in pow' * pow'
--     | otherwise =
--         let !pow' = powRational'Old x (divide (n - 1)  2)
--         in x * pow' * pow'

--------------------------------------------------------------------------------2
-- | Function to return the Just value from a Maybe.
{-# INLINEABLE fromJust #-}
fromJust :: Maybe a -> a
fromJust (Just !valueInfo) = valueInfo
fromJust Nothing           = traceError "fromJust"

{-# INLINEABLE enumerate #-}
enumerate :: [b] -> [(Integer, b)]
enumerate !x =
    let !len = length x

        createList :: Integer -> Integer -> [Integer] -> [Integer]
        createList !n !i !list
            | n == 0 = list
            | otherwise = createList (n - 1) (i + 1) (list ++ [i])
    in  zip (createList len 0 []) x

--------------------------------------------------------------------------------2

{-# INLINEABLE isElement #-}
-- check if an element is in a list
isElement :: Eq a => a -> [a] -> Bool
isElement _ []        = False
isElement !x (y : ys) = x == y || isElement x ys

--------------------------------------------------------------------------------2

{-# INLINEABLE find' #-}
find' :: (a -> Bool) -> [a] -> Maybe a
find' _ []       = Nothing
find' f (x : xs) = if f x then Just x else find' f xs

--------------------------------------------------------------------------------2

{-# INLINEABLE findWithIdx' #-}
findWithIdx' :: (a -> Bool) -> [a] -> Maybe (Integer, a)
findWithIdx' _ [] = Nothing
findWithIdx' f l =
    let findWithIdx'' :: (a -> Bool) -> [a] -> Integer -> Maybe (Integer, a)
        findWithIdx'' _ [] _         = Nothing
        findWithIdx'' f' (x : xs) !i = if f' x then Just (i, x) else findWithIdx'' f' xs (i + 1)
    in  findWithIdx'' f l 0

-- {-# INLINABLE take #-}
-- -- | Plutus Tx version of 'Data.List.take'.
-- take :: Integer -> [a] -> [a]
-- take n _      | n <= 0 =  []
-- take _ []              =  []
-- take n (x:xs)          =  x : take (Builtins.subtractInteger n 1) xs

{-# INLINEABLE drop' #-}
drop' :: Integer -> [a] -> [a]
drop' _ [] = []
drop' n (x : xs)
    | n <= 0 = x : xs
    | otherwise = drop' (n - 1) xs

--------------------------------------------------------------------------------2

{-# INLINEABLE filter' #-}
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []       = []
filter' f (x : xs) = if f x then x : filter' f xs else filter' f xs

--------------------------------------------------------------------------------2

{-# INLINEABLE filterListWithList #-}
filterListWithList :: Eq a => [a] -> [a] -> [a]
filterListWithList xs filterList = filter (`elem` filterList) xs

--------------------------------------------------------------------------------2

-- | Take two list and compare each element of them with the given function. Remove the elements that match the function for speed future comparisons.
{-# INLINEABLE compareWithFunctionWhileRemoving #-}
compareWithFunctionWhileRemoving :: forall a b. [a] -> [b] -> (a -> b -> Bool) -> Bool
compareWithFunctionWhileRemoving xs ys f = go xs ys []
    where
        go :: [a] -> [b] -> [b] -> Bool
        go [] ys' _ = null ys'
        go (x : xs') ys' prevYs =
            case findAndRemove' x [] ys' of
                Just ys'' -> go xs' ys'' (prevYs ++ ys'')
                Nothing   -> False

        findAndRemove' :: a -> [b] -> [b] -> Maybe [b]
        findAndRemove' _ _ [] = Nothing
        findAndRemove' x prevYs (y : ys')
            | f x y = Just (prevYs ++ ys')
            | otherwise = findAndRemove' x (prevYs ++ [y]) ys'

--------------------------------------------------------------------------------2

{-# INLINEABLE isTxOutAnInput #-}
isTxOutAnInput :: LedgerApiV2.TxOutRef -> LedgerContextsV2.TxInfo -> Bool
isTxOutAnInput !txOutRef !info = any (\i -> LedgerApiV2.txInInfoOutRef i == txOutRef) $ LedgerApiV2.txInfoInputs info

--------------------------------------------------------------------------------2

{-# INLINEABLE isDateReached #-}
isDateReached :: LedgerApiV2.POSIXTime -> LedgerContextsV2.TxInfo -> Bool
isDateReached !date !info = LedgerIntervalV1.contains (LedgerIntervalV1.from date) $ LedgerApiV2.txInfoValidRange info

{-# INLINEABLE isDateNotReached #-}
isDateNotReached :: LedgerApiV2.POSIXTime -> LedgerContextsV2.TxInfo -> Bool
isDateNotReached !date !info = not (isDateReached date info)

--------------------------------------------------------------------------------2

{- Check if the date is correct.  -}
{-# INLINEABLE isDateInRange #-}
isDateInRange :: LedgerApiV2.POSIXTime -> LedgerContextsV2.TxInfo -> Bool
isDateInRange !dateAt !info =
    -- XXX: +1 because timeRange lower closure may be False
    (dateAt + 1) `LedgerIntervalV1.member` LedgerApiV2.txInfoValidRange info

--------------------------------------------------------------------------------

{- Check that the tx range interval of validity. Must be lees than T.validTimeRange Pool Param. -}
{-# INLINABLE isValidRange #-}
isValidRange :: LedgerContextsV2.TxInfo -> LedgerApiV2.POSIXTime -> Bool
isValidRange !info = isCorrectIntervalSize (LedgerApiV2.txInfoValidRange info)

--------------------------------------------------------------------------------2

{-# INLINEABLE getLowerBoundFromInterval #-}
getLowerBoundFromInterval :: LedgerApiV2.Interval a -> Maybe a
getLowerBoundFromInterval !iv = case LedgerApiV2.ivFrom iv of
    LedgerApiV2.LowerBound (LedgerApiV2.Finite lBound) _ -> Just lBound
    _                                                    -> Nothing

--------------------------------------------------------------------------------2

{-# INLINEABLE isCorrectIntervalSize #-}
isCorrectIntervalSize :: LedgerApiV2.Interval LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Bool
isCorrectIntervalSize iv len =
    case getLowerBoundFromInterval iv of
        Just t  -> LedgerIntervalV1.interval t (t + len) `LedgerIntervalV1.contains` iv
        Nothing -> False

--------------------------------------------------------------------------------2

{-# INLINEABLE zeroToBBS #-}
zeroToBBS :: BuiltinByteString
zeroToBBS = emptyByteString -- intToBBS 0

{-# INLINEABLE intToBBS #-}
intToBBS :: Integer -> BuiltinByteString
intToBBS !x
    -- 45 is ASCII code for '-'
    | x < 0 = consByteString 45 $ intToBBS (negate x)
    -- x is single-digit
    | x `quotient` 10 == 0 = digitToBS x
    | otherwise = intToBBS (x `quotient` 10) <> digitToBS (x `remainder` 10)
    where
        digitToBS :: Integer -> BuiltinByteString
        -- 48 is ASCII code for '0'
        digitToBS !d = consByteString (d + 48) emptyByteString

{-# INLINEABLE txOutRefToBBS #-}
txOutRefToBBS :: LedgerApiV2.TxOutRef -> BuiltinByteString
txOutRefToBBS !txOutRef =
    let idTxOut = LedgerApiV2.txOutRefId txOutRef
        indexTxOut = LedgerApiV2.txOutRefIdx txOutRef
    in  intToBBS indexTxOut <> LedgerApiV2.getTxId idTxOut

{-# INLINEABLE flatValueToBBS #-}
flatValueToBBS :: LedgerApiV2.CurrencySymbol -> LedgerApiV2.TokenName -> Integer -> BuiltinByteString
flatValueToBBS !cs !tn !am = LedgerApiV2.unCurrencySymbol cs <> LedgerApiV2.unTokenName tn <> intToBBS am

{-# INLINEABLE txValueToBBS #-}
txValueToBBS :: LedgerApiV2.Value -> BuiltinByteString
txValueToBBS !value = foldl (<>) zeroToBBS [flatValueToBBS cs tn am | (cs, tn, am) <- LedgerValue.flattenValue value]

{-# INLINEABLE assetClassToBBS #-}
assetClassToBBS :: LedgerValue.AssetClass -> BuiltinByteString
assetClassToBBS !ac =
    let !(cs, tn) = LedgerValue.unAssetClass ac
    in  LedgerApiV2.unCurrencySymbol cs <> LedgerApiV2.unTokenName tn

{-# INLINEABLE pOSIXTimeToBBS #-}
pOSIXTimeToBBS :: LedgerApiV2.POSIXTime -> BuiltinByteString
pOSIXTimeToBBS !time = intToBBS $ LedgerApiV2.getPOSIXTime time

--------------------------------------------------------------------------------2

-- | Get the total value of an Aseets Class in the 'Value'.
{-# INLINEABLE getValue_With_AC_InValue #-}
getValue_With_AC_InValue :: LedgerValue.Value -> LedgerValue.AssetClass -> LedgerValue.Value
getValue_With_AC_InValue !v !ac =
    LedgerValue.assetClassValue ac (LedgerValue.assetClassValueOf v ac)

-- | Get the total value of an Aseets Class in the 'Value'.
{-# INLINEABLE getAmt_With_AC_InValue #-}
getAmt_With_AC_InValue :: LedgerValue.Value -> LedgerValue.AssetClass -> Integer
getAmt_With_AC_InValue !v !ac = LedgerValue.assetClassValueOf v ac

--------------------------------------------------------------------------------2

-- | Get the total value of lovelace in the 'Value'.
{-# INLINEABLE getValueOfLovelace #-}
getValueOfLovelace :: LedgerValue.Value -> Integer
getValueOfLovelace !v =
    LedgerAda.getLovelace $ LedgerAda.fromValue v

--------------------------------------------------------------------------------2

-- | Get the total value of a CurrencySymbol in the 'Value'. Doesnt matter the TokenName
{-# INLINEABLE getValueOfCurrencySymbol #-}
getValueOfCurrencySymbol :: LedgerValue.Value -> LedgerApiV2.CurrencySymbol -> LedgerValue.Value
getValueOfCurrencySymbol (LedgerValue.Value !mp) !cs =
    case TxAssocMap.lookup cs mp of
        Nothing  -> LedgerAda.lovelaceValueOf 0
        Just mp' -> LedgerValue.Value $ TxAssocMap.singleton cs mp'

--------------------------------------------------------------------------------2

-- | Get the amount of the given CurrencySymbol in the 'Value'. Doesnt matter the TokenName
{-# INLINEABLE getAmtOfCurrencySymbol #-}
getAmtOfCurrencySymbol :: LedgerValue.Value -> LedgerApiV2.CurrencySymbol -> Integer
getAmtOfCurrencySymbol !value !cs =
    TxFold.foldl (+) 0 [am | (cs', _, am) <- flattenValue value, cs' == cs]

--------------------------------------------------------------------------------2

-- | Get the amount of the given TokenName in the 'Value'. Doesnt matter the CurrencySymbol
{-# INLINEABLE getAmtOfTokenName #-}
getAmtOfTokenName :: LedgerValue.Value -> LedgerApiV2.TokenName -> Integer
getAmtOfTokenName !value !tn =
    TxFold.foldl (+) 0 [am | (_, tn', am) <- flattenValue value, tn' == tn]

--------------------------------------------------------------------------------2

-- | Get the CurrencySymbol of the given TokenName in the 'Value'.
{-# INLINEABLE getCurrencySymbol_Of_TokenName_InValue #-}
getCurrencySymbol_Of_TokenName_InValue :: LedgerValue.Value -> LedgerApiV2.TokenName -> LedgerApiV2.CurrencySymbol
getCurrencySymbol_Of_TokenName_InValue !value !tn =
    head [cs | (cs, tn', _) <- flattenValue value, tn' == tn]

--------------------------------------------------------------------------------2

{-# INLINEABLE isToken_With_AC_InValue #-}
isToken_With_AC_InValue :: LedgerApiV2.Value -> LedgerValue.AssetClass -> Bool
isToken_With_AC_InValue !value !ac = LedgerValue.assetClassValueOf value ac >= 1

{-# INLINEABLE isToken_With_AC_And_Negative_Amt_InValue #-}
isToken_With_AC_And_Negative_Amt_InValue :: LedgerApiV2.Value -> LedgerValue.AssetClass -> Bool
isToken_With_AC_And_Negative_Amt_InValue !value !ac = LedgerValue.assetClassValueOf value ac < 0

{-# INLINEABLE isToken_With_TN_InValue #-}
isToken_With_TN_InValue :: LedgerApiV2.Value -> LedgerApiV2.TokenName -> Bool
isToken_With_TN_InValue !value !tn = getAmtOfTokenName value tn >= 1

{-# INLINEABLE isToken_With_CS_InValue #-}
isToken_With_CS_InValue :: LedgerApiV2.Value -> LedgerApiV2.CurrencySymbol -> Bool
isToken_With_CS_InValue !value !cs = getAmtOfCurrencySymbol value cs >= 1

{-# INLINEABLE isToken_With_AC_AndAmt_InValue #-}
isToken_With_AC_AndAmt_InValue :: LedgerApiV2.Value -> LedgerValue.AssetClass -> Integer -> Bool
isToken_With_AC_AndAmt_InValue !value !ac !amt = LedgerValue.assetClassValueOf value ac == amt

{-# INLINEABLE isToken_With_CS_AndAmt_InValue #-}
isToken_With_CS_AndAmt_InValue :: LedgerApiV2.Value -> LedgerValue.CurrencySymbol -> Integer -> Bool
isToken_With_CS_AndAmt_InValue !value !cs !amt = getAmtOfCurrencySymbol value cs == amt

--------------------------------------------------------------------------------2

{-# INLINEABLE isNFT_With_AC_InValue #-}
isNFT_With_AC_InValue :: LedgerApiV2.Value -> LedgerValue.AssetClass -> Bool
isNFT_With_AC_InValue !value !ac = LedgerValue.assetClassValueOf value ac == 1

{-# INLINEABLE isNFT_With_TN_InValue #-}
isNFT_With_TN_InValue :: LedgerApiV2.Value -> LedgerApiV2.TokenName -> Bool
isNFT_With_TN_InValue !value !tn = getAmtOfTokenName value tn == 1

{-# INLINEABLE isNFT_With_CS_InValue #-}
isNFT_With_CS_InValue :: LedgerApiV2.Value -> LedgerApiV2.CurrencySymbol -> Bool
isNFT_With_CS_InValue !value !cs = getAmtOfCurrencySymbol value cs == 1

--------------------------------------------------------------------------------2

-- | Check if there is any token minting with the right asset class.
{-# INLINEABLE isNFT_Minting_With_AC #-}
isNFT_Minting_With_AC :: LedgerValue.AssetClass -> LedgerContextsV2.TxInfo -> Bool
isNFT_Minting_With_AC !ac !info =
    let !mintingValue = LedgerApiV2.txInfoMint info
    in  isNFT_With_AC_InValue mintingValue ac

--------------------------------------------------------------------------------2

-- | Check if there is any token minting with the currecy symbol, donsent matter the token name
{-# INLINEABLE isNFT_Minting_With_CS #-}
isNFT_Minting_With_CS :: LedgerApiV2.CurrencySymbol -> LedgerContextsV2.TxInfo -> Bool
isNFT_Minting_With_CS !currencySymbol !info =
    let !mintingValue = LedgerApiV2.txInfoMint info
    in  isNFT_With_CS_InValue mintingValue currencySymbol

--------------------------------------------------------------------------------2

-- | Check if there is any token minting with the right token name, donsent matter the currency symbol .
{-# INLINEABLE isNFT_Minting_With_TN #-}
isNFT_Minting_With_TN :: LedgerApiV2.TokenName -> LedgerContextsV2.TxInfo -> Bool
isNFT_Minting_With_TN !tokenName !info =
    let !mintingValue = LedgerApiV2.txInfoMint info
    in  isNFT_With_TN_InValue mintingValue tokenName

--------------------------------------------------------------------------------2

-- | Check if there is any token minting with the right asset class and amount.
{-# INLINEABLE isToken_Minting_With_AC_AndAmt #-}
isToken_Minting_With_AC_AndAmt :: LedgerValue.AssetClass -> Integer -> LedgerContextsV2.TxInfo -> Bool
isToken_Minting_With_AC_AndAmt !ac !amt !info =
    let !mintingValue = LedgerApiV2.txInfoMint info
    in  isToken_With_AC_AndAmt_InValue mintingValue ac amt

-- | Check if there is any token minting with right currecy symbol and amount.
{-# INLINEABLE isToken_Minting_With_CS_AndAmt #-}
isToken_Minting_With_CS_AndAmt :: LedgerValue.CurrencySymbol -> Integer -> LedgerContextsV2.TxInfo -> Bool
isToken_Minting_With_CS_AndAmt !cs !amt !info =
    let !mintingValue = LedgerApiV2.txInfoMint info
    in  isToken_With_CS_AndAmt_InValue mintingValue cs amt

--------------------------------------------------------------------------------2

-- | Check if there is any token minting with the right asset class, donsent matter the token name and amount.
{-# INLINEABLE isToken_Minting_With_AC #-}
isToken_Minting_With_AC :: LedgerValue.AssetClass -> LedgerContextsV2.TxInfo -> Bool
isToken_Minting_With_AC !ac !info =
    let !mintingValue = LedgerApiV2.txInfoMint info
    in  isToken_With_AC_InValue mintingValue ac

--------------------------------------------------------------------------------2

{-# INLINEABLE isToken_Minting_With_CS #-}
isToken_Minting_With_CS :: LedgerApiV2.CurrencySymbol -> LedgerContextsV2.TxInfo -> Bool
isToken_Minting_With_CS !cs !info =
    let !mintingValue = LedgerApiV2.txInfoMint info
    in  isToken_With_CS_InValue mintingValue cs

--------------------------------------------------------------------------------2

-- | Check if there is any token burning with the right asset class and amount.
{-# INLINEABLE isToken_Burning_With_AC_AndAmt #-}
isToken_Burning_With_AC_AndAmt :: LedgerValue.AssetClass -> Integer -> LedgerContextsV2.TxInfo -> Bool
isToken_Burning_With_AC_AndAmt !ac !amt !info =
    let !mintingValue = LedgerApiV2.txInfoMint info
    in  isToken_With_AC_AndAmt_InValue mintingValue ac amt

-- | Check if there is any token burning with right currecy symbol and amount.
{-# INLINEABLE isToken_Burning_With_CS_AndAmt #-}
isToken_Burning_With_CS_AndAmt :: LedgerValue.CurrencySymbol -> Integer -> LedgerContextsV2.TxInfo -> Bool
isToken_Burning_With_CS_AndAmt !cs !amt !info =
    let !mintingValue = LedgerApiV2.txInfoMint info
    in  isToken_With_CS_AndAmt_InValue mintingValue cs amt

--------------------------------------------------------------------------------22

-- | Check if there is any token minting with the right asset class.
{-# INLINEABLE isNFT_Burning_With_AC #-}
isNFT_Burning_With_AC :: LedgerValue.AssetClass -> LedgerContextsV2.TxInfo -> Bool
isNFT_Burning_With_AC !ac !info =
    let !mintingValue = LedgerApiV2.txInfoMint info
    in  isToken_With_AC_AndAmt_InValue mintingValue ac (negate 1)

--------------------------------------------------------------------------------2

-- | Check if there is any token burning with the currecy symbol, donsent matter the token name
{-# INLINEABLE isNFT_Burning_With_CS #-}
isNFT_Burning_With_CS :: LedgerApiV2.CurrencySymbol -> LedgerContextsV2.TxInfo -> Bool
isNFT_Burning_With_CS !currencySymbol !info =
    let !mintingValue = LedgerApiV2.txInfoMint info
    in  isToken_With_CS_AndAmt_InValue mintingValue currencySymbol (negate 1)

--------------------------------------------------------------------------------2

-- | Check if there is any token burning with the right asset class.
{-# INLINEABLE isToken_Burning_With_AC #-}
isToken_Burning_With_AC :: LedgerValue.AssetClass -> LedgerContextsV2.TxInfo -> Bool
isToken_Burning_With_AC !ac !info =
    let !mintingValue = LedgerApiV2.txInfoMint info
    in  isToken_With_AC_And_Negative_Amt_InValue mintingValue ac

--------------------------------------------------------------------------------2

{-# INLINEABLE isSignedByAny #-}
isSignedByAny :: [LedgerApiV2.PubKeyHash] -> LedgerContextsV2.TxInfo -> Bool
isSignedByAny !pubKeyHashes !txInfo =
    any (LedgerContextsV2.txSignedBy txInfo) pubKeyHashes

{-# INLINEABLE isSignedByAll #-}
isSignedByAll :: [LedgerApiV2.PubKeyHash] -> LedgerContextsV2.TxInfo -> Bool
isSignedByAll !pubKeyHashes !txInfo =
    all (LedgerContextsV2.txSignedBy txInfo) pubKeyHashes

--------------------------------------------------------------------------------2

-- {-# INLINEABLE flattenValue #-}
-- flattenValue :: LedgerValue.Value -> [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)]
-- flattenValue (LedgerValue.Value !mp) =
--     let !f1 = TxAssocMap.toList mp
--         !f2 = [(cs, TxAssocMap.toList mp') | (cs, mp') <- f1]
--         !f3 = [(cs, tn, amt) | (cs, f4) <- f2, (tn, amt) <- f4]
--     in  f3

{-# INLINEABLE flattenValue #-}
flattenValue :: LedgerValue.Value -> [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)]
flattenValue = flattenValueWithoutZeros

--------------------------------------------------------------------------------2

{-# INLINEABLE flattenValueWithoutZeros #-}
flattenValueWithoutZeros :: LedgerValue.Value -> [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)]
flattenValueWithoutZeros (LedgerValue.Value !mp) =
    let !f1 = TxAssocMap.toList mp
        !f2 = [(cs, TxAssocMap.toList mp') | (cs, mp') <- f1]
        !f3 = [(cs, tn, amt) | (cs, f4) <- f2, (tn, amt) <- f4, amt /= 0]
    in  f3

---------------------------------------------------


{-# INLINEABLE isEqFlattenValue #-}
isEqFlattenValue :: [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)] -> [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)] -> Bool
isEqFlattenValue a b = TxBuiltins.serialiseData (LedgerApiV2.toBuiltinData a) == TxBuiltins.serialiseData (LedgerApiV2.toBuiltinData b)
    -- TODO : check if this is correct

---------------------------------------------------

{-# INLINEABLE isEqListTN #-}
isEqListTN :: [(LedgerApiV2.TokenName, Integer)] -> [(LedgerApiV2.TokenName, Integer)] -> Bool
isEqListTN [] [] = True
isEqListTN [] ((_, !am2) : (!xs2)) =
    am2 == 0 && isEqListTN [] xs2
isEqListTN ((_, !am1) : (!xs1)) [] =
    am1 == 0 && isEqListTN xs1 []
isEqListTN ((!tn1, !am1) : (!xs1)) ((!tn2, !am2) : (!xs2))
    | am1 == 0 =
        isEqListTN xs1 ((tn2, am2) : xs2)
    | am2 == 0 =
        isEqListTN ((tn1, am1) : xs1) xs2
    | tn1 == tn2 && am1 == am2 =
        isEqListTN xs1 xs2
    | otherwise =
        let isEqListTN' :: (LedgerApiV2.TokenName, Integer) -> [(LedgerApiV2.TokenName, Integer)] -> [(LedgerApiV2.TokenName, Integer)] -> [(LedgerApiV2.TokenName, Integer)] -> Bool
            isEqListTN' (!tn1', !am1') !xs1' !xs2' ((!tn2', !am2') : (!xs3'))
                | am2' == 0 = isEqListTN' (tn1', am1') xs1' xs2' xs3'
                | tn1' == tn2' && am1' == am2' = isEqListTN xs1' (xs2' ++ xs3')
                | otherwise = isEqListTN' (tn1', am1') xs1' ((tn2', am2') : xs2') xs3'
            isEqListTN' _ _ _ _ = False
        in  isEqListTN' (tn1, am1) xs1 [(tn2, am2)] xs2

---------------------------------------------------

{-# INLINEABLE isIncludeListTN #-}
isIncludeListTN :: [(LedgerApiV2.TokenName, Integer)] -> [(LedgerApiV2.TokenName, Integer)] -> Bool
isIncludeListTN [] [] = True
isIncludeListTN _ [] = True
isIncludeListTN [] ((_, !am2) : (!xs2)) =
    am2 == 0 && isIncludeListTN [] xs2
isIncludeListTN ((!tn1, !am1) : (!xs1)) ((!tn2, !am2) : (!xs2))
    | am1 == 0 =
        isIncludeListTN xs1 ((tn2, am2) : xs2)
    | am2 == 0 =
        isIncludeListTN ((tn1, am1) : xs1) xs2
    | tn1 == tn2 && am1 >= am2 =
        isIncludeListTN xs1 xs2
    | otherwise =
        let listTNIsIncludesListTN' :: (LedgerApiV2.TokenName, Integer) -> [(LedgerApiV2.TokenName, Integer)] -> [(LedgerApiV2.TokenName, Integer)] -> [(LedgerApiV2.TokenName, Integer)] -> Bool
            listTNIsIncludesListTN' (!tn2', !am2') !xs2' !xs1' ((!tn1', !am1') : (!xs3'))
                | am1' == 0 = listTNIsIncludesListTN' (tn2', am2') xs2' xs1' xs3'
                | tn1' == tn2' && am1' >= am2' = isIncludeListTN (xs1' ++ xs3') xs2'
                | otherwise = listTNIsIncludesListTN' (tn2', am2') xs2' ((tn1', am1') : xs1') xs3'
            listTNIsIncludesListTN' _ _ _ _ = False
        in  listTNIsIncludesListTN' (tn2, am2) xs2 [(tn1, am1)] xs1

---------------------------------------------------

{-# INLINEABLE isEqListCS #-}
isEqListCS :: [(LedgerApiV2.CurrencySymbol, TxAssocMap.Map LedgerApiV2.TokenName Integer)] -> [(LedgerApiV2.CurrencySymbol, TxAssocMap.Map LedgerApiV2.TokenName Integer)] -> Bool
isEqListCS [] [] = True
isEqListCS ((_, !mp1) : (!xs1)) [] =
    let !listTN1 = TxAssocMap.toList mp1
    in  isEqListTN listTN1 [] && isEqListCS xs1 []
isEqListCS [] ((_, !mp2) : (!xs2)) =
    let !listTN2 = TxAssocMap.toList mp2
    in  isEqListTN [] listTN2 && isEqListCS [] xs2
isEqListCS ((!cs1, !mp1) : (!xs1)) ((!cs2, !mp2) : (!xs2))
    | cs1 == cs2 =
        let !listTN1 = TxAssocMap.toList mp1
            !listTN2 = TxAssocMap.toList mp2
        in  isEqListTN listTN1 listTN2 && isEqListCS xs1 xs2
    | otherwise =
        let isEqListCS' :: (LedgerApiV2.CurrencySymbol, TxAssocMap.Map LedgerApiV2.TokenName Integer) -> [(LedgerApiV2.CurrencySymbol, TxAssocMap.Map LedgerApiV2.TokenName Integer)] -> [(LedgerApiV2.CurrencySymbol, TxAssocMap.Map LedgerApiV2.TokenName Integer)] -> [(LedgerApiV2.CurrencySymbol, TxAssocMap.Map LedgerApiV2.TokenName Integer)] -> Bool

            isEqListCS' (!_, !mp1') !xs1' !xs2' [] =
                let !listTN1 = TxAssocMap.toList mp1'
                in  isEqListTN listTN1 [] && isEqListCS xs1' xs2'
            isEqListCS' (!cs1', !mp1') !xs1' !xs2' ((!cs2', !mp2') : xs3') =
                if cs1' == cs2'
                    then
                        let !listTN1 = TxAssocMap.toList mp1'
                            !listTN2 = TxAssocMap.toList mp2'
                        in  isEqListTN listTN1 listTN2 && isEqListCS xs1' (xs2' ++ xs3')
                    else isEqListCS' (cs1', mp1') xs1' ((cs2', mp2') : xs2') xs3'
        in  isEqListCS' (cs1, mp1) xs1 [(cs2, mp2)] xs2

---------------------------------------------------

{-# INLINEABLE isIncludeListCS #-}
isIncludeListCS :: [(LedgerApiV2.CurrencySymbol, TxAssocMap.Map LedgerApiV2.TokenName Integer)] -> [(LedgerApiV2.CurrencySymbol, TxAssocMap.Map LedgerApiV2.TokenName Integer)] -> Bool
isIncludeListCS [] [] = True
isIncludeListCS _ [] = True
isIncludeListCS [] ((_, !mp2) : (!xs2)) =
    let !listTN2 = TxAssocMap.toList mp2
    in  isIncludeListTN [] listTN2 && isIncludeListCS [] xs2
isIncludeListCS ((!cs1, !mp1) : (!xs1)) ((!cs2, !mp2) : (!xs2))
    | cs1 == cs2 =
        let !listTN1 = TxAssocMap.toList mp1
            !listTN2 = TxAssocMap.toList mp2
        in  isIncludeListTN listTN1 listTN2 && isIncludeListCS xs1 xs2
    | otherwise =
        let listCSIsIncludesListCS' :: (LedgerApiV2.CurrencySymbol, TxAssocMap.Map LedgerApiV2.TokenName Integer) -> [(LedgerApiV2.CurrencySymbol, TxAssocMap.Map LedgerApiV2.TokenName Integer)] -> [(LedgerApiV2.CurrencySymbol, TxAssocMap.Map LedgerApiV2.TokenName Integer)] -> [(LedgerApiV2.CurrencySymbol, TxAssocMap.Map LedgerApiV2.TokenName Integer)] -> Bool

            listCSIsIncludesListCS' (!_, !mp2') !xs2' !xs1' [] =
                let !listTN2 = TxAssocMap.toList mp2'
                in  isIncludeListTN [] listTN2 && isIncludeListCS xs1' xs2'
            listCSIsIncludesListCS' (!cs2', !mp2') !xs2' !xs1' ((!cs1', !mp1') : xs3') =
                if cs1' == cs2'
                    then
                        let !listTN1 = TxAssocMap.toList mp1'
                            !listTN2 = TxAssocMap.toList mp2'
                        in  isIncludeListTN listTN1 listTN2 && isIncludeListCS (xs1' ++ xs3') xs2'
                    else listCSIsIncludesListCS' (cs2', mp2') xs2' ((cs1', mp1') : xs1') xs3'
        in  listCSIsIncludesListCS' (cs2, mp2) xs2 [(cs1, mp1)] xs1

--------------------------------------------------------------------------------2

{-# INLINEABLE isIncludeValue' #-}
isIncludeValue' :: LedgerApiV2.Value -> LedgerApiV2.Value -> Bool
isIncludeValue' !value !valueToFind =
    let !valueToFind' = flattenValue valueToFind
    in  all
            ( \(cs, tn, amount) ->
                let !ac = LedgerValue.AssetClass (cs, tn)
                in  LedgerValue.assetClassValueOf value ac >= amount
            )
            valueToFind'

{-# INLINEABLE isIncludeValue #-}
isIncludeValue :: LedgerApiV2.Value -> LedgerApiV2.Value -> Bool
isIncludeValue (LedgerValue.Value !mp1) (LedgerValue.Value !mpToFind2) =
    let !listCS1 = TxAssocMap.toList mp1
        !listToFindCS2 = TxAssocMap.toList mpToFind2
    in  listCS1 `isIncludeListCS` listToFindCS2

--------------------------------------------------------------------------------2

{-# INLINEABLE isEqValue #-}
isEqValue :: LedgerApiV2.Value -> LedgerApiV2.Value -> Bool
isEqValue (LedgerValue.Value !mp1) (LedgerValue.Value !mp2) =
    let !listCS1 = TxAssocMap.toList mp1
        !listCS2 = TxAssocMap.toList mp2
    in  listCS1 `isEqListCS` listCS2

--------------------------------------------------------------------------------2

{-# INLINEABLE isEqValuesAndDatums #-}
isEqValuesAndDatums :: LedgerApiV2.ToData d => [(LedgerApiV2.Value, d)] -> [(LedgerApiV2.Value, d)] -> Bool
isEqValuesAndDatums !valuesAndDatums1 !valuesAndDatums2 =
    let valuesAndDatumsEqualsValuesAndDatums1 :: LedgerApiV2.ToData d => [(LedgerApiV2.Value, d)] -> [(LedgerApiV2.Value, d)] -> Bool
        valuesAndDatumsEqualsValuesAndDatums1 [] [] = True
        valuesAndDatumsEqualsValuesAndDatums1 ((v1, d1) : xs1) ((v2, d2) : xs2)
            | v1 `isEqValue` v2 && d1 `isUnsafeEqDatums` d2 =
                valuesAndDatumsEqualsValuesAndDatums1 xs1 xs2
            | otherwise =
                flattenValuesAndDatumsEqualsFlattenValuesAndDatums2 (v1, d1) xs1 [(v2, d2)] xs2
        valuesAndDatumsEqualsValuesAndDatums1 _ _ = False

        flattenValuesAndDatumsEqualsFlattenValuesAndDatums2 :: LedgerApiV2.ToData d => (LedgerApiV2.Value, d) -> [(LedgerApiV2.Value, d)] -> [(LedgerApiV2.Value, d)] -> [(LedgerApiV2.Value, d)] -> Bool
        flattenValuesAndDatumsEqualsFlattenValuesAndDatums2 (v1, d1) xs1 xs2 ((v2, d2) : xs3)
            | v1 `isEqValue` v2 && d1 `isUnsafeEqDatums` d2 =
                valuesAndDatumsEqualsValuesAndDatums1 xs1 (xs2 ++ xs3)
            | otherwise =
                flattenValuesAndDatumsEqualsFlattenValuesAndDatums2 (v1, d1) xs1 ((v2, d2) : xs2) xs3
        flattenValuesAndDatumsEqualsFlattenValuesAndDatums2 _ _ _ _ = False
    in  valuesAndDatums1 `valuesAndDatumsEqualsValuesAndDatums1` valuesAndDatums2

--------------------------------------------------------------------------------2

-- | tienen que estar normalizados, o sea, mismo orden y mismos campos
{-# INLINEABLE isUnsafeEqDatums #-}
isUnsafeEqDatums :: (PlutusTx.ToData d) => d -> d -> Bool
isUnsafeEqDatums !dat1 !dat2 =
    TxBuiltins.serialiseData (LedgerApiV2.toBuiltinData dat1) == TxBuiltins.serialiseData (LedgerApiV2.toBuiltinData dat2)

--------------------------------------------------------------------------------2

{-# INLINEABLE getUnsafeOwnMintingTokenNameAndAmt #-}
getUnsafeOwnMintingTokenNameAndAmt :: LedgerContextsV2.ScriptContext -> [(LedgerApiV2.TokenName, Integer)]
getUnsafeOwnMintingTokenNameAndAmt !ctx =
    let !cs = LedgerContextsV2.ownCurrencySymbol ctx
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        !flatten = TxAssocMap.lookup cs (LedgerApiV2.getValue $ LedgerApiV2.txInfoMint info)
    in  TxAssocMap.toList $ fromJust flatten

--------------------------------------------------------------------------------2

{-# INLINEABLE getUnsafeOwnMintingValue #-}
getUnsafeOwnMintingValue :: LedgerContextsV2.ScriptContext -> LedgerValue.Value
getUnsafeOwnMintingValue !ctx =
    let !cs = LedgerContextsV2.ownCurrencySymbol ctx
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        !flatten = TxAssocMap.lookup cs (LedgerApiV2.getValue $ LedgerApiV2.txInfoMint info)
        !list = TxAssocMap.toList $ fromJust flatten
        makeVal (tn, am) = LedgerValue.assetClassValue (LedgerValue.AssetClass (cs, tn)) am
    in  foldl (<>) (LedgerAda.lovelaceValueOf 0)  ((\(tn, am) -> makeVal (tn, am)) <$> list)

--------------------------------------------------------------------------------2

{-# INLINEABLE createValueAddingTokensOfCurrencySymbol #-}
createValueAddingTokensOfCurrencySymbol :: LedgerValue.AssetClass -> LedgerApiV2.CurrencySymbol -> Bool -> LedgerApiV2.Value -> Integer -> LedgerApiV2.Value
createValueAddingTokensOfCurrencySymbol !ac !cs !acIsWithoutTokenName !value !cantidad =
    if not acIsWithoutTokenName
        then LedgerValue.assetClassValue ac cantidad
        else
            let !tokenOfCurrencySymbol = [(tn, am) | (cs', tn, am) <- flattenValue value, cs' == cs]

                compareTokenName :: (LedgerApiV2.TokenName, Integer) -> (LedgerApiV2.TokenName, Integer) -> Ordering
                compareTokenName (!tn1, _) (!tn2, _)
                    | tn1 < tn2 = LT
                    | otherwise = GT

                !tokenOfCurrencySymbol_Ordered = sortBy compareTokenName tokenOfCurrencySymbol

                sumarTokens :: [(LedgerApiV2.TokenName, Integer)] -> Integer -> LedgerApiV2.Value
                sumarTokens [] !left =
                    if left > 0
                        then do
                            traceError "createValueAddingTokensOfCurrencySymbol"
                        else LedgerAda.lovelaceValueOf 0
                sumarTokens !list !left =
                    let (tn, am) = head list
                        !harvest_AC = LedgerValue.AssetClass (cs, tn)
                    in  if am > left
                            then LedgerValue.assetClassValue harvest_AC left
                            else LedgerValue.assetClassValue harvest_AC am <> sumarTokens (tail list) (left - am)
            in  sumarTokens tokenOfCurrencySymbol_Ordered cantidad

--------------------------------------------------------------------------------2

{-# INLINEABLE calculateMinADA #-}
calculateMinADA :: Integer -> Integer -> Integer -> Bool -> Integer
calculateMinADA !numAssets !sumAssetNameLengths !numPIDs !isHash =
    let -- const numPIDs=1
        -- The number of policy scripts referenced in the UTxO. If there is only one type of token in the UTxO, then this is just 1.
        -- var numAssets=1
        -- The number of asset names present in the UTxO. If there is only one type of token, then this is just 1.
        -- const sumAssetNameLengths=32
        -- Bytes    The number of bytes needed to store all of the asset names. If these do not include unicode characters (e.g., emojis), then this is just the total number of letters (characters) in the asset names. For instance, a token named "Test" needs 4 bytes for its name.

        -- Fixed parameters
        !minUTxOValue = 1000000 :: Integer
        -- ADA	The minimum number of ADA that must be present in ADA-only UTxOs.
        !pidSize = 28
        -- Bytes	The number of bytes in a policy ID.
        !coinSize = 2
        -- Bytes	At the Alonzo HFC, this parameter was corrected to be 2 because the original value 0 was an implementation error.
        !uTxOEntrySizeWithoutVal = 27
        -- Bytes	The number of bytes in a transaction if there were no value at all in it.
        !adaOnlyUTxOSize = uTxOEntrySizeWithoutVal + coinSize
        -- Bytes	The number of bytes in a transaction if it were to only contain ADA.
        !coinsPerUTxOWord = TxRatio.truncate $ TxRatio.unsafeRatio minUTxOValue adaOnlyUTxOSize -- = 34482
        -- coinsPerUTxOByte =  TxRatio.truncate $ TxRatio.unsafeRatio coinsPerUTxOWord  8 -- = 4310.25
        !hash = if isHash then (10 :: Integer) else 0 -- si hay data hash suman 10 words
        roundupBytesToWords :: Integer -> Integer
        roundupBytesToWords !number = TxRatio.truncate (TxRatio.unsafeRatio (number + 7) 8)

        !sizeWords = 6 + roundupBytesToWords (numAssets * 12 + sumAssetNameLengths + numPIDs * pidSize)

        !sizeCoins = coinsPerUTxOWord * (uTxOEntrySizeWithoutVal + sizeWords + hash)
        -- sizeCoins =  coinsPerUTxOByte * (160 + (sizeWords + hash )* 8 )

        !minADA = max minUTxOValue sizeCoins

    in  TxRatio.truncate (TxRatio.unsafeRatio (130 * minADA) 100) -- TODO: 130% of the minimum UTxO value

{-# INLINEABLE calculateNumAssetsAndPIDS #-}
calculateNumAssetsAndPIDS :: LedgerApiV2.Value -> (Integer, Integer, Integer)
calculateNumAssetsAndPIDS !value =
    let

        -- !valueWithOutAda = value <> negate ( LedgerValue.adaOnlyValue value)
        !valueWithAda = value <> LedgerAda.lovelaceValueOf 0
        !flattenValue' = flattenValue valueWithAda

        sumarPId :: LedgerValue.Value-> Integer
        sumarPId (LedgerValue.Value mp) = length (TxAssocMap.toList mp) - 1
        -- necesito restar uno por que en funcionamiento la entrada de ada siempre esta
        -- de hecho me aseguro de que este sumando cero antes
        -- es que si no, a veces viene o a veces no
        -- incluso aunque sea un value sin ada, onchain a veces lo trae
        -- ademas no lo pude eliminar ni con la resta anterior (valueWithOutAda)

        !numPIDs = sumarPId valueWithAda
        -- !numAssets = length [tn | (_, tn, amt) <- flattenValue', amt > 0]
        !numAssets = length [tn | (_, tn, _) <- flattenValue']
        -- !sumAssetNameLengths = sum [lengthOfByteString $ LedgerApiV2.unTokenName tn | (_, tn, amt) <- flattenValue', amt > 0]
        !sumAssetNameLengths = sum [lengthOfByteString $ LedgerApiV2.unTokenName tn | (_, tn, _) <- flattenValue']
    in  (numAssets, sumAssetNameLengths, numPIDs)

{-# INLINEABLE calculateMinADAOfValue #-}
calculateMinADAOfValue :: LedgerApiV2.Value -> Bool -> Integer
calculateMinADAOfValue !value !isHash =
    let !(numAssets, sumAssetNameLengths, numPIDs) = calculateNumAssetsAndPIDS value
        !minADA = calculateMinADA numAssets sumAssetNameLengths numPIDs isHash
    in  minADA

--------------------------------------------------------------------------------2

{-# INLINEABLE getTxOut_In_TxOut_And_Datum #-}
getTxOut_In_TxOut_And_Datum :: (LedgerApiV2.TxOut, d) -> LedgerApiV2.TxOut
getTxOut_In_TxOut_And_Datum = fst

{-# INLINEABLE getValue_In_TxOut_And_Datum #-}
getValue_In_TxOut_And_Datum :: (LedgerApiV2.TxOut, d) -> LedgerApiV2.Value
getValue_In_TxOut_And_Datum = LedgerApiV2.txOutValue . fst

{-# INLINEABLE getAddress_In_TxOut_And_Datum #-}
getAddress_In_TxOut_And_Datum :: (LedgerApiV2.TxOut, d) -> LedgerApiV2.Address
getAddress_In_TxOut_And_Datum = LedgerApiV2.txOutAddress . fst

{-# INLINEABLE getDatum_In_TxOut_And_Datum #-}
getDatum_In_TxOut_And_Datum :: (LedgerApiV2.TxOut, d) -> d
getDatum_In_TxOut_And_Datum = snd

--------------------------------------------------------------------------------2

{-# INLINEABLE getUnsafeScriptHash_In_Address #-}
getUnsafeScriptHash_In_Address :: LedgerApiV2.Address -> LedgerApiV2.ValidatorHash
getUnsafeScriptHash_In_Address  (LedgerApiV2.Address (LedgerApiV2.ScriptCredential script_Hash) _) = script_Hash
getUnsafeScriptHash_In_Address  _                                                                  = traceError "getScriptHash_In_Address"

--------------------------------------------------------------------------------2

{-# INLINEABLE isScriptAddress #-}
isScriptAddress :: LedgerApiV2.Address -> Bool
isScriptAddress (LedgerApiV2.Address (LedgerApiV2.ScriptCredential _) _) = True
isScriptAddress _                                                        = False

--------------------------------------------------------------------------------2

getRedeemerForConsumeInput ::  LedgerApiV2.TxOutRef -> LedgerApiV2.TxInfo -> Maybe LedgerApiV2.Redeemer
getRedeemerForConsumeInput txOutRef info =
    let
        !txInfoRedeemers = LedgerApiV2.txInfoRedeemers info
        !spendRedeemer' = TxAssocMap.lookup (LedgerApiV2.Spending txOutRef) txInfoRedeemers
    in
        spendRedeemer'

--------------------------------------------------------------------------------2

{-# INLINEABLE isBurningAllTokenOwnCSAnyAmount #-}
isBurningAllTokenOwnCSAnyAmount :: LedgerContextsV2.ScriptContext -> Bool
isBurningAllTokenOwnCSAnyAmount ctx = case getUnsafeOwnMintingTokenNameAndAmt ctx of
    [] -> False
    x  -> all (\(_, amt) -> amt < 0) x

--------------------------------------------------------------------------------2

{-# INLINEABLE isMintingNFTOwnCSAnyTN #-}
isMintingNFTOwnCSAnyTN :: LedgerContextsV2.ScriptContext -> Bool
isMintingNFTOwnCSAnyTN ctx = case getUnsafeOwnMintingTokenNameAndAmt ctx of
    [] -> False
    x  -> all (\(_, amt) -> amt == 1) x
--------------------------------------------------------------------------------2

-- | Gets the Input TxInInfo corresponding to the TxOutRef. Its unsafe because it assumes that the TxOutRef is in the list
{-# INLINEABLE getUnsafe_TxInInfo_By_TxOutRef #-}
getUnsafe_TxInInfo_By_TxOutRef :: [LedgerApiV2.TxInInfo] -> LedgerApiV2.TxOutRef -> LedgerApiV2.TxInInfo
getUnsafe_TxInInfo_By_TxOutRef [] _ = traceError "getUnsafe_TxInInfo_By_TxOutRef"
getUnsafe_TxInInfo_By_TxOutRef ((LedgerApiV2.TxInInfo tref ot) : tl) o_ref
    | tref == o_ref = LedgerApiV2.TxInInfo tref ot
    | otherwise = getUnsafe_TxInInfo_By_TxOutRef tl o_ref

-- | Gets the Input TxOut currently being validated by the script. Its unsafe becasue is using getUnsafe_TxInInfo_By_TxOutRef
{-# INLINEABLE getUnsafe_Own_Input_TxOut #-}
getUnsafe_Own_Input_TxOut :: LedgerContextsV2.ScriptContext -> LedgerApiV2.TxOut
getUnsafe_Own_Input_TxOut (LedgerContextsV2.ScriptContext t_info (LedgerContextsV2.Spending o_ref)) = LedgerApiV2.txInInfoResolved (getUnsafe_TxInInfo_By_TxOutRef (LedgerApiV2.txInfoInputs t_info) o_ref)
getUnsafe_Own_Input_TxOut _                                                                         = traceError "getUnsafe_Own_Input_TxOut"

-- --------------------------------------------------------------------------------2

-- | Gets the Datum attached to the TxOut. Its unsafe becasue is asuming that the txOut has a Datum and that the Datum is of type datum
{-# INLINEABLE getUnsafe_Datum_From_TxOut #-}
getUnsafe_Datum_From_TxOut :: forall datum. PlutusTx.UnsafeFromData datum => LedgerContextsV2.ScriptContext -> LedgerApiV2.TxOut -> datum
getUnsafe_Datum_From_TxOut !ctx !txOut =
    let findDatum :: LedgerTxV2.OutputDatum -> Maybe LedgerApiV2.Datum
        findDatum LedgerTxV2.NoOutputDatum               = Nothing
        findDatum (LedgerTxV2.OutputDatumHash datumHash) = LedgerContextsV2.findDatum datumHash (LedgerContextsV2.scriptContextTxInfo ctx)
        findDatum (LedgerTxV2.OutputDatum datum)         = Just datum
    in  case findDatum $ LedgerTxV2.txOutDatum txOut of
            Nothing -> traceError "getUnsafe_Datum_From_TxOut"
            Just x  -> LedgerApiV2.unsafeFromBuiltinData @datum $ LedgerApiV2.getDatum x

-- | Gets the Datum type attached to the TxOut and returns a tuple (txOut, datum type).
-- | Its unsafe becasue is asuming that the txOut has a Datum and that the Datum is of type datum
{-# INLINEABLE getTxOuts_And_DatumTypes_From_TxOuts_By_CS #-}
getTxOuts_And_DatumTypes_From_TxOuts_By_CS :: forall datum datumType. PlutusTx.UnsafeFromData datum => LedgerContextsV2.ScriptContext -> [LedgerApiV2.TxOut] -> LedgerApiV2.CurrencySymbol -> (datum -> datumType) -> [(LedgerApiV2.TxOut, datumType)]
getTxOuts_And_DatumTypes_From_TxOuts_By_CS !ctx !txOuts !cs !getDatumTypeFromDatum =
    [(txOut, getDatumTypeFromDatum $ getUnsafe_Datum_From_TxOut @datum ctx txOut) | txOut <- txOuts, 
        isScriptAddress (LedgerApiV2.txOutAddress txOut) && isToken_With_CS_InValue (LedgerApiV2.txOutValue txOut) cs]

-- | Gets the Datum type attached to the TxOut and returns a tuple (txOut, datum type).
-- | Its unsafe becasue is asuming that the txOut has a Datum and that the Datum is of type datum
{-# INLINEABLE getTxOuts_And_DatumTypes_From_TxOuts_By_AC #-}
getTxOuts_And_DatumTypes_From_TxOuts_By_AC :: forall datum datumType. PlutusTx.UnsafeFromData datum => LedgerContextsV2.ScriptContext -> [LedgerApiV2.TxOut] -> LedgerValue.AssetClass -> (datum -> datumType) -> [(LedgerApiV2.TxOut, datumType)]
getTxOuts_And_DatumTypes_From_TxOuts_By_AC !ctx !txOuts !ac !getDatumTypeFromDatum =
    [(txOut, getDatumTypeFromDatum $ getUnsafe_Datum_From_TxOut @datum ctx txOut) | txOut <- txOuts, 
        isScriptAddress (LedgerApiV2.txOutAddress txOut) && isToken_With_AC_InValue (LedgerApiV2.txOutValue txOut) ac]

{-# INLINEABLE getTxOutRefs_TxOuts_And_DatumTypes_From_TxOutRefs_TxOuts_By_CS #-}
getTxOutRefs_TxOuts_And_DatumTypes_From_TxOutRefs_TxOuts_By_CS :: forall datum datumType. PlutusTx.UnsafeFromData datum => LedgerContextsV2.ScriptContext -> [(LedgerApiV2.TxOutRef, LedgerApiV2.TxOut)] -> LedgerApiV2.CurrencySymbol -> (datum -> datumType) -> [(LedgerApiV2.TxOutRef, LedgerApiV2.TxOut, datumType)]
getTxOutRefs_TxOuts_And_DatumTypes_From_TxOutRefs_TxOuts_By_CS !ctx !txOutRef_And_TxOuts !cs !getDatumTypeFromDatum =
    [(txOutRef, txOut, getDatumTypeFromDatum $ getUnsafe_Datum_From_TxOut @datum ctx txOut) | (txOutRef, txOut) <- txOutRef_And_TxOuts, 
        isScriptAddress (LedgerApiV2.txOutAddress txOut) && isToken_With_CS_InValue (LedgerApiV2.txOutValue txOut) cs]

{-# INLINEABLE getTxOutRefs_TxOuts_And_DatumTypes_From_TxOutRefs_TxOuts_By_AC #-}
getTxOutRefs_TxOuts_And_DatumTypes_From_TxOutRefs_TxOuts_By_AC :: forall datum datumType. PlutusTx.UnsafeFromData datum => LedgerContextsV2.ScriptContext -> [(LedgerApiV2.TxOutRef, LedgerApiV2.TxOut)] -> LedgerValue.AssetClass -> (datum -> datumType) -> [(LedgerApiV2.TxOutRef, LedgerApiV2.TxOut, datumType)]
getTxOutRefs_TxOuts_And_DatumTypes_From_TxOutRefs_TxOuts_By_AC !ctx !txOutRef_And_TxOuts  !ac !getDatumTypeFromDatum =
    [(txOutRef, txOut, getDatumTypeFromDatum $ getUnsafe_Datum_From_TxOut @datum ctx txOut) | (txOutRef, txOut) <- txOutRef_And_TxOuts, isScriptAddress (LedgerApiV2.txOutAddress txOut) && isToken_With_AC_InValue (LedgerApiV2.txOutValue txOut) ac]

{-# INLINEABLE getTxOut_And_DatumType_From_TxOut_And_CS_And_Address #-}
getTxOut_And_DatumType_From_TxOut_And_CS_And_Address :: forall datum datumType. PlutusTx.UnsafeFromData datum => LedgerContextsV2.ScriptContext -> LedgerApiV2.TxOut -> LedgerApiV2.CurrencySymbol -> Maybe Ledger.Address -> (datum -> datumType) -> Maybe (LedgerApiV2.TxOut, datumType)
getTxOut_And_DatumType_From_TxOut_And_CS_And_Address !ctx !txOut !cs !add' !getDatumTypeFromDatum =
    if (case  add' of
            Just add -> LedgerApiV2.txOutAddress txOut == add
            _        -> True)
        && isToken_With_CS_InValue (LedgerApiV2.txOutValue txOut) cs
       then Just (txOut, getDatumTypeFromDatum $ getUnsafe_Datum_From_TxOut @datum ctx txOut)
       else Nothing

{-# INLINEABLE getTxOut_And_DatumType_From_TxOut_And_AC_And_Address #-}
getTxOut_And_DatumType_From_TxOut_And_AC_And_Address :: forall datum datumType. PlutusTx.UnsafeFromData datum => LedgerContextsV2.ScriptContext -> LedgerApiV2.TxOut -> LedgerValue.AssetClass -> Maybe Ledger.Address -> (datum -> datumType) -> Maybe (LedgerApiV2.TxOut, datumType)
getTxOut_And_DatumType_From_TxOut_And_AC_And_Address !ctx !txOut !ac !add' !getDatumTypeFromDatum =
    if (case  add' of
            Just add -> LedgerApiV2.txOutAddress txOut == add
            _        -> True)
        && isToken_With_AC_InValue (LedgerApiV2.txOutValue txOut) ac
       then Just (txOut, getDatumTypeFromDatum $ getUnsafe_Datum_From_TxOut @datum ctx txOut)
       else Nothing

--------------------------------------------------------------------------------2
