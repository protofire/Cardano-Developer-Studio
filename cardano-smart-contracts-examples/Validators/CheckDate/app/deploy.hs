{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module Main where

--------------------------------------------------------------------------------2
-- External Imports
--------------------------------------------------------------------------------2
import qualified Control.Monad.IO.Class        as MonadIOClass (MonadIO (..))
import qualified Data.Time                     as DataTime (defaultTimeLocale, formatTime, getCurrentTime)
import qualified Data.List as DataList
import qualified Control.Exception as ControlException (throwIO)
import qualified Ledger
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api          as LedgerApiV2
import qualified PlutusTx
import           PlutusTx.Prelude              hiding (unless)
import qualified Prelude                       as P
import qualified System.Directory              as SystemDirectory
import qualified System.FilePath               as SystemFilePath
import qualified System.FilePath.Posix         as SystemFilePathPosix
import qualified Data.String                   as DataString
import qualified Ledger.Value                  as LedgerValue
import qualified PlutusTx.Builtins.Class       as TxBuiltinsClass
import qualified Data.Aeson                  as DataAeson (FromJSON, ToJSON)
import qualified Data.OpenApi.Schema         as DataOpenApiSchema (ToSchema)
import qualified GHC.Generics                as GHCGenerics (Generic)
import qualified Schema
import qualified System.Environment  as SystemEnvironment (getArgs, getProgName)
import qualified System.Process  as SystemProcess

--------------------------------------------------------------------------------2
-- Internal Imports
--------------------------------------------------------------------------------2

import qualified Helpers.CLI            as CLIHelpers
import qualified Helpers.Deploy         as DeployHelpers
import qualified Helpers.OffChain       as OffChainHelpers

import           DatumCheckAfterDeadlineValidator  as OnChain (datumCheckAfterDeadlineValidator)
import           DatumCheckBeforeDeadlineValidator as OnChain (datumCheckBeforeDeadlineValidator)
import           ParamCheckAfterDeadlineValidator  as OnChain (paramCheckAfterDeadlineValidator)
import           ParamCheckBeforeDeadlineValidator as OnChain (paramCheckBeforeDeadlineValidator)

--------------------------------------------------------------------------------2

main :: P.IO ()
main = deploy

deploy :: P.IO ()
deploy = do
    ------------------------------
    args <- SystemEnvironment.getArgs
    ------------------------------
    case args of
        [baseFolder] -> runDeploy baseFolder
        [] -> runDeploy ""
        _ -> P.putStrLn "Error: Expected 1 argument: baseFolder"
    ------------------------------

runDeploy :: P.String -> P.IO ()
runDeploy baseFolder = do
    ------------------------------
    progName <- SystemEnvironment.getProgName
    ------------------------------
    let 
        stripSuffix suffix str = 
            if suffix `DataList.isSuffixOf` str
            then P.take (P.length str P.- P.length suffix) str
            else str
        projectName = stripSuffix "Deploy" progName
    ------------------------------
    let path = baseFolder SystemFilePathPosix.</> "export" SystemFilePathPosix.</> projectName
    ------------------------------
    currentTime <- DataTime.getCurrentTime
    let defaultName = DataTime.formatTime DataTime.defaultTimeLocale "%Y-%m-%d-%H-%M" currentTime
    ------------------------------
    P.putStrLn $ "Folder Name (default=" ++ defaultName ++ "):"
    folderName <- CLIHelpers.getStrWithDefault defaultName
    ------------------------------
    SystemDirectory.removePathForcibly (path SystemFilePathPosix.</> folderName)
    SystemDirectory.createDirectoryIfMissing True (path SystemFilePathPosix.</> folderName)
    ------------------------------
    P.putStrLn "Generating Files..."
    ------------------------------
    P.putStrLn $ "Path: " ++ path SystemFilePathPosix.</> folderName
    ------------------------------
    do 
        P.putStrLn "Generating 'datumCheckAfterDeadlineValidator' Script..."
        let validator = OnChain.datumCheckAfterDeadlineValidator
            validator_Hash = OffChainHelpers.hashValidator validator
            validator_Address = OffChainHelpers.addressValidator validator_Hash
        _ <- DeployHelpers.deployValidator (path SystemFilePathPosix.</> folderName) "datumCheckAfterDeadlineValidator" validator
        _ <- DeployHelpers.deployValidatorHash (path SystemFilePathPosix.</> folderName) "datumCheckAfterDeadlineValidator" validator_Hash
        DeployHelpers.deployValidatorAddress (path SystemFilePathPosix.</> folderName) "datumCheckAfterDeadlineValidator" validator_Address
    ------------------------------
    do 
        P.putStrLn "Generating 'datumCheckBeforeDeadlineValidator' Script..."
        let validator = OnChain.datumCheckBeforeDeadlineValidator
            validator_Hash = OffChainHelpers.hashValidator validator
            validator_Address = OffChainHelpers.addressValidator validator_Hash
        _ <- DeployHelpers.deployValidator (path SystemFilePathPosix.</> folderName) "datumCheckBeforeDeadlineValidator" validator
        _ <- DeployHelpers.deployValidatorHash (path SystemFilePathPosix.</> folderName) "datumCheckBeforeDeadlineValidator" validator_Hash
        DeployHelpers.deployValidatorAddress (path SystemFilePathPosix.</> folderName) "datumCheckBeforeDeadlineValidator" validator_Address
    ------------------------------
    do
        P.putStrLn "Generating 'paramCheckAfterDeadlineValidator' Script..."
        ------------------------------
        let currentTimeLedger = CLIHelpers.uTCTimeToLedgerPosixTime currentTime
            currentTimePlus15Minuts = currentTimeLedger P.+ (15 P.* 60 P.* 1000)
        deadline <- CLIHelpers.getTime "Deadline POSIXTime" currentTimePlus15Minuts currentTimeLedger
        ------------------------------
        let validator = OnChain.paramCheckAfterDeadlineValidator deadline
            validator_Hash = OffChainHelpers.hashValidator validator 
            validator_Address = OffChainHelpers.addressValidator validator_Hash
        _ <- DeployHelpers.deployValidator (path SystemFilePathPosix.</> folderName) "paramCheckAfterDeadlineValidator" validator
        _ <- DeployHelpers.deployValidatorHash (path SystemFilePathPosix.</> folderName) "paramCheckAfterDeadlineValidator" validator_Hash
        DeployHelpers.deployValidatorAddress (path SystemFilePathPosix.</> folderName) "paramCheckAfterDeadlineValidator" validator_Address
    ------------------------------
    do
        P.putStrLn "Generating 'paramCheckBeforeDeadlineValidator' Script..."
        ------------------------------
        let currentTimeLedger = CLIHelpers.uTCTimeToLedgerPosixTime currentTime
            currentTimePlus15Minuts = currentTimeLedger P.+ (15 P.* 60 P.* 1000)
        deadline <- CLIHelpers.getTime "Deadline POSIXTime " currentTimePlus15Minuts currentTimeLedger
        ------------------------------
        let validator = OnChain.paramCheckBeforeDeadlineValidator deadline
            validator_Hash = OffChainHelpers.hashValidator validator
            validator_Address = OffChainHelpers.addressValidator validator_Hash
        _ <- DeployHelpers.deployValidator (path SystemFilePathPosix.</> folderName) "paramCheckBeforeDeadlineValidator" validator
        _ <- DeployHelpers.deployValidatorHash (path SystemFilePathPosix.</> folderName) "paramCheckBeforeDeadlineValidator" validator_Hash
        DeployHelpers.deployValidatorAddress (path SystemFilePathPosix.</> folderName) "paramCheckBeforeDeadlineValidator" validator_Address
    ------------------------------
    return ()
