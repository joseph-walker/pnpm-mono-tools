{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Command.AuditInstalled
    ( AuditInstalled
    , auditInstalledCommand
    , runAuditInstalledCommand
    ) where

import Control.Exception ( IOException, try )
import Control.Monad.Except
import Data.Task ( Task )
import Lib.Report ( createReports, printReports )
import Options.Applicative
import Sources.Pnpm ( parsePnpmAudit )
import System.Exit
import System.Process ( readProcessWithExitCode )

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC

data InputSource
    = FileInput FilePath
    | StdInput
    | InputNone

data AuditInstalled = AuditInstalled
    { auditInstalledInput :: InputSource
    }

fileInput :: Parser InputSource
fileInput = FileInput <$> strOption
    (  long "file"
    <> short 'f'
    <> metavar "TARGET"
    <> help "Path to dependencies.json file from pnpm list"
    )

stdInput :: Parser InputSource
stdInput = flag' StdInput
  (  long "stdin"
  <> help "Read pnpm list from stdin"
  )

getInputFromPnpmList :: Task BS.ByteString
getInputFromPnpmList = do
    (code, stdOut, stdErr) <- liftIO $ readProcessWithExitCode "pnpm" ["list", "-r", "--json"] ""
    case code of
        ExitSuccess ->
            -- For some reason pnpm list doesn't exit with a bad status code if the directory has no package.jsons
            if stdOut == "" then
                throwError genericError
            else
                return $ BSC.pack stdOut
        _ ->
            throwError stdErr
    where
        genericError = "Error executing pnpm command - does this directory contain a package.json?"

getInputFromFile :: FilePath -> Task BS.ByteString
getInputFromFile inputFile = do
    fileOrErr <- liftIO tryReadFile
    either handleError return fileOrErr
    where
        tryReadFile :: IO (Either IOException BS.ByteString)
        tryReadFile =
            try $ BS.readFile inputFile
        handleError :: IOException -> Task a
        handleError _ =
            throwError ("Unable to open file - does " ++ inputFile ++ " exist?")

auditInstalledParser :: Parser AuditInstalled
auditInstalledParser =
    AuditInstalled <$> (fileInput <|> stdInput <|> pure InputNone)

auditInstalledCommand :: ParserInfo AuditInstalled
auditInstalledCommand =
    info auditInstalledParser ( progDesc "Run a dependency audit of the currently resolved package versions")

runAuditInstalledCommand :: AuditInstalled -> Task ()
runAuditInstalledCommand opts = do
    input <- case auditInstalledInput opts of
        FileInput file -> getInputFromFile file
        _              -> getInputFromPnpmList -- TODO: Handle stdIn input type
    audit <- either throwError return (parsePnpmAudit input)
    liftIO $ printReports (createReports audit)
