{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Main where

import System.Process
import Sources.Pnpm ( parsePnpmAudit )
import Data.Dependencies ( Package )
import Lib.Report ( createReports, printReports )
import Control.Monad.Except
import Control.Exception
import Options.Applicative

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC

import System.Exit (ExitCode(ExitSuccess))

type Task = ExceptT String IO

data InputSource
    = FileInput FilePath
    | StdInput
    | InputNone

data AuditInstalledOptions = AuditInstalledOptions
    { auditInstalledInput :: InputSource
    }

data Command
    = AuditInstalled AuditInstalledOptions

data MonoTools = MonoTools
    { optCommand :: Command
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

inputSource :: Parser InputSource
inputSource =
    fileInput <|> stdInput <|> pure InputNone

auditInstalledCommand :: Parser Command
auditInstalledCommand =
    AuditInstalled . AuditInstalledOptions <$> inputSource

monoTools :: Parser MonoTools
monoTools = MonoTools <$>
    hsubparser
        ( command "audit-installed" (info auditInstalledCommand ( progDesc "String")) )

main :: IO ()
main =
    runMonoTools =<< execParser opts
    where
        opts = info (monoTools <**> helper)
            (  fullDesc
            <> progDesc "Tooling for managing dependencies in a PNPM / Turbo monorepo"
            )

runMonoTools :: MonoTools -> IO ()
runMonoTools monoToolsOptions =
    case optCommand monoToolsOptions of
        AuditInstalled auditInstalledOptions ->
            runAuditInstalledCommand auditInstalledOptions

runAuditInstalledCommand :: AuditInstalledOptions -> IO ()
runAuditInstalledCommand opts = do
    result <- runExceptT runAuditInstalledCommand'
    case result of
        Left err -> putStrLn $ "Err: " ++ err
        Right _  -> pure ()
    where
        runAuditInstalledCommand' = do
            input <- case auditInstalledInput opts of
                FileInput file ->
                    getInputFromFile file
                _ ->
                    getInputFromPnpmList
            audit <- either throwError return (parsePnpmAudit input)
            let reports = createReports audit
            liftIO $ printReports reports

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
    fileOrErr <- liftIO (try $ BS.readFile inputFile :: IO (Either IOException BS.ByteString))
    either (\_ -> throwError ("Unable to open file - does " ++ inputFile ++ " exist?")) return fileOrErr
