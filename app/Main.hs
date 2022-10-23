module Main where

import System.Process
import System.Exit
import System.IO
import Sources.Pnpm ( parsePnpmAudit )
import Data.Dependencies ( Package )
import Lib.Report ( createReports, printReports )
import Control.Monad.Except
import Control.Exception
import qualified Data.ByteString.Lazy as BS

type Task = ExceptT String IO

runPnpmList :: Task BS.ByteString
runPnpmList = do
    (_, stdOut, stdErr, process) <- liftIO $ createProcess (proc "pnpm" ["list", "-r", "--json"])
    case BS.hGetContents <$> stdOut of
        Just h ->
            liftIO h
        Nothing ->
            throwError "Error executing pnpm command"

runPnpmReadAuditFile :: Task BS.ByteString
runPnpmReadAuditFile = do
    fileOrErr <- liftIO (try $ BS.readFile "./deps.json" :: IO (Either IOException BS.ByteString))
    either (\_ -> throwError "Error reading file") return fileOrErr

createReport :: ExceptT String IO [Package]
createReport = do
    pnpmListOutput <- runPnpmReadAuditFile
    either throwError return (parsePnpmAudit pnpmListOutput)

main :: IO ()
main = do
    audit <- runExceptT createReport
    let reports = createReports <$> audit
    case reports of
        Left err -> print err
        Right reports' -> printReports reports'
