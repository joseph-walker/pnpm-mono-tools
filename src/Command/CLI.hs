{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Command.CLI ( monoTools ) where

import Command.AuditInstalled
import Control.Monad.Except
import Lib.Report ( createReports, printReports )
import Options.Applicative
import Sources.Pnpm ( parsePnpmAudit )
import Command.AuditPackages ( auditPackagesCommand, runAuditPackagesCommand )

data Command
    = CmdAuditInstalled AuditInstalled
    | CmdAuditPackage ()

data MonoTools = MonoTools
    { optCommand :: Command
    }

monoToolsParser :: Parser MonoTools
monoToolsParser = MonoTools <$> hsubparser
    (  command "audit-installed" (CmdAuditInstalled <$> auditInstalledCommand)
    <> command "audit-packages" (CmdAuditPackage <$> auditPackagesCommand)
    )

monoTools :: IO ()
monoTools =
    runMonoTools =<< execParser opts
    where
        opts = info (monoToolsParser <**> helper)
            (  fullDesc
            <> progDesc "Tooling for managing dependencies in a PNPM / Turbo monorepo"
            )

runMonoTools :: MonoTools -> IO ()
runMonoTools monoToolsOptions = do
    result <- runExceptT commandResult
    case result of
        Left err -> putStrLn $ "Err: " ++ err
        Right _  -> pure ()
    where
        commandResult = case optCommand monoToolsOptions of
            CmdAuditInstalled opts ->
                runAuditInstalledCommand opts
            CmdAuditPackage _ ->
                runAuditPackagesCommand
