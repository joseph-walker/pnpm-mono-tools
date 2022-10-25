{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Command.AuditPackages ( auditPackagesCommand, runAuditPackagesCommand, AuditPackages ) where

import           Command.Shared       ( packageFilterParser )
import           Control.Monad        ( join )
import           Control.Monad.Except
import qualified Data.ByteString.Lazy as BS
import           Data.Maybe           ( fromMaybe )
import           Data.Task            ( Task )
import           Lib.Report           ( createReports, printReports )
import           Lib.Util             ( maybeFilter )
import           Options.Applicative
import           Sources.PackageJson  ( parsePackageJson )
import           System.FilePath.Glob ( compile, globDir )

data AuditPackages = AuditPackages
    { packageFilter :: Maybe String
    }

auditPackagesParser :: Parser AuditPackages
auditPackagesParser =
    AuditPackages <$> packageFilterParser

getPackageJsonPaths :: IO [FilePath]
getPackageJsonPaths =
    join <$> globDir
        [ compile "packages/*/package.json"
        , compile "apps/*/package.json"
        , compile "package.json"
        ] "/Users/josephwalker/Workspace/rent"

auditPackagesCommand :: ParserInfo AuditPackages
auditPackagesCommand =
    info auditPackagesParser $ progDesc "Run a dependency audit of the versions requested by individual pacakge.json files in the repo"

runAuditPackagesCommand :: AuditPackages -> Task ()
runAuditPackagesCommand (AuditPackages pkgFilter)= do
    paths <- liftIO getPackageJsonPaths
    files <- liftIO $ mapM BS.readFile paths
    audit <- liftEither $ mapM parsePackageJson files
    let filteredReports = maybeFilter pkgFilter $ createReports audit
    reports <- case filteredReports of
        [] -> throwError $ "No packages match requested pattern \"" ++ fromMaybe "" pkgFilter ++ "\""
        xs -> return xs
    liftIO $ printReports filteredReports
