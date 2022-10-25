module Command.AuditPackages ( auditPackagesCommand, runAuditPackagesCommand ) where

import Control.Monad ( join )
import Control.Monad.Except
import Data.Task ( Task )
import Lib.Report ( createReports, printReports )
import Options.Applicative
import Sources.PackageJson ( parsePackageJson )
import System.FilePath.Glob ( compile, globDir )

import qualified Data.ByteString.Lazy as BS

getPackageJsonPaths :: IO [FilePath]
getPackageJsonPaths =
    join <$> globDir
        [ compile "packages/*/package.json"
        , compile "apps/*/package.json"
        , compile "package.json"
        ] "/Users/josephwalker/Workspace/rent"

auditPackagesCommand :: ParserInfo ()
auditPackagesCommand =
    info (pure ()) ( progDesc "Run a dependency audit of the versions requested by individual pacakge.json files in the repo" )

runAuditPackagesCommand :: Task ()
runAuditPackagesCommand = do
    paths <- liftIO getPackageJsonPaths
    files <- liftIO $ mapM BS.readFile paths
    audit <- liftEither $ mapM parsePackageJson files
    liftIO $ printReports (createReports audit)
