{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Command.AuditPackages ( auditPackagesCommand, runAuditPackagesCommand, AuditPackages ) where

import           Command.Shared       ( failingOnly, packageFilterParser )
import           Control.Monad        ( join )
import           Control.Monad.Except
import qualified Data.ByteString.Lazy as BS
import           Data.Maybe           ( fromMaybe, isJust )
import           Data.Task            ( Task )
import           Lib.Report           ( Report, createReports, printReports )
import           Lib.Util             ( filterAll )
import           Options.Applicative
import           Sources.PackageJson  ( parsePackageJson )
import           System.FilePath.Glob ( compile, globDir )

data AuditPackages = AuditPackages
    { _packageFilter :: Maybe String
    , _failingOnly   :: Bool
    }

auditPackagesParser :: Parser AuditPackages
auditPackagesParser =
    AuditPackages
        <$> packageFilterParser
        <*> failingOnly

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
runAuditPackagesCommand (AuditPackages pkgFilter failingOnlyFlag)= do
    paths <- liftIO getPackageJsonPaths
    files <- liftIO $ mapM BS.readFile paths
    audit <- liftEither $ mapM parsePackageJson files
    let filteredReports = filterAll filters $ createReports audit
    reports <- case filteredReports of
        [] -> throwError "No packages match requested filtering options"
        xs -> return xs
    liftIO $ printReports filteredReports
    where
        filters :: [(String, Report) -> Bool]
        filters =
            [ maybe (const True) (\p -> (== p) . fst) pkgFilter
            , if failingOnlyFlag then isJust . snd else const True
            ]
