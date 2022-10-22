module Lib.Report ( printReport, createReports ) where

import Lib.Util ( initMap )
import Data.Dependencies ( Package (..), Dependency (..) )
import Data.Maybe ( fromMaybe )
import Data.List ( sortOn, groupBy, nub )
import Control.Monad ( join )
import Control.Arrow ( (***) )

import qualified Data.ByteString.Lazy as BS

data VersionEntry = VersionEntry
    { version :: String
    , dependents :: [String]
    }
    deriving (Eq)

instance Show VersionEntry where
    show (VersionEntry version deps) =
        "@" ++ version ++ " " ++ show deps

data VersionGroup = VersionGroup
    { name :: String
    , versions :: [VersionEntry]
    }
    deriving (Eq)

instance Show VersionGroup where
    show (VersionGroup name versions) =
        name ++ " => " ++ show versions

type Report = Either VersionGroup String

unwrapTupleList :: [(a,b)] -> Maybe (b, [a])
unwrapTupleList [] =
    Nothing
unwrapTupleList xs =
    Just (snd . head $ xs, fst <$> xs)

extractDependenciesFromPackage :: Package -> [(String, Dependency)]
extractDependenciesFromPackage (Package name _ devDeps deps) =
    let
        addName dep = (name, dep)
        xs = fromMaybe [] deps
        ys = fromMaybe [] devDeps
    in
        addName <$> xs ++ ys

depMapToVersionEntry :: (Dependency, [String]) -> VersionEntry
depMapToVersionEntry (dep, deps) =
    VersionEntry (dependencyVersion dep) (nub deps)

depMapToVersionGroup :: [(Dependency, [String])] -> VersionGroup
depMapToVersionGroup deps =
    let
        name = dependencyName . fst . head $ deps
    in
        VersionGroup name (depMapToVersionEntry <$> deps)

createVersionGroups :: [(String, Dependency)] -> [VersionGroup]
createVersionGroups deps =
    let
        dependencyName' dep =
            (dependencyName . snd $ dep) <> (dependencyVersion . snd $ dep)
        dependencyNameEq (a,_) (b,_) =
            dependencyName a == dependencyName b
        groupedByDependencyName =
            groupBy (\a b -> snd a == snd b) (sortOn dependencyName' deps)
    in
        maybe [] (fmap depMapToVersionGroup . groupBy dependencyNameEq) (mapM unwrapTupleList groupedByDependencyName)

validateVersionGroup :: VersionGroup -> Report
validateVersionGroup group@(VersionGroup name versions)
    | length versions == 1 = Right name
    | otherwise =
        Left group

createReports :: [Package] -> [Report]
createReports pkgs =
    validateVersionGroup <$> (sortOn name . createVersionGroups $ extractDependenciesFromPackage =<< pkgs)

printReportErrDependent :: Bool -> Bool -> String -> IO ()
printReportErrDependent False False dependent =
    putStrLn $ "│  ├─ "++ dependent
printReportErrDependent False True dependent =
    putStrLn $ "│  └─ "++ dependent
printReportErrDependent True False dependent =
    putStrLn $ "   ├─ "++ dependent
printReportErrDependent True True dependent =
    putStrLn $ "   └─ "++ dependent

printReportErrVersion :: Bool -> VersionEntry -> IO ()
printReportErrVersion last (VersionEntry name dependents) = do
    putStrLn $ dirChar : "─ " ++ name
    sequence_ $ initMap (printReportErrDependent last False) (printReportErrDependent last True) dependents
    where
        dirChar = if last then '└' else '├'

printReportErr :: VersionGroup -> IO ()
printReportErr (VersionGroup name versions) = do
    putStrLn $ "❌ " ++ name
    sequence_ $ initMap (printReportErrVersion False) (printReportErrVersion True) versions

printReportOk :: String -> IO ()
printReportOk name =
    putStrLn $ "✅ " ++ name

printReport :: Report -> IO ()
printReport r =
    case r of
        Left err -> printReportErr err
        Right ok -> printReportOk ok
