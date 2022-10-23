module Lib.Report ( createReports, printReports ) where

import Lib.Util ( initMap )
import Data.Dependencies ( Package (..), Dependency (..) )
import Data.Maybe ( fromMaybe )

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M

type DependencyWithParent = (String, Dependency)
type VersionMap = M.Map String [String]
type DependencyMap = M.Map String VersionMap
type Report = Maybe VersionMap

extractDependenciesFromPackage :: Package -> [DependencyWithParent]
extractDependenciesFromPackage (Package name _ devDeps deps) =
    let
        addName dep = (name, dep)
        xs = fromMaybe [] deps
        ys = fromMaybe [] devDeps
    in
        addName <$> xs ++ ys

addVersionToDependencyMap :: String -> String -> VersionMap -> VersionMap
addVersionToDependencyMap version dependent versionGroup =
    if M.member version versionGroup then
        M.adjust (++ [dependent]) version versionGroup
    else
        M.union versionGroup $ M.fromList [(version, [dependent])]

foldDependencyWithParent :: DependencyWithParent -> DependencyMap -> DependencyMap
foldDependencyWithParent (dependent, Dependency name version) report =
    if M.member name report then
        M.adjust (addVersionToDependencyMap version dependent) name report
    else
        M.insert name empty report
    where
        empty = M.fromList [(version, [dependent])]

validateVersionMapHasOneVersion :: VersionMap -> Report
validateVersionMapHasOneVersion versionMap
    | M.size versionMap == 1 =
        Nothing
    | otherwise =
        Just versionMap

printReportErrDependent :: Bool -> Bool -> String -> IO ()
printReportErrDependent False False dependent =
    putStrLn $ "│  ├─ " ++ dependent
printReportErrDependent False True dependent =
    putStrLn $ "│  └─ " ++ dependent
printReportErrDependent True False dependent =
    putStrLn $ "   ├─ " ++ dependent
printReportErrDependent True True dependent =
    putStrLn $ "   └─ " ++ dependent

printReportErrVersion :: Bool -> (String, [String]) -> IO ()
printReportErrVersion last (version, dependents) = do
    putStrLn $ dirChar : "─ " ++ version
    sequence_ $ initMap (printReportErrDependent last False) (printReportErrDependent last True) dependents
    where
        dirChar = if last then '└' else '├'

printReportErr :: String -> VersionMap -> IO ()
printReportErr name versionMap = do
    putStrLn $ "❌ " ++ name
    sequence_ $ initMap (printReportErrVersion False) (printReportErrVersion True) (M.toList versionMap)

printReportOk :: String -> IO ()
printReportOk name =
    putStrLn $ "✅ " ++ name

printReport :: (String, Report) -> IO ()
printReport (pkg, report) =
    case report of
        Just versionMap -> printReportErr pkg versionMap
        Nothing -> printReportOk pkg

printReports :: [(String, Report)] -> IO ()
printReports =
    mapM_ printReport

createReports :: [Package] -> [(String, Report)]
createReports packages =
    M.toList . M.map validateVersionMapHasOneVersion . foldr foldDependencyWithParent M.empty $ extractDependenciesFromPackage =<< packages
