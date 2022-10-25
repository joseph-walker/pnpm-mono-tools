{-# LANGUAGE InstanceSigs #-}

module Sources.Pnpm ( parsePnpmAudit ) where

import           Data.Aeson
import           Data.Aeson.Types     ( Key, Parser )
import qualified Data.ByteString.Lazy as BS
import           Data.Dependencies    ( Dependency (..), Package (..) )
import           Data.Map             ( Map, elems )
import           Data.Maybe           ( fromMaybe )

data PnpmDependency = PnpmDependency
    { from    :: String
    , version :: String
    }
    deriving Show

instance FromJSON PnpmDependency where
    parseJSON :: Value -> Parser PnpmDependency
    parseJSON = withObject "Dependency" $ \v -> do
        from    <- v .: "from"
        version <- v .: "version"
        return $ PnpmDependency from version

data PnpmPackage = PnpmPackage
    { name            :: String
    , packageVersion  :: Maybe String
    , dependencies    :: Maybe (Map String PnpmDependency)
    , devDependencies :: Maybe (Map String PnpmDependency)
    }
    deriving Show

instance FromJSON PnpmPackage where
    parseJSON :: Value -> Parser PnpmPackage
    parseJSON = withObject "Package" $ \v -> do
        name            <- v .: "name"
        packageVersion  <- v .:? "version"
        dependencies    <- v .:? "dependencies"
        devDependencies <- v .:? "devDependencies"
        return $ PnpmPackage name packageVersion dependencies devDependencies

pnpmDependencyToDependency :: PnpmDependency -> Dependency
pnpmDependencyToDependency (PnpmDependency from version) =
    Dependency from version

pnpmAuditToPackageList :: PnpmPackage -> Package
pnpmAuditToPackageList (PnpmPackage name version dependencies devDependencies) =
    let
        extractDependencies = fmap pnpmDependencyToDependency . elems
    in
        Package
            name
            (fromMaybe "0.0.0" version)
            (extractDependencies <$> dependencies)
            (extractDependencies <$> devDependencies)

parsePnpmAudit :: BS.ByteString -> Either String [Package]
parsePnpmAudit src =
    fmap pnpmAuditToPackageList <$> eitherDecode src
