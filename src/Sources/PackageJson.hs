{-# LANGUAGE InstanceSigs #-}

module Sources.PackageJson ( parsePackageJson ) where

import Data.Aeson
import Data.Aeson.Types ( Key, Parser )
import Data.Dependencies ( Package (..), Dependency (..) )
import Data.Map ( Map, elems, toList )

import qualified Data.ByteString.Lazy as BS

data PackageJson = PackageJson
    { name            :: String
    , dependencies    :: Maybe (Map String String)
    , devDependencies :: Maybe (Map String String)
    }
    deriving ( Eq, Show )

instance FromJSON PackageJson where
    parseJSON :: Value -> Parser PackageJson
    parseJSON = withObject "PackageJson" $ \v -> do
        name            <- v .:  "name"
        dependencies    <- v .:? "dependencies"
        devDependencies <- v .:? "devDependencies"
        return $ PackageJson name dependencies devDependencies

packageJsonToStandardPackage :: PackageJson -> Package
packageJsonToStandardPackage (PackageJson name deps devDeps) =
    Package name "" (parseDeps deps) (parseDeps devDeps)
    where
        parseDeps = fmap (fmap (uncurry Dependency) . toList)

parsePackageJson :: BS.ByteString -> Either String Package
parsePackageJson src =
    packageJsonToStandardPackage <$> eitherDecode src
