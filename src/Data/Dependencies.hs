-- | Standardized format for interacting with dependencies
module Data.Dependencies ( Dependency (..), Package (..) ) where

-- | A package description with a version, e.g. axios@1.0.1
data Dependency = Dependency
    { dependencyName    :: String -- ^ Name
    , dependencyVersion :: String -- ^ Version - format is unrestricted, i.e. not necessarily SemVer
    }
    deriving Eq

instance Show Dependency where
    show (Dependency name version) =
        name ++ "@" ++ version

data Package = Package
    { packageName     :: String
    , packageVersion  :: String
    , dependencies    :: Maybe [Dependency]
    , devDependencies :: Maybe [Dependency]
    }
    deriving Eq

instance Show Package where
    show (Package name version deps devDeps) =
        name
        ++ "@"
        ++ version
        ++ "\n - deps => "
        ++ maybe "[]" show deps
        ++ "\n - devs => "
        ++ maybe "[]" show devDeps
        ++ "\n"
