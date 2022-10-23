module Data.Dependencies ( Dependency (..), Package (..) ) where

data Dependency = Dependency
    { dependencyName    :: String
    , dependencyVersion :: String
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
