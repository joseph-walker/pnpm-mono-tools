module Data.Dependencies ( Dependency (..), Package (..) ) where

data Dependency = Dependency
    { dependencyName :: String
    , dependencyVersion :: String
    }
    deriving (Eq)

instance Show Dependency where
    show (Dependency name version) =
        name ++ "@" ++ version

data Package = Package
    { packageName :: String
    , packageVersion :: String
    , dependencies :: Maybe [Dependency]
    , devDependencies :: Maybe [Dependency]
    }
    deriving (Eq)
