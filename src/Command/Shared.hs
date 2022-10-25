module Command.Shared ( packageFilterParser ) where

import Options.Applicative

packageFilterParser :: Parser (Maybe String)
packageFilterParser = optional $ strOption
    (  long "package-filter"
    <> short 'p'
    <> metavar "PACKAGE"
    <> help "Filter the report to only show a specific package"
    )
