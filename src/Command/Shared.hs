module Command.Shared ( packageFilterParser, failingOnly ) where

import Options.Applicative

packageFilterParser :: Parser (Maybe String)
packageFilterParser = optional $ strOption
    (  long "package-filter"
    <> short 'p'
    <> metavar "PACKAGE"
    <> help "Filter the report to only show a specific package"
    )

failingOnly :: Parser Bool
failingOnly =
    switch
        (  long "failing-only"
        <> short 'x'
        <> help "Only report packages with problems"
        )
