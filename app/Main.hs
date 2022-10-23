module Main where

import Sources.Pnpm ( parsePnpmAudit )
import Lib.Report ( createReports, printReports )
import qualified Data.ByteString.Lazy as BS

main :: IO ()
main = do
    audit <- parsePnpmAudit <$> BS.readFile "./deps.json"
    let reports = createReports <$> audit
    case reports of
        Nothing -> print "Error parsing report"
        Just reports' -> printReports reports'
