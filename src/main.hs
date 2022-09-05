import System.Environment

import DCommands
import DParser
import DConverter
import DScaler
import DRounder

main = do
    args <- getArgs
    if elem "-h" args || elem "-H" args
        then do
            putStrLn usage
        else do
            dPathStr <- getContents
            putStrLn$show (process args (readD dPathStr))

process :: [String] -> PathD -> PathD
process [] pathD = pathD
process ("--abs":largs) pathD = process largs (toAbs pathD)
process ("--rel":largs) pathD = process largs (toRel pathD)
process ("--scale":n:largs) pathD = process largs (dScale (read n) pathD)
process ("--round":largs) pathD = process largs (roundD pathD)

usage :: String
usage = "test"
