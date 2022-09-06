import System.Environment

import DCommands
import DParser
import DConverter
import DScaler
import DRounder
import DSkeleton

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
process ("--scale":n:largs) pathD = let rate = read n
    in process largs (dScale rate rate pathD)
process ("--scale-xy":nx:ny:largs) pathD = process largs (dScale rateX rateY pathD)
    where
        rateX = read nx
        rateY = read ny
process ("--round":largs) pathD = process largs (roundD pathD)
process ("--skeleton":largs) pathD = process largs (dSkeleton pathD)

usage :: String
usage = "test"
