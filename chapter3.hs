import System.IO

import Ray
import V3

color :: Ray -> Color3
color r = (conv $ 1 - t)*(color3 1 1 1) + (conv t)*(color3 0.5 0.7 1)
    where
        unitDir = mkUnit $ slope r
        t :: Double
        t = 0.5*(y unitDir + 1)

main :: IO ()
main = undefined
