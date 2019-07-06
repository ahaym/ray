import Control.Monad
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
main = withFile "chapter3.ppm" WriteMode $ \h -> do
    hPutStrLn h "P3"
    let nx = 200
        ny = 100
    hPrint h (floor nx)
    hPrint h (floor ny)
    hPrint h 255
    forM_ [(x, y) | y <- [ny-1, ny-2..0], x <- [0..nx-1]] $ \(x, y) -> do
        let u = conv $ x / nx
            v = conv $ y / ny
            lowerLeft = V3 (-2) (-1) (-1)
            horizontal = V3 4 0 0
            vertical = V3 0 2 0
            col = color $ Ray origin
                (lowerLeft + u*horizontal + v*vertical)
        hPutStrLn h $ mkRow col
