module Chapter4 where

import Control.Monad
import System.IO

import Ray
import Sphere
import V3

main :: IO ()
main = withFile "chapter4.ppm" WriteMode $ \h -> do
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
            sphere = Sphere (V3 0 0 (-1)) 0.5
            col = colorCh4 sphere $ Ray origin
                (lowerLeft + u*horizontal + v*vertical)
        hPutStrLn h $ mkRow col
