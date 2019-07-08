module Chapter5 where

import Control.Monad
import System.IO

import qualified Chapter3 as Ch3
import Ray
import Sphere
import V3

color :: Hitable -> Ray -> Color3
color h r = case hitAbstract (0, 9999999) h r of
    Just (HitData _ _ (V3 x y z )) -> (conv 0.5)*(color3 (x + 1) (y + 1) (z + 1))
    Nothing -> Ch3.color r

main :: IO ()
main = withFile "chapter5.ppm" WriteMode $ \h -> do
    hPutStrLn h "P3"
    let nx = 400
        ny = 200
    hPrint h (floor nx)
    hPrint h (floor ny)
    hPrint h 255
    forM_ [(x, y) | y <- [ny-1, ny-2..0], x <- [0..nx-1]] $ \(x, y) -> do
        let u = conv $ x / nx
            v = conv $ y / ny
            lowerLeft = V3 (-2) (-1) (-1)
            horizontal = V3 4 0 0
            vertical = V3 0 2 0
            world = HList [HSphere $ Sphere (V3 0 0 (-1)) 0.5
                ,HSphere $ Sphere (V3 0 (-100.5) (-1)) 100]
            col = color world $ Ray origin (lowerLeft + u*horizontal + v*vertical)
        hPutStrLn h $ mkRow col
