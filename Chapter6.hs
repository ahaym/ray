module Chapter6 where

import Control.Monad
import System.IO
import System.Random

import Camera
import qualified Chapter5 as Ch5
import Ray
import Sphere
import V3

main :: IO ()
main = withFile "chapter6.ppm" WriteMode $ \h -> do
    hPutStrLn h "P3"
    let nx = 400
        ny = 200
        ns = 32
        lowerLeft = V3 (-2) (-1) (-1)
        horizontal = V3 4 0 0
        vertical = V3 0 2 0
        cam = Camera lowerLeft horizontal vertical (V3 0 0 0)
        world = HList [HSphere $ Sphere (V3 0 0 (-1)) 0.5 ,HSphere $ Sphere (V3 0 (-100.5) (-1)) 100]
    hPrint h (floor nx)
    hPrint h (floor ny)
    hPrint h 255
    forM_ [(x, y) | y <- [ny-1, ny-2..0], x <- [0..nx-1]] $ \(x, y) -> do
        rands <- replicateM ns $ let action = randomRIO (0, 0.9999999) in (,) <$> action <*> action
        let for = flip fmap
            cols = for rands $ \(rand1, rand2) -> 
                let u = (x + rand1) / nx
                    v = (y + rand2) / ny
                    r = getRay cam u v
                in Ch5.color world r
            col = sum cols / (conv $ fromIntegral ns)
        hPutStrLn h $ mkRow col
