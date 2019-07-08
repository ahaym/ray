module Main where

import Control.Monad
import System.IO
import System.Random

import Camera
import qualified Chapter3 as Ch3
import Ray
import Sphere
import V3

main :: IO ()
main = withFile "chapter7.ppm" WriteMode $ \h -> do
    hPutStrLn h "P3"
    let nx = 200
        ny = 100
        ns = 100
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
        cols <- forM rands $ \(rand1, rand2) -> 
            let u = (x + rand1) / nx
                v = (y + rand2) / ny
                r = getRay cam u v
            in color world r
        let col = sum cols / (conv $ fromIntegral ns)
        hPutStrLn h $ mkRow col

randomUS :: IO V3
randomUS = do
    v <- let action = randomRIO (-1, 1) in V3 <$> action <*> action <*> action
    if squaredMag v < 1 then return v else randomUS

color :: Hitable -> Ray -> IO Color3
color h r = case hitAbstract (0.001, 999999999) h r of
    Just (HitData _ recP recNormal) -> do
        rv <- randomUS
        let target = recP + recNormal + rv
        col <- color h $ Ray recP (target - recP)
        return $ (conv 0.5)*col
    Nothing -> return $ Ch3.color r

