{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad
import System.IO
import System.Random

import Camera
import Material
import Ray
import Sphere
import V3

main :: IO ()
main = withFile "chapter8.ppm" WriteMode $ \h -> do
    hPutStrLn h "P3"
    let nx = 200
        ny = 100
        ns = 100
        lowerLeft = V3 (-2) (-1) (-1)
        horizontal = V3 4 0 0
        vertical = V3 0 2 0
        cam = Camera lowerLeft horizontal vertical (V3 0 0 0)
        world = HList
            [ sphere (V3 0 0 (-1)) 0.5 (Matte $ color3 0.8 0.3 0.3)
            , sphere (V3 0 (-100.5) (-1)) 100 (Matte $ color3 0.8 0.8 0)
            , sphere (V3 1 0 (-1)) 0.5 (Metal 1 $ color3 0.8 0.6 0.2)
            , sphere (V3 (-1) 0 (-1)) 0.5 (Metal 0.3 $ color3 0.8 0.8 0.8)
            ]
    hPrint h (floor nx)
    hPrint h (floor ny)
    hPrint h 255
    forM_ [(x, y) | y <- [ny-1, ny-2..0], x <- [0..nx-1]] $ \(x, y) -> do
        rands <- replicateM ns $ let action = randomRIO (0, 0.9999999) in (,) <$> action <*> action
        cols <- forM rands $ \(rand1, rand2) -> 
            let u = (x + rand1) / nx
                v = (y + rand2) / ny
                r = getRay cam u v
            in color world r 0
        let col = sum cols / (conv $ fromIntegral ns)
        hPutStrLn h $ mkRow col

color :: Hitable -> Ray -> Int -> IO Color3
color h r depth = case hitscan (0.001, 999999999) h r of
    Just (hd,recMat)
        | depth < 50 -> scatter recMat r hd >>= \case
            Just (r', attenuation) -> (attenuation*) <$> color h r' (depth + 1) 
            _ -> return $ color3 0 0 0
        | otherwise -> return $ color3 0 0 0
    Nothing -> return $ (conv $ 1 - t)*(color3 1 1 1) + (conv t)*(color3 0.5 0.7 1)
    where
        unitDir = mkUnit $ slope r
        t = 0.5*(y unitDir + 1)

