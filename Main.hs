{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad
import Data.Maybe
import System.IO
import System.Random

import Camera
import Material
import Ray
import Sphere
import V3

main :: IO ()
main = withFile "lightball.ppm" WriteMode $ \h -> do
    hPutStrLn h "P3"
    let nx = 1000
        ny = 500
        ns = 100
        lookFrom = V3 12 2 3
        lookAt = V3 0 0 (-1)
        vup = (V3 0 1 0)
        vfov = 20
        aspect = 2
        aperture = 0.1
        focusDist = 10
        cam = mkCamera lookFrom lookAt vup vfov aspect aperture focusDist
    hPrint h (floor nx)
    hPrint h (floor ny)
    hPrint h 255
    world <- randomScene
    forM_ [(x, y) | y <- [ny-1, ny-2..0], x <- [0..nx-1]] $ \(x, y) -> do
        rands <- replicateM ns $ let action = randomRIO (0, 0.9999999) in (,) <$> action <*> action
        cols <- forM rands $ \(rand1, rand2) -> do
            let u = (x + rand1) / nx
                v = (y + rand2) / ny
            r <- getRay cam u v
            color world r 0
        let col = sum cols / (conv $ fromIntegral ns)
        hPutStrLn h $ mkRow col

color :: Hitable -> Ray -> Int -> IO Color3
color h r depth = case hitscan (0.001, 999999999) h r of
    Just (_,Light) -> return $ color3 1 1 1
    Just (hd,recMat)
        | depth < 50 -> scatter recMat r hd >>= \case
            Just (r', attenuation) -> (attenuation*) <$> color h r' (depth + 1) 
            _ -> return $ color3 0 0 0
        | otherwise -> return $ color3 0 0 0
    Nothing -> return $ color3 0.1 0.1 0.1 --(conv $ 1 - t)*(color3 1 1 1) + (conv t)*(color3 0.5 0.7 1)
    where
        unitDir = mkUnit $ slope r
        t = 0.5*(y unitDir + 1)

randomScene :: IO Hitable
randomScene = do
    let initialWorld =
            [ sphere (V3 0 1 0) 1 (Glass 1.5)
            , sphere (V3 0 (-1000) (0)) 1000 (Matte $ color3 0.6 0.6 0.6)
            , sphere (V3 (-4) 1 0) 1 (Matte $ color3 0.8 0.3 0.3)           
            , sphere (V3 4 1 0) 1 (Metal 0.2 $ color3 0.8 0.6 0.2)
            , sphere (V3 14 9 7) 6.5 Light
            ]
    objects'm <- forM [(a, b) | a <- [-11..11], b <- [-11..11]] $ \(a, b) -> do
        chooseMat <- randomRIO (0, 2) :: IO Int
        centerX <- randomRIO (0, 0.9)
        centerZ <- randomRIO (0, 0.9)
        let center = V3 (a + centerX) 0.2 (b + centerZ)
        if (mag $ center - (V3 4 0.2 0)) > 0.9
            then case chooseMat of
                0 -> Just <$> randomMatte center
                1 -> Just <$> randomMetal center
                2 -> return $ Just (sphere center 0.2 (Glass 1.5))
            else return Nothing
    return . HList $ initialWorld ++ catMaybes objects'm
    where
        randomMatte center = do
            let r2 = (*) <$> randomRIO (0, 1) <*> randomRIO (0, 1)
            albedo <- color3 <$> r2 <*> r2 <*> r2
            return $ sphere center 0.2 (Matte albedo)
        randomMetal center = do
            let action = randomRIO (0, 1)
            fuzz <- action
            albedo <- color3 <$> action <*> action <*> action
            return $ sphere center 0.2 (Metal fuzz albedo)
