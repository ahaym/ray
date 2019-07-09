module Ray where

import System.Random

import V3

data Ray = Ray
    { inter :: V3
    , slope :: V3
    } deriving (Show)

pointAt :: Double -> Ray -> V3
pointAt t r = inter r + (conv t)*(slope r)

randomUS :: IO V3
randomUS = do
    v <- let action = randomRIO (-1, 1) in V3 <$> action <*> action <*> action
    if squaredMag v < 1 then return v else randomUS


reflect :: V3 -> V3 -> V3
reflect v n = v - 2*(conv $ dot v n)*n

data HitData = HitData
    { t :: Double
    , hitPoint :: V3
    , normal :: V3
    } deriving Show
