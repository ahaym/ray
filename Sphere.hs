module Sphere where

import qualified Chapter3 as Ch3
import Ray
import V3

data Sphere = Sphere 
    { center :: V3
    , radius :: Double
    }

hsCh4 :: Sphere -> Ray -> Bool
hsCh4 (Sphere cent rad) (Ray inter slop) = discriminant > 0
    where
        oc = inter - cent
        a = dot slop slop
        b = 2 * (dot oc slop)
        c = dot oc oc - rad^2
        discriminant = b^2 - 4*a*c

color :: (Sphere -> Ray -> Bool) -> Sphere -> Ray -> Color3
color cFunc sphere ray
    | cFunc sphere ray = color3 1 0 0
    | otherwise = Ch3.color ray
