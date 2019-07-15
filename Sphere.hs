module Sphere where

import Material
import Ray
import V3

data Sphere = Sphere 
    { center :: !V3
    , radius :: !Double
    , sphereMat :: !Material
    } deriving Show

data Hitable
    = HSphere !Sphere
    | HList ![Hitable]
    deriving Show

sphere :: V3 -> Double -> Material -> Hitable
sphere c r m = HSphere $ Sphere c r m

hitscan :: (Double, Double) -> Hitable -> Ray -> Maybe (HitData, Material)
hitscan (tmin, tmax) (HSphere (Sphere cent rad mat)) r@(Ray inter slop)
    | discriminant <= 0 = Nothing
    | temp1 > tmin && temp1 < tmax = mkHitData temp1
    | temp2 > tmin && temp2 < tmax = mkHitData temp2 
    | otherwise = Nothing
    where
        oc = inter - cent
        a = dot slop slop
        b = (dot oc slop)
        c = dot oc oc - rad^2
        discriminant = b^2 - a*c

        temp1 = (-b - sqrt (b^2 - a*c)) / a
        temp2 = (-b + sqrt (b^2 - a*c)) / a

        mkHitData t = let p = pointAt t r in
            Just $ (HitData t p ((p - cent) / conv rad), mat)

hitscan (tmin, tmax) (HList hs) r = foldl go Nothing hs
    where
        go hd'm x = let tmax' = maybe tmax (t . fst) hd'm in case hitscan (tmin, tmax') x r of
            Nothing -> hd'm
            res -> res
