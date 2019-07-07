module Sphere where

import qualified Chapter3 as Ch3
import Ray
import V3

data Sphere = Sphere 
    { center :: V3
    , radius :: Double
    } deriving Show

data HitData = HitData
    { t :: Double
    , hitPoint :: V3
    , normal :: V3
    } deriving Show

data Hitable
    = HSphere Sphere
    | HList [Hitable]
    deriving Show

hsCh4 :: Sphere -> Ray -> Bool
hsCh4 (Sphere cent rad) (Ray inter slop) = discriminant > 0
    where
        oc = inter - cent
        a = dot slop slop
        b = 2 * (dot oc slop)
        c = dot oc oc - rad^2
        discriminant = b^2 - 4*a*c

hsCh5 :: (Double, Double) -> Sphere -> Ray -> Maybe HitData
hsCh5 (tmin, tmax) (Sphere cent rad) r@(Ray inter slop)
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
            Just $ HitData t p ((p - cent) / conv rad)

hitAbstract :: (Double, Double) -> Hitable -> Ray -> Maybe HitData
hitAbstract interval (HSphere sp) r = hsCh5 interval sp r
hitAbstract (tmin, tmax) (HList hs) r = foldl go Nothing hs
    where
        go hd'm x = let tmax' = maybe tmax t hd'm in case hitAbstract (tmin, tmax') x r of
            Nothing -> hd'm
            res -> res

colorCh4 :: Sphere -> Ray -> Color3
colorCh4 sphere ray
    | hsCh4 sphere ray = color3 1 0 0
    | otherwise = Ch3.color ray
