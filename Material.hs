module Material where

import Ray
import Sphere
import V3

data MatSphere = MSphere Sphere Material deriving Show

data MatHitable
    = MS MatSphere
    | ML [MatHitable]
    deriving Show

data Material
    = Matte Color3
    | Metal Double Color3
    deriving Show

scatter :: Material -> Ray -> HitData -> IO (Maybe (Ray, Color3))

scatter (Matte albedo) (Ray inter slope) (HitData _ recP recNorm) = do
    rv <- randomUS
    let target = recNorm + rv
        scattered = Ray recP target
    return $ Just (scattered, albedo)

scatter (Metal fuzz albedo) (Ray inter slope) (HitData _ recP recNorm) = do
    rv <- randomUS
    let reflected = reflect (mkUnit slope) recNorm
        scattered@(Ray _ slope') = Ray recP (reflected + (conv fuzz)*rv)
    return $ if dot slope' recNorm > 0
        then Just (scattered, albedo)
        else Nothing

reflect :: V3 -> V3 -> V3
reflect v n = v - 2*(conv $ dot v n)*n

hitWithMat :: (Double, Double) -> MatHitable -> Ray -> Maybe (HitData, Material)
hitWithMat interval (MS (MSphere sp mat)) r = case hsCh5 interval sp r of
    Just r -> Just (r, mat)
    Nothing -> Nothing
hitWithMat (tmin, tmax) (ML hs) r = foldl go Nothing hs
    where
        go hd'm x = let tmax' = maybe tmax (t . fst) hd'm in case hitWithMat (tmin, tmax') x r of
            Nothing -> hd'm
            res -> res

msph :: V3 -> Double -> Material -> MatHitable
msph v r m = MS $ MSphere (Sphere v r) m
