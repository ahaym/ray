module Material where

import System.Random

import Ray
import V3

data Material
    = Matte Color3
    | Metal Double Color3
    | Glass Double
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

scatter (Glass refIdx) (Ray inter slope) (HitData _ recP recNorm) = randomRIO (0, 0.999) >>= \rv ->
    let attenuation = color3 1 1 1
        proj = dot slope recNorm
        (outwardNormal, ratio, cosine) = if proj > 0
            then (-recNorm, refIdx, refIdx*proj / (mag slope))
            else (recNorm, 1 / refIdx, -proj / (mag slope))
        reflected = reflect slope recNorm
    in return . Just $ case refract slope outwardNormal ratio of
        Just refracted -> if rv < schlick cosine refIdx
            then (Ray recP reflected, attenuation)
            else (Ray recP refracted, attenuation)
        Nothing -> (Ray recP reflected, attenuation)

reflect :: V3 -> V3 -> V3
reflect v n = v - 2*(conv $ dot v n)*n

refract :: V3 -> V3 -> Double -> Maybe V3
refract v n ratio = if discriminant > 0 then Just refracted else Nothing
    where
        uv = mkUnit v
        dt = dot uv n
        discriminant = 1 - (ratio^2)*(1 - dt^2)
        refracted = (conv ratio)*(uv - n*(conv dt)) - n*(conv $ sqrt discriminant)

schlick :: Double -> Double -> Double
schlick cosine refIdx = r0 + (1 - r0)*(1 - cosine)^5
    where
        r0 = ((1 - refIdx) / (1 + refIdx))^2
