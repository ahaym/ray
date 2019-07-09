module Material where

import Ray
import V3

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
