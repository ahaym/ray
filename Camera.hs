module Camera where

import Ray
import V3

data Camera = Camera
    { lowerLeft :: V3
    , horizontal :: V3
    , vertical :: V3
    , origin :: V3
    }

mkCamera :: V3 -> V3 -> V3 -> Double -> Double -> Camera
mkCamera lookFrom lookAt vup vfov aspect = Camera lowerLeft horizontal vertical origin
    where
        theta = vfov*(pi / 180)
        halfHeight = tan (theta / 2)
        halfWidth = aspect*halfHeight
        origin = lookFrom
        w = mkUnit $ lookFrom - lookAt
        u = mkUnit $ cross vup w
        v = cross w u
        lowerLeft = origin - (conv halfWidth)*u - (conv halfHeight)*v - w
        horizontal = 2*(conv halfWidth)*u
        vertical = 2*(conv halfHeight)*v

getRay :: Camera -> Double -> Double -> Ray
getRay (Camera ll horiz vert z) u v = Ray z $ ll + (conv u*horiz) + (conv v*vert) - z
