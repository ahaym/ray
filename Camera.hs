module Camera where

import Ray
import RTM
import V3

data Camera = Camera
    { lowerLeft :: V3
    , horizontal :: V3
    , vertical :: V3
    , origin :: V3
    , u :: V3
    , v :: V3
    , lensRadius :: Double
    }


mkCamera :: V3 -> V3 -> V3 -> Double -> Double -> Double -> Double -> Camera
mkCamera lookFrom lookAt vup vfov aspect aperture focusDist_ = 
    Camera lowerLeft horizontal vertical origin u v (aperture / 2)
    where
        theta = vfov*(pi / 180)
        halfHeight = tan (theta / 2)
        halfWidth = aspect*halfHeight
        origin = lookFrom
        w = mkUnit $ lookFrom - lookAt
        u = mkUnit $ cross vup w
        v = cross w u
        focusDist = conv focusDist_
        lowerLeft = origin - (conv halfWidth)*focusDist*u - (conv halfHeight)*focusDist*v - focusDist*w
        horizontal = 2*(conv halfWidth)*focusDist*u
        vertical = 2*(conv halfHeight)*focusDist*v

getRay :: Camera -> Double -> Double -> RTM Ray
getRay (Camera ll horiz vert z u v lensRadius) s t = randomUS >>= \rv ->
    let V3 rdx rdy _ = (conv lensRadius)*rv
        offset = u*(conv rdx) + v*(conv rdy)
    in return $ Ray (z + offset) (ll + (conv s)*horiz + (conv t)*vert- z - offset)
