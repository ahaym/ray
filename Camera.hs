module Camera where

import Ray
import V3

data Camera = Camera
    { lowerLeft_ :: V3
    , horizontal_ :: V3
    , vertical_ :: V3
    , origin_ :: V3
    }

getRay :: Camera -> Double -> Double -> Ray
getRay (Camera ll horiz vert z) u v = Ray z $ ll + (conv u*horiz) + (conv v*vert) - z
