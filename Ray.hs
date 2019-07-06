module Ray where

import V3

data Ray = Ray
    { inter:: V3
    , slope :: V3
    } deriving (Show)

pointAt :: Double -> Ray -> V3
pointAt t r = inter r + (conv t)*(slope r)
