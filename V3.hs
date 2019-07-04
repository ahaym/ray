{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module V3 where

class Vec3 v where
    conv :: Double -> v
    mkUnit :: v -> v
    dot :: v -> v -> Double
    cross :: v -> v -> v
    squaredMag :: v -> Double
    mag :: v -> Double
    mag = (^2) . squaredMag

data V3 = V3
    { x :: Double
    , y :: Double 
    , z :: Double
    } deriving (Eq, Show)

instance Num V3 where
    (V3 a1 b1 c1) + (V3 a2 b2 c2) = V3 (a1 + a2) (b1 + b2) (c1 + c2)
    (V3 a1 b1 c1) * (V3 a2 b2 c2) = V3 (a1 * a2) (b1 * b2) (c1 * c2)
    abs (V3 a b c) = V3 (abs a) (abs b) (abs c)
    signum (V3 a b c) = V3 (signum a) (signum b) (signum c)
    fromInteger x = let x' = fromIntegral x in V3 x' x' x'
    negate (V3 a b c) = V3 (-a) (-b) (-c)

instance Fractional V3 where
    (V3 a1 b1 c1) / (V3 a2 b2 c2) = V3 (a1 / a2) (b1 / b2) (c1 / c2)
    fromRational x = let x' = fromRational x in V3 x' x' x'

instance Vec3 V3 where
   conv = fromRational . toRational
   mkUnit v = v / (conv $ mag v)
   dot (V3 a1 b1 c1) (V3 a2 b2 c2) = a1*a2 + b1*b2 + c1*c2
   cross (V3 a1 a2 a3) (V3 b1 b2 b3) = V3 (a2*b3 - a3*b2) (a3*b1 - a1*b3) (a1*b2 - a2*b1)
   squaredMag (V3 a b c) = a^2 + b^2 + c^2

newtype Color3 = C_ { unC :: V3 } deriving (Eq, Show, Num, Fractional, Vec3)

r :: Color3 -> Double
r = x . unC

g :: Color3 -> Double
g = y . unC

b :: Color3 -> Double
b = z . unC

c :: Double -> Double -> Double -> Color3
c a b c = C_ (V3 a b c)