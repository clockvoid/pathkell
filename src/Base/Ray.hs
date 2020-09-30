module Base.Ray where

import Base.Vec

data Ray = Ray Vec3 Vec3 deriving (Eq, Show)

epsilon :: Double
epsilon = 0.001

org :: Ray -> Vec3
org (Ray v1 v2) = v1 + v2 *| epsilon

dir :: Ray -> Vec3
dir (Ray v1 v2) = normalize v2

