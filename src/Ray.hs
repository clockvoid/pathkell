module Ray where

import Vec

data Ray = Ray Vec3 Vec3 deriving (Eq, Show)

org :: Ray -> Vec3
org (Ray v1 v2) = v1

dir :: Ray -> Vec3
dir (Ray v1 v2) = v2
