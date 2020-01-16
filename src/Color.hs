module Color where

import Vec

type Color = Vec3

color :: Double -> Double -> Double -> Color
color x y z = vec3 x y z
