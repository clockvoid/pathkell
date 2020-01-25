module Color where

import Vec

type Color = Vec3

color :: Double -> Double -> Double -> Color
color = vec3

background :: Color
background = color 0 0 0
