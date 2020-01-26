module Color where

import Vec

class ColorInterface a where
  clamp :: a -> (Int, Int, Int)
  clampToString :: a -> String

data Color = Color Double Double Double deriving (Show, Eq)

color :: Double -> Double -> Double -> Color
color = Color

background :: Color
background = color 0 0 0

fromVec3 :: Vec3 -> Color
fromVec3 (Vec3 x y z) = color x y z

instance ColorInterface Color where
  clamp (Color r g b) = (intR, intG, intB)
    where
      intR = abs $ floor $ 255.0 * r
      intG = abs $ floor $ 255.0 * g
      intB = abs $ floor $ 255.0 * b
  clampToString color = rString ++ " " ++ gString ++ " " ++ bString
    where
      (intR, intG, intB) = clamp color
      rString = show intR
      gString = show intG
      bString = show intB

