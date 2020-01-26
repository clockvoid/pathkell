module Sphere where

import Vec
import Color
import ReflectionType
import Ray
import Object

eps :: Double
eps = 0.0000001

data Sphere = Sphere Double Vec3 Color Color ReflectionType deriving Show

sphere :: Double -> Vec3 -> Color -> Color -> ReflectionType -> Sphere
sphere = Sphere

radius :: Sphere -> Double
radius (Sphere r _ _ _ _) = r

position :: Sphere -> Vec3
position (Sphere _ p _ _ _) = p

emission :: Sphere -> Color
emission (Sphere _ _ e _ _) = e

sphereColor :: Sphere -> Color
sphereColor (Sphere _ _ _ c _) = c

instance Object Sphere where
  intersect sphere ray
    | (det >= 0) && (t1 > eps) = t1
    | (det >= 0) && (t2 > eps) = t2
    | otherwise  = 0
    where
      o_p = position sphere - org ray
      b = o_p `dot` dir ray
      det = b * b - (o_p `dot` o_p) + radius sphere * radius sphere
      t1 = b - sqrt det
      t2 = b + sqrt det
