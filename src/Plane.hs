module Plane where

import Vec
import Ray
import Material
import Intersectable
import Intersection

plane :: Vec3 -> Vec3 -> Material -> Object
plane p n material = Plane (normalize n) d material
  where
    d = -p `dot` (normalize n)

planeN :: Object -> Vec3
planeN (Plane n _ _) = n

distance :: Object -> Double
distance (Plane _ d _) = d

