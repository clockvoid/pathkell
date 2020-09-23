module Intersectable where

import Intersection
import Ray
import Material
import Vec

data Object = Sphere Vec3 Double Material | Plane Vec3 Double Material deriving Show

class Intersectable a where
  intersect :: a -> Ray -> Intersection

