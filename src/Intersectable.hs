module Intersectable where

import Intersection
import Ray
import Material
import Vec

class Intersectable a where
  intersect :: a -> Ray -> Intersection

