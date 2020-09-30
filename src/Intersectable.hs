module Intersectable where

import Base.Ray
import Base.Vec
import Material
import Intersection

class Intersectable a where
  intersect :: a -> Ray -> Intersection

