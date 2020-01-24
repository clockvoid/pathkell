module Object where

import Ray

class Object a where
  intersect :: a -> Ray -> Double
