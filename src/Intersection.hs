module Intersection where

import Base.Vec
import Material

data Intersection = Intersection
  { t :: Double
  , p :: Vec3
  , n :: Vec3
  , material :: Material
  } | NO_HIT deriving (Eq, Show)

intersectionT :: Intersection -> Double
intersectionT = t

intersectionP :: Intersection -> Vec3
intersectionP = p

intersectionN :: Intersection -> Vec3
intersectionN = n

intersectionMaterial :: Intersection -> Material
intersectionMaterial = material

