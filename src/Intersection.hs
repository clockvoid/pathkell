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
intersectionT (Intersection t _ _ _) = t

intersectionP :: Intersection -> Vec3
intersectionP (Intersection _ p _ _) = p

intersectionN :: Intersection -> Vec3
intersectionN (Intersection _ _ n _) = n

intersectionMaterial :: Intersection -> Material
intersectionMaterial (Intersection _ _ _ m) = m

