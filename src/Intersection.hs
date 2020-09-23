module Intersection where

import Vec
import Material

data Intersection = Intersection Double Vec3 Vec3 Material | NO_HIT deriving (Eq, Show)

intersectionT :: Intersection -> Double
intersectionT (Intersection t _ _ _) = t

intersectionP :: Intersection -> Vec3
intersectionP (Intersection _ p _ _) = p

intersectionN :: Intersection -> Vec3
intersectionN (Intersection _ _ n _) = n

intersectionMaterial :: Intersection -> Material
intersectionMaterial (Intersection _ _ _ m) = m

