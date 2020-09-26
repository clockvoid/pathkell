module Object where

import Vec
import Ray
import Material
import Intersectable
import Intersection

data Object = 
      Sphere Vec3 Double Material
    | Plane Vec3 Double Material
    | CheckedObj Object Double Material
    deriving Show

plane :: Vec3 -> Vec3 -> Material -> Object
plane p n = Plane (normalize n) d
  where
    d = -p `dot` normalize n

maxD :: Double -> Double -> Double
maxD x y
  | x <= 0    = y
  | otherwise = x

instance Intersectable Object where
  intersect (Sphere sphereCenter sphereRadius material) ray
    | 0 > d     = NO_HIT
    | 0 < t     = Intersection t p n material
    | otherwise = NO_HIT
    where
      rayOrigin = org ray
      rayDir = dir ray
      v = rayOrigin - sphereCenter
      b = rayDir `dot` v
      c = v `dot` v - sphereRadius ** 2
      d = b * b - c
      s = sqrt d
      t = maxD (-b - s) (-b + s)
      p = rayOrigin + rayDir *| t
      n = normalize $ p - sphereCenter
  intersect (Plane n d material) ray
    | 0 < t      = Intersection t (rayOrigin + rayDir *| t) n material
    | otherwise  = NO_HIT
    where
      rayOrigin = org ray
      rayDir = dir ray
      v = n `dot` rayDir
      t = -(n `dot` rayOrigin + d) / v
  intersect (CheckedObj obj gridWidth material) ray
    | isect /= NO_HIT && ((i `mod` 2) == 0) = Intersection (t isect) (p isect) (n isect) material
    | otherwise                             = isect
    where
      isect = intersect obj ray
      _vec = vmap (/ gridWidth) (p isect)
      i = round (vecX _vec) + round (vecY _vec) + round (vecZ _vec)

