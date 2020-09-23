module Object where

import Vec
import Ray
import Material
import Intersectable
import Intersection

data Object = Sphere Vec3 Double Material | Plane Vec3 Double Material deriving Show

plane :: Vec3 -> Vec3 -> Material -> Object
plane p n material = Plane (normalize n) d material
  where
    d = -p `dot` (normalize n)

maxD :: Double -> Double -> Double
maxD x y
  | x <= 0 = y
  | True   = x

instance Intersectable Object where
  intersect (Sphere sphereCenter sphereRadius material) ray
    | 0 > d = NO_HIT
    | 0 < t = Intersection t p n material
    | True  = NO_HIT
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
    | 0 < t = Intersection t (rayOrigin + rayDir *| t) n material
    | True  = NO_HIT
    where
      rayOrigin = org ray
      rayDir = dir ray
      v = n `dot` rayDir
      t = -(n `dot` rayOrigin + d) / v

