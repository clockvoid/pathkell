module Base.Vec where

class Vec a where
  (/|) :: a -> Double -> a
  (*|) :: a -> Double -> a
  (|*) :: Double -> a -> a
  dot :: a -> a -> Double
  cross :: a -> a -> a
  lenSquared :: a -> Double
  length :: a -> Double
  normalize :: a -> a
  reflect :: a -> a -> a
  reflact :: a -> a -> Double -> a
  vmap :: (Double -> Double) -> a -> a
  vpromote :: Double -> a
  lerp :: Double -> a -> a -> a

data Vec3 = Vec3
  { vecX :: Double 
  , vecY :: Double
  , vecZ :: Double
  } deriving (Eq)

instance Num Vec3 where
  (Vec3 x1 y1 z1) + (Vec3 x2 y2 z2) = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)
  (Vec3 x1 y1 z1) - (Vec3 x2 y2 z2) = Vec3 (x1 - x2) (y1 - y2) (z1 - z2)
  (Vec3 x1 y1 z1) * (Vec3 x2 y2 z2) = Vec3 (x1 * x2) (y1 * y2) (z1 * z2)
  abs = vmap abs
  signum = vmap signum
  fromInteger = vpromote . fromInteger

instance Vec Vec3 where
  (Vec3 x1 y1 z1) /| b = Vec3 (x1 / b) (y1 / b) (z1 / b)
  (Vec3 x1 y1 z1) *| b = Vec3 (x1 * b) (y1 * b) (z1 * b)
  b |* (Vec3 x1 y1 z1) = Vec3 (b * x1) (b * y1) (b * z1)
  dot (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2
  cross (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 ((y1 * z2) - (z1 * y2)) ((z1 * x2) - (x1 * z2)) ((x1 * y2) - (y1 * x2))
  lenSquared (Vec3 x y z) = x * x + y * y + z * z
  length vec = sqrt $ lenSquared vec
  normalize vec = vec /| Base.Vec.length vec
  reflect self n = self - n *| (2 * self `dot` n)
  reflact self n eta
    | 0 < d     = a - b
    | otherwise = reflect self n
    where
      dotN = self `dot` n
      d = 1 - eta ** 2 * (1 - dotN ** 2)
      a = (self - n *| dotN) *| eta
      b = n *| sqrt d
  vmap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)
  vpromote a = Vec3 a a a
  lerp alpha v1 v2 = ((v2 - v1) *| alpha) + v1

vec3 :: Double -> Double -> Double -> Vec3
vec3 = Vec3

instance Show Vec3 where
  show (Vec3 x y z) = show x ++ " " ++ show y ++ " " ++ show z ++ "\n"

radians :: Double -> Double
radians deg = deg / 180 * pi

degrees :: Double -> Double
degrees rad = rad / pi * 180

