module Vec where

class Vec a where
  (/|) :: a -> Double -> a
  (*|) :: a -> Double -> a
  (|*) :: Double -> a -> a
  dot :: a -> a -> Double
  cross :: a -> a -> a
  lenSquared :: a -> Double
  length :: a -> Double
  normalize :: a -> a
  vmap :: (Double -> Double) -> a -> a
  vpromote :: Double -> a
  lerp :: Double -> a -> a -> a

data Vec3 = Vec3 Double Double Double deriving (Eq)

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
  normalize vec = vec /| Vec.length vec
  vmap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)
  vpromote a = Vec3 a a a
  lerp alpha v1 v2 = ((v2 - v1) *| alpha) + v1

vec3 :: Double -> Double -> Double -> Vec3
vec3 = Vec3

vecX :: Vec3 -> Double
vecX (Vec3 x _ _) = x

vecY :: Vec3 -> Double
vecY (Vec3 _ y _) = y

vecZ :: Vec3 -> Double
vecZ (Vec3 _ _ z) = z

instance Show Vec3 where
  show (Vec3 x y z) = show x ++ " " ++ show y ++ " " ++ show z ++ "\n"

radians :: Double -> Double
radians deg = deg / 180 * pi

degrees :: Double -> Double
degrees rad = rad / pi * 180
