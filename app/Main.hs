module Main where

import Object
import Color
import Vec
import Sphere
import Ray
import ReflectionType
import Camera

camera1 :: Camera
camera1 = camera (vec3 0 0 0) (vec3 4 0 0) (vec3 0 2 0) (vec3 (-2) (-1) (-1))

screen :: [Vec3]
screen = [vec3 0 (y / 256) (z / 256) | y <- [0..256], z <- [0..256]]

rays :: [Ray]
rays = map (\(Vec3 x y z) -> getRay y z camera1) screen

radius1 :: Double
radius1 = 10

position1 :: Vec3
position1 = vec3 10 0 0

emission1 :: Color
emission1 = color 0 0 0

sphereColor1 :: Color
sphereColor1 = color 1 1 1

reflectionType1 :: ReflectionType
reflectionType1 = DIFFUSE

sphere1 :: Sphere
sphere1 = sphere radius1 position1 emission1 sphereColor1 reflectionType1

ts :: [Double]
ts = [sphere1 `intersect` ray | ray <- rays]

hitpoints :: [Vec3]
hitpoints = zipWith hited rays ts
  where
    hited ray t = if t == 0 then vec3 0 0 0 else hit ray t
    hit ray t = org ray + t |* dir ray

isHit :: [Vec3]
isHit = map hit ts
  where
    hit t = if t == 0 then vec3 0 0 0 else vec3 1 1 1

transformColor :: Ray -> Color
transformColor (Ray org dir) = fromVec3 $ normalize $ lerp t (Vec3 0.5 0.7 1) (Vec3 1 1 1)
  where
    d = normalize dir 
    t = 0.5 * (vecY dir + 1)

main :: IO ()
main = 
  print $ map (clampToString . transformColor) rays
  --print $ map (clampToString . fromVec3) isHit

