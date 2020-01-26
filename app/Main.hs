module Main where

import Object
import Color
import Vec
import Sphere
import Ray
import ReflectionType

camera :: Vec3
camera = vec3 0 0 0

screen :: [Vec3]
screen = [vec3 5 (y * (15 / 128)) (z * (15 / 128)) | y <- [-127..128], z <- [-127..128]]

rays :: [Ray]
rays = map (Ray camera . normalize) screen

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

main :: IO ()
main = 
  print $ map (clampToString . fromVec3 . normalize . (\vec -> vec - position1)) hitpoints
  --print $ map (clampToString . fromVec3) isHit

