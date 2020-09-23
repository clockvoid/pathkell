module Main where

import Text.Printf
import Control.Monad
import Vec
import Numeric.Limits

height :: Int
height = 256

width :: Int
width = 256

eye :: Vec3
eye = vec3 0 0 5

sphere :: Vec3
sphere = vec3 0 0 0

radius :: Double
radius = 1

lightPos :: Vec3
lightPos = vec3 10 10 10

lightPower :: Double
lightPower = 4000

not_hit :: Double
not_hit = infinity

maxD :: Double -> Double -> Double
maxD x y
  | x <= 0                                = y
  | True                                  = x

calcPrimaryRay :: (Int, Int) -> Vec3
calcPrimaryRay (x, y) = normalize $ vec3 dx dy dz
  where
    dx = fromIntegral x + 0.5 - fromIntegral width / 2
    dy = -(fromIntegral y + 0.5 - fromIntegral height / 2)
    dz = -(fromIntegral height)

intersectRaySphere :: Vec3 -> Vec3 -> Vec3 -> Double -> Double
intersectRaySphere rayOrigin rayDir sphereCenter sphereRadius
  | 0 > d = not_hit
  | 0 < t = t
  | True  = not_hit
  where
   v = rayOrigin - sphereCenter
   b = rayDir `dot` v
   c = v `dot` v - sphereRadius ** 2
   d = b * b - c
   s = sqrt d
   t = maxD (-b - s) (-b + s)

calcPixelColor :: (Int, Int) -> (Int, Int, Int)
calcPixelColor dir = 
  if t == not_hit
     then (0, 0, 0)
     else (i, i, i)
  where
    ray = calcPrimaryRay dir
    t = intersectRaySphere eye ray sphere radius
    brightness = diffuseLighting eye ray sphere t
    i = min (round (brightness * 255)) 255

diffuseLighting :: Vec3 -> Vec3 -> Vec3 -> Double -> Double
diffuseLighting eye ray sphere t = 
  if dotNL > 0
     then lightPower * dotNL / (4 * pi * r * r)
     else 0
  where
    p = eye + (t |* ray)
    n = normalize $ p - sphere
    v = lightPos - p
    l = normalize v
    dotNL = n `dot` l
    r = Vec.length v

draw :: [(Int, Int, Int)]
draw = map calcPixelColor [(x, y) | y <- [0..width - 1], x <- [0..height - 1]]

main :: IO ()
main = do
  putStrLn "P3"
  putStr "256"
  putStr " "
  putStrLn "256"
  putStrLn "255"
  forM_ draw $ \(r, g, b) -> do
    printf "%d %d %d\n" r g b

