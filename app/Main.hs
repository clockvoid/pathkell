module Main where

import Text.Printf
import Control.Monad
import Vec
import Spectrum
import Numeric.Limits
import Scene
import Intersectable
import Sphere
import Plane
import Ray
import Scene
import Light
import Material

height :: Int
height = 256

width :: Int
width = 256

intersectableList :: [Object]
intersectableList = [
  Sphere (Vec3 (-2) 0 0) 0.8 (Material (Spectrum 0.9 0.1 0.5)),
  Sphere (Vec3 0 0 0) 0.8 (Material (Spectrum 0.1 0.9 0.5)),
  Sphere (Vec3 2 0 0) 0.8 (Material (Spectrum 0.1 0.5 0.9)),
  plane (Vec3 0 (-0.8) 0) (Vec3 0 1 0) (Material (Spectrum 0.8 0.8 0.8))
                 ]

lightList :: [Light]
lightList = [
  Light (Vec3 100 100 100) (Spectrum 400000 100000 400000),
  Light (Vec3 (-100) 100 100) (Spectrum 100000 400000 100000)
            ]

scene :: Scene
scene = Scene intersectableList lightList

eye :: Vec3
eye = vec3 0 0 7

calcPrimaryRay :: (Int, Int) -> Ray
calcPrimaryRay (x, y) = Ray eye (normalize $ vec3 dx dy dz)
  where
    dx = fromIntegral x + 0.5 - fromIntegral width / 2
    dy = -(fromIntegral y + 0.5 - fromIntegral height / 2)
    dz = -(fromIntegral height)

calcPixelColor :: (Int, Int) -> (Int, Int, Int)
calcPixelColor dir = toColor l
  where
    ray = calcPrimaryRay dir
    l = trace scene ray

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

