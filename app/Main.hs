module Main where

import Text.Printf
import Control.Monad
import Vec
import Spectrum
import Numeric.Limits
import Scene
import Intersectable
import Object
import Ray
import Scene
import Light
import Material

height :: Int
height = 1080

width :: Int
width = 1920

intersectableList :: [Object]
intersectableList = [
  Sphere (Vec3 (-1) 0 0) 1 (Material 0 0.9 1.5 (Spectrum 0.1 0.5 0.9)),
  Sphere (Vec3 1 0 0) 1 (Material 0.8 0 1 (Spectrum 0.9 0.1 0.5)),
  -- Sphere (Vec3 0 0 (2)) 1 (Material 0.8 0.9 1.8 (Spectrum 0.5 0.9 0.1)),
  -- Sphere (Vec3 (-2) 0 0) 0.8 (Material (Spectrum 0.9 0.1 0.5)),
  -- Sphere (Vec3 0 0 0) 0.8 (Material (Spectrum 0.1 0.9 0.5)),
  -- Sphere (Vec3 2 0 0) 0.8 (Material (Spectrum 0.1 0.5 0.9)),
  -- Sphere (Vec3 0 2 0) 0.8 (Material (Spectrum 0.2 0.2 0.2)),
  CheckedObj _plane 1 mtlFloor2,
  CheckedObj _planeWall 1 mtlFloor2
                 ]
  where
    mtlFloor1 = Material 0 0 1 (Spectrum 0.5 0.5 0.5)
    mtlFloor2 = Material 0 0 1 (Spectrum 0.2 0.2 0.2)
    _plane = plane (Vec3 0 (-1) 0) (Vec3 0 1 0) mtlFloor1
    _planeWall = plane (Vec3 0 0 (-5)) (Vec3 0 0 1) mtlFloor1

lightList :: [Light]
lightList = [
  Light (Vec3 100 100 100) (Spectrum 800000 800000 800000)
  -- Light (Vec3 100 100 100) (Spectrum 400000 100000 400000),
  -- Light (Vec3 (-100) 100 100) (Spectrum 100000 400000 100000)
  -- Light (Vec3 100 (-100) 100) (Spectrum 100000 400000 400000)
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
    l = trace 0 scene black ray

draw :: [(Int, Int, Int)]
draw = map calcPixelColor [(x, y) | y <- [0..height - 1], x <- [0..width - 1]]

main :: IO ()
main = do
  putStrLn "P3"
  putStr $ show width
  putStr " "
  putStrLn $ show height
  putStrLn "255"
  forM_ draw $ \(r, g, b) -> do
    printf "%d %d %d\n" r g b

