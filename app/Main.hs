module Main where

import Text.Printf
import Control.Monad
import Vec

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

calcPrimaryRay :: (Int, Int) -> Vec3
calcPrimaryRay (x, y) = normalize $ vec3 dx dy dz
  where
    dx = fromIntegral x + 0.5 - fromIntegral width / 2
    dy = -(fromIntegral y + 0.5 - fromIntegral height / 2)
    dz = -(fromIntegral height)

intersectRaySphere :: Vec3 -> Vec3 -> Vec3 -> Double -> Bool
intersectRaySphere rayOrigin rayDir sphereCenter sphereRadius = 0 <= (b * b - c)
  where
   v = rayOrigin - sphereCenter
   b = rayDir `dot` v
   c = v `dot` v - sphereRadius ** 2

calcPixelColor :: (Int, Int) -> (Int, Int, Int)
calcPixelColor dir = if intersectRaySphere eye ray sphere radius
                        then
                          (255, 255, 255)
                        else
                          (0, 0, 0)
                          where
                            ray = calcPrimaryRay dir

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

