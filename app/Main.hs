module Main where

import Text.Printf
import Control.Monad

height :: Float
height = 256

width :: Float
width = 256

eye = (0, 0, 5)
sphere = (0, 0, 0)
radius = 1

intersectRaySphere :: (Float, Float, Float) -> (Float, Float, Float) -> (Float, Float, Float) -> Float -> Bool
intersectRaySphere (ox, oy, oz) (dx, dy, dz) (cx, cy, cz) r = 0 <= (b * b - 4 * a * c)
  where
   a = dx * dx + dy * dy + dz * dz
   b = 2 * (dx * (ox - cx) + dy * (oy - cy) + dz * (oz - cz))
   c = (ox - cx) ** 2 + (oy - cy) ** 2 + (oz - cz) ** 2 - r ** 2

calcPixelColor :: (Float, Float) -> (Int, Int, Int)
-- calcPixelColor(x, y) = (round (abs (x + 0.5 - width / 2)), round (abs (y + 0.5 - height / 2)), 0)
calcPixelColor (x, y) = if intersectRaySphere eye ray sphere radius
                        then
                          (255, 255, 255)
                        else
                          (0, 0, 0)
                          where
                            ray = (x + 0.5 - width / 2, -(y + 0.5 - height / 2), -height)

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

