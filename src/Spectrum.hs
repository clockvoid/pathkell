module Spectrum where

data Spectrum = Spectrum Double Double Double

class SpectrumInterface a where
  (*||) :: a -> Double -> a
  (||*) :: Double -> a -> a
  toColor :: a -> (Int, Int, Int)
  vmapS :: (Double -> Double) -> a -> a
  vpromoteS :: Double -> a

instance SpectrumInterface Spectrum where
  (Spectrum x1 y1 z1) *|| b = Spectrum (x1 * b) (y1 * b) (z1 * b)
  b ||* (Spectrum x1 y1 z1) = Spectrum (b * x1) (b * y1) (b * z1)
  toColor (Spectrum r g b) = (ir, ig, ib)
    where
      ir = round $ min (r * 255) 255
      ig = round $ min (g * 255) 255
      ib = round $ min (b * 255) 255
  vmapS f (Spectrum x y z) = Spectrum (f x) (f y) (f z)
  vpromoteS a = Spectrum a a a

instance Num Spectrum where
  (Spectrum x1 y1 z1) + (Spectrum x2 y2 z2) = Spectrum (x1 + x2) (y1 + y2) (z1 + z2)
  (Spectrum x1 y1 z1) - (Spectrum x2 y2 z2) = Spectrum (x1 - x2) (y1 - y2) (z1 - z2)
  (Spectrum x1 y1 z1) * (Spectrum x2 y2 z2) = Spectrum (x1 * x2) (y1 * y2) (z1 * z2)
  abs = vmapS abs
  signum = vmapS signum
  fromInteger = vpromoteS . fromInteger

black :: Spectrum
black = Spectrum 0 0 0

