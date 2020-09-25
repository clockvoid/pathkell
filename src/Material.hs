module Material where

import Spectrum

data Material = Material
  { reflective :: Double
  , diffuse :: Spectrum
  } deriving (Eq, Show)

