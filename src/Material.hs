module Material where

import Base.Spectrum

data Material = Material
  { reflective :: Double
  , reflactive :: Double
  , reflactiveIndex :: Double
  , diffuse :: Spectrum
  } deriving (Eq, Show)

