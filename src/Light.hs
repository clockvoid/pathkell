module Light where

import Base.Vec
import Base.Spectrum

data Light = Light
  { pos :: Vec3
  , power :: Spectrum
  } deriving (Eq, Show)

