module Material where

import Spectrum

data Material = Material Spectrum deriving (Eq, Show)

diffuse :: Material -> Spectrum
diffuse (Material color) = color

