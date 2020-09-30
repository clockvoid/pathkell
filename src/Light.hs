module Light where

import Base.Vec
import Base.Spectrum

data Light = Light Vec3 Spectrum deriving (Eq, Show)

pos :: Light -> Vec3
pos (Light position _) = position

power :: Light -> Spectrum
power (Light _ pow) = pow

