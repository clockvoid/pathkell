module Scene where

import Intersectable
import Light
import Ray
import Vec
import Intersection
import Spectrum
import Material
import Object

data Scene = Scene [Object] [Light] deriving Show

intersectables :: Scene -> [Object]
intersectables (Scene isect _) = isect

lights :: Scene -> [Light]
lights (Scene _ lightList) = lightList

trace :: Scene -> Ray -> Spectrum
trace scene ray
  | isect == NO_HIT = black
  | True            = lighting (lights scene) (intersectionP isect) (intersectionN isect) (intersectionMaterial isect)
    where
      isect = findNearestIntersection (intersectables scene) ray

compareIntersection :: Intersection -> Intersection -> Intersection
compareIntersection isect NO_HIT = isect
compareIntersection NO_HIT isect = isect
compareIntersection NO_HIT NO_HIT = NO_HIT
compareIntersection isect1 isect2 = if intersectionT isect1 < intersectionT isect2 then isect1 else isect2

findNearestIntersection :: [Object] -> Ray -> Intersection
findNearestIntersection isectables ray = foldr (\isectable isect -> compareIntersection (intersect isectable ray) isect) NO_HIT isectables 

lighting :: [Light] -> Vec3 -> Vec3 -> Material -> Spectrum
lighting lights p n m = foldr (\light spectrum -> spectrum + (diffuseLighting p n (diffuse m) light)) black lights

diffuseLighting :: Vec3 -> Vec3 -> Spectrum -> Light -> Spectrum
diffuseLighting p n diffuseColor light = 
  if dotNL > 0
     then lightPower *|| factor * diffuseColor
     else black
  where
    lightPos = pos light
    lightPower = power light
    v = lightPos - p
    l = normalize v
    dotNL = n `dot` l
    r = Vec.length v
    factor = dotNL / (4 * pi * r * r)

