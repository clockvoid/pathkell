module Scene where

import Intersectable
import Light
import Ray
import Vec
import Intersection
import Spectrum
import Material
import Object
import Numeric.Limits

data Scene = Scene [Object] [Light] deriving Show

intersectables :: Scene -> [Object]
intersectables (Scene isect _) = isect

lights :: Scene -> [Light]
lights (Scene _ lightList) = lightList

trace :: Scene -> Ray -> Spectrum
trace scene ray
  | isect == NO_HIT = black
  | True            = lighting (intersectables scene)(lights scene) (intersectionP isect) (intersectionN isect) (intersectionMaterial isect)
    where
      isect = findNearestIntersection (intersectables scene) ray

compareIntersection :: Intersection -> Intersection -> Intersection
compareIntersection isect NO_HIT = isect
compareIntersection NO_HIT isect = isect
compareIntersection NO_HIT NO_HIT = NO_HIT
compareIntersection isect1 isect2 = if intersectionT isect1 < intersectionT isect2 then isect1 else isect2

findNearestIntersection :: [Object] -> Ray -> Intersection
findNearestIntersection isectables ray = foldr (\isectable isect -> compareIntersection (intersect isectable ray) isect) NO_HIT isectables 

lighting :: [Object] -> [Light] -> Vec3 -> Vec3 -> Material -> Spectrum
lighting objects lights p n m = foldr (\light spectrum -> spectrum + (diffuseLighting objects p n (diffuse m) light)) black lights

diffuseLighting :: [Object] -> Vec3 -> Vec3 -> Spectrum -> Light -> Spectrum
diffuseLighting objects p n diffuseColor light = 
  if (dotNL > 0) && visible objects p lightPos
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

distance :: Intersection -> Double
distance NO_HIT = infinity
distance isect  = t isect

visible :: [Object] -> Vec3 -> Vec3 -> Bool
visible objList org target = foldr (\obj _visible -> ((distance (intersect obj shadowRay)) >= (Vec.length v)) && _visible) True objList
  where
    v = normalize $ target - org
    shadowRay = Ray org v

