module Scene where

import Base.Ray
import Base.Vec
import Base.Spectrum

import Intersectable
import Light
import Intersection
import Material
import Object

import Numeric.Limits

import Control.Monad.ST
import Control.Monad
import Data.STRef

data Scene = Scene
  { intersectables :: [Object]
  , lights :: [Light]
  } deriving Show

specularReflection :: Int -> Scene -> Double -> Spectrum -> Intersection -> Ray -> Spectrum
specularReflection depth scene ks l isect ray = l + c *|| ks * diffuse _material
  where
    _material = material isect
    r = reflect (dir ray) (n isect)
    c = trace (depth + 1) scene l (Ray (p isect) r)

diffuseReflection :: [Object] -> [Light] -> Spectrum -> Intersection -> Spectrum
diffuseReflection objects lights l isect = l + lighting objects lights (p isect) (n isect) (material isect)

intoReflaction :: Int -> Scene -> Double -> Spectrum -> Intersection -> Ray -> Spectrum
intoReflaction depth scene kt l isect ray = l + c *|| kt * diffuse _material
  where
    _material = material isect
    r = reflact (dir ray) (n isect) (1 / reflactiveIndex _material)
    c = trace (depth + 1) scene l (Ray (p isect) r)

toOutReflaction :: Int -> Scene -> Spectrum -> Intersection -> Ray -> Spectrum
toOutReflaction depth scene l isect ray = trace (depth + 1) scene l (Ray (p isect) r)
  where
    _material = material isect
    r = reflact (dir ray) (vmap (* (-1)) (n isect)) (reflactiveIndex _material)

decideL :: Double -> Double -> Double -> Int -> Scene -> Spectrum -> Intersection -> Ray -> Spectrum
decideL ks kt kd depth scene l isect ray = runST $ do
  refL <- newSTRef l
  when (0 < ks) (modifySTRef refL (+ _specular))
  when (0 < kt) (modifySTRef refL (+ _into))
  when (0 < kd) (modifySTRef refL (+ _diffuse))
  readSTRef refL
  where
    _specular = specularReflection depth scene ks l isect ray
    _into = intoReflaction depth scene kt l isect ray
    _diffuse = diffuseReflection (intersectables scene) (lights scene) l isect

trace :: Int -> Scene -> Spectrum -> Ray -> Spectrum
trace depth scene l ray
  | depth > 10      = black
  | isect == NO_HIT = black
  | isInto          = decideL ks kt kd depth scene l isect ray
  | not isInto      = toOutReflaction depth scene l isect ray
  | otherwise       = l
    where
      isect = findNearestIntersection (intersectables scene) ray
      m = material isect
      isInto = n isect `dot` dir ray < 0
      ks = reflective m
      kt = reflactive m
      kd = 1 - ks - kt

compareIntersection :: Intersection -> Intersection -> Intersection
compareIntersection isect NO_HIT = isect
compareIntersection NO_HIT isect = isect
compareIntersection NO_HIT NO_HIT = NO_HIT
compareIntersection isect1 isect2 = if intersectionT isect1 < intersectionT isect2 then isect1 else isect2

findNearestIntersection :: [Object] -> Ray -> Intersection
findNearestIntersection isectables ray = foldr (\isectable isect -> compareIntersection (isectable `intersect` ray) isect) NO_HIT isectables 

lighting :: [Object] -> [Light] -> Vec3 -> Vec3 -> Material -> Spectrum
lighting objects lights p n m = foldr (\light spectrum -> spectrum + diffuseLighting objects p n (diffuse m) light) black lights

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
    r = Base.Vec.length v
    factor = dotNL / (4 * pi * r * r)

distance :: Intersection -> Double
distance NO_HIT = infinity
distance isect  = t isect

visible :: [Object] -> Vec3 -> Vec3 -> Bool
visible objList org target = foldr (\obj _visible -> ((distance (intersect obj shadowRay)) >= (Base.Vec.length v)) && _visible) True objList
  where
    v = normalize $ target - org
    shadowRay = Ray org v

