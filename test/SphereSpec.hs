module SphereSpec (spec) where

import Test.Hspec
import Sphere
import Object
import Ray
import Vec
import Color
import ReflectionType

radius1 :: Double
radius1 = 5

position1 :: Vec3
position1 = vec3 10 0 0

emission1 :: Color
emission1 = vec3 0 0 0

sphereColor1 :: Color
sphereColor1 = vec3 0 0 0

reflectionType1 :: ReflectionType
reflectionType1 = DIFFUSE

sphere1 :: Sphere
sphere1 = sphere radius1 position1 emission1 sphereColor1 reflectionType1

ray1 :: Ray
ray1 = Ray (vec3 0 0 0) (vec3 1 0 0)

spec :: Spec
spec =
  describe "sphere" $
    it "intersect" $
      intersect sphere1 ray1 `shouldBe` 5
      
