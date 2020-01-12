module VecSpec (spec) where

import Test.Hspec
import Vec

vec1 :: Vec3
vec1 = vec3 1 1 1

vec2 :: Vec3
vec2 = vec3 2 2 2

spec :: Spec
spec = do
  describe "vec" $
    it "+" $
      (vec1 + vec2) `shouldBe` (vec3 3 3 3)

