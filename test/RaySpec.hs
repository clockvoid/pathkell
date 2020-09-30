module RaySpec (spec) where

import Test.Hspec
import Base.Ray
import Base.Vec

ray1 :: Ray
ray1 = Ray (vec3 1 1 1) (vec3 2 2 2)

ray2 :: Ray
ray2 = Ray (vec3 2 2 2) (vec3 3 3 3)

ray3 :: Ray
ray3 = Ray (vec3 1 1 1) (vec3 2 2 2)

spec :: Spec
spec = do
  describe "ray" $
    it "= false" $
      ray1 == ray2 `shouldBe` False

  describe "ray" $
    it "= true" $
      ray1 == ray3 `shouldBe` True

  describe "ray" $
    it "org" $
      org ray1 `shouldBe` vec3 1 1 1

  describe "ray" $
    it "dir" $
      dir ray1 `shouldBe` nomalize $ vec3 2 2 2

