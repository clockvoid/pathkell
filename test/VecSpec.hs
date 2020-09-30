module VecSpec (spec) where

import Test.Hspec
import Base.Vec

vec1 :: Vec3
vec1 = vec3 1 1 1

vec2 :: Vec3
vec2 = vec3 2 2 2

spec :: Spec
spec = do
  describe "vec" $
    it "+" $
      (vec1 + vec2) `shouldBe` vec3 3 3 3

  describe "vec" $
    it "-" $
      (vec2 - vec1) `shouldBe` vec3 1 1 1

  describe "vec" $
    it "*" $
      (vec1 * vec2) `shouldBe` vec3 2 2 2

  describe "vec" $
    it "左からの拡大" $
      (3 |* vec1) `shouldBe` vec3 3 3 3

  describe "vec" $
    it "右からの拡大" $
      (vec1 *| 3) `shouldBe` vec3 3 3 3

  describe "vec" $
    it "右からの縮小" $
      (vec2 /| 2) `shouldBe` vec3 1 1 1
  
  describe "vec" $
    it "ドット積" $
      (vec1 `dot` vec2) `shouldBe` 6

  describe "vec" $
    it "クロス積" $
      (vec1 `cross` vec2) `shouldBe` vec3 0 0 0

  describe "vec" $
    it "大きさの二乗" $
      (lenSquared vec1) `shouldBe` 3

  describe "vec" $
    it "大きさ" $
      (Vec.length vec1) `shouldBe` (sqrt 3)

