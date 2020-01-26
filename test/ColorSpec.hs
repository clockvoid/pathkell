module ColorSpec (spec) where

import Test.Hspec
import Color
import Vec

color1 :: Color
color1 = color 1.0 1.0 1.0

color2 :: Color
color2 = color 0.5 0.3 0.08

vec1 :: Vec3
vec1 = vec3 1 2 3

spec :: Spec
spec = do
  describe "color" $
    it "255" $
      clamp color1 `shouldBe` (255, 255, 255)

  describe "color" $
    it "floating" $
      clamp color2 `shouldBe` (127, 76, 20)
      
  describe "color" $
    it "255 string" $
      clampToString color1 `shouldBe` "255 255 255"

  describe "color" $
    it "fromVec3" $
      fromVec3 vec1 `shouldBe` color 1 2 3
