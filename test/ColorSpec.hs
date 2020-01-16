module ColorSpec (spec) where

import Test.Hspec
import Color
import Vec

color1 :: Color
color1 = color 1 1 1

color2 :: Color
color2 = color 2 2 2

spec :: Spec
spec = do
  describe "color" $
    it "+" $
      (color1 + color2) `shouldBe` color 3 3 3

  describe "color" $
    it "-" $
      (color2 - color1) `shouldBe` color 1 1 1

  describe "color" $
    it "*" $
      (color1 * color2) `shouldBe` color 2 2 2

  describe "color" $
    it "左からの拡大" $
      (3 |* color1) `shouldBe` color 3 3 3

  describe "color" $
    it "右からの拡大" $
      (color1 *| 3) `shouldBe` color 3 3 3

  describe "color" $
    it "右からの縮小" $
      (color2 /| 2) `shouldBe` color 1 1 1
  
  describe "color" $
    it "ドット積" $
      (color1 `dot` color2) `shouldBe` 6

  describe "color" $
    it "クロス積" $
      (color1 `cross` color2) `shouldBe` color 0 0 0

  describe "color" $
    it "大きさの二乗" $
      (lenSquared color1) `shouldBe` 3

  describe "color" $
    it "大きさ" $
      (Vec.length color1) `shouldBe` (sqrt 3)

  describe "color" $
    it "background" $
      background `shouldBe` color 0 0 0
