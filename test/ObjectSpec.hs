module ObjectSpec (spec) where

import           Base.Ray
import           Base.Spectrum
import           Base.Vec
import           Intersectable
import           Intersection
import           Material
import           Object
import           Test.Hspec

radius1 :: Double
radius1 = 5

position1 :: Vec3
position1 = vec3 10 0 0

material1 :: Material
material1 = Material 0 0.9 1.5 $ Spectrum 0.1 0.3 0.5

sphere1 :: Object
sphere1 = Sphere position1 radius1 material1

ray1 :: Ray
ray1 = Ray (vec3 0 0 0) (vec3 1 0 0)

isect :: Intersection
isect = Intersection 4.9990000000000006 (Vec3 5.000000000000001 0 0) (Vec3 (-1) 0 0) material1

spec :: Spec
spec =
  describe "sphere" $
    it "intersect" $
      intersect sphere1 ray1 `shouldBe` isect

