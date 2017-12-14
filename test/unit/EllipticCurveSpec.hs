module EllipticCurveSpec where

import EllipticCurve
import Test.Hspec

spec :: Spec
spec = do
    describe "EllipticCurve: ExtendedEuclidian Algorithm" $ do
        it ("Checks the addition of a point with infinity") $ do
            point1 + infinity `shouldBe` point1
        it ("Checks the addition of a point with infinity") $ do
            infinity + point1 `shouldBe` point1
        it ("Checks the double of a point " ++ show point1 ) $ do
            _double point1 `shouldBe` doubledPoint1
        it ("Checks scalar multiple of a point " ++ show point1 ++ " with 1") $ do
            scalePoint  point1 1 `shouldBe` point1
        it ("Checks scalar multiple of a point " ++ show point1 ++ " with 2") $ do
            scalePoint  point1 2 `shouldBe` doubledPoint1
        it ("Checks scalar multiple of a point " ++ show point1 ++ " with 3") $ do
            scalePoint  point1 3 `shouldBe` point1x3
        it ("Checks scalar multiple of a point " ++ show point1 ++ " with 4") $ do
            scalePoint  point1 4 `shouldBe` point1x4
        it ("Checks scalar multiple of a point " ++ show point1 ++ " with 5") $ do
            scalePoint  point1 5 `shouldBe` point1

curve1 = Curve 2 3 97

point1 = Point 3 6 curve1 False
doubledPoint1 = Point 80 10 curve1 False
point1x3 = Point 80 87 curve1 False
point1x4 = Point 3 91 curve1 False

infinity = Point 0 0 curve1 True
