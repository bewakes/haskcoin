module ExtendedEuclidianSpec where

import EllipticCurve
import Test.Hspec

spec :: Spec
spec = do
    describe "EllipticCurve: ExtendedEuclidian Algorithm" $ do
        it ("Checks a,b,g for m="++show m1++" and n="++show n1) $ do
            extendedEuclidAlgorithm m1 n1 `shouldBe` (-1, 4, 7)
        it ("Checks m*a + n*b = g for m="++show m1++" and n="++show n1) $ do
            m1*a1 + n1*b1 `shouldBe` g1

        it ("Checks a,b,g for m="++show m2++" and n="++show n2) $ do
            extendedEuclidAlgorithm m2 n2 `shouldBe` (-64, 47, 23)
        it ("Checks m*a + n*b = g for m="++show m2++" and n="++show n2) $ do
            m2*a2 + n2*b2 `shouldBe` g2

        it ("Checks a,b,g for m="++show m3++" and n="++show n3) $ do
            extendedEuclidAlgorithm m3 n3 `shouldBe` (-2, 1, 46)
        it ("Checks m*a + n*b = g for m="++show m3++" and n="++show n3) $ do
            m3*a3 + n3*b3 `shouldBe` g3

        it ("Checks a,b,g for m="++show m4++" and n="++show n4) $ do
            extendedEuclidAlgorithm m4 n4 `shouldBe` (1, 0, m4)
        it ("Checks m*a + n*b = g for m="++show m4++" and n="++show n4) $ do
            m4*a4 + n4*b4 `shouldBe` m4

    where (a1, b1, g1) = extendedEuclidAlgorithm m1 n1
          (a2, b2, g2) = extendedEuclidAlgorithm m2 n2
          (a3, b3, g3) = extendedEuclidAlgorithm m3 n3
          (a4, b4, g4) = extendedEuclidAlgorithm m3 n3


m1 = 77
n1 = 21

m2 = 2415
n2 = 3289

m3 = 4278
n3 = 8602

m4 = 546
n4 = m4*3
