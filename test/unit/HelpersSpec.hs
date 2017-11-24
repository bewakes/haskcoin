module HelpersSpec where

import Helpers

import Test.Hspec

spec :: Spec
spec = do
    -- testing binTreeHeight with leaves length even
    describe "Helpers" $ do
        it "Checks binary tree height with leaves length 2" $ do
            binTreeHeight 2 `shouldBe` 2
        it "Checks binary tree height with leaves length 4" $ do
            binTreeHeight 4 `shouldBe` 3
        it "Checks binary tree height with leaves length 6" $ do
            binTreeHeight 6 `shouldBe` 4
        it "Checks binary tree height with leaves length 8" $ do
            binTreeHeight 8 `shouldBe` 4
        it "Checks binary tree height with leaves length 16" $ do
            binTreeHeight 16 `shouldBe` 5
        it "Checks Apply function N times for value 6" $ do
            applyFunctionNTimes halveLenEven 6 ((binTreeHeight 6) - 1) `shouldBe` [6,4,2]
        it "Checks Apply function N times for value 5" $ do
            applyFunctionNTimes halveLenEven 5 ((binTreeHeight 5) -1 ) `shouldBe` [5,4,2]

halveLenEven len = if half `mod` 2 == 0 then half else half + 1
    where half = (len + 1) `div` 2
