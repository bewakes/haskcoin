module MerkleTreeSpec where

import Test.Hspec

spec :: Spec
spec = do
    describe "Test" $ do
        it "tests" $ do
            head [1,2,3] `shouldBe` 1
