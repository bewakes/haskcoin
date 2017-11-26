module MerkleTreeSpec where

import MerkleTree

import Control.Exception (evaluate)

import Test.Hspec

spec :: Spec
spec = do
    describe "MerkleTree" $ do
        it "Checks Next level index" $ do
            nextLevelIndex (["1", "1", "2", "3", "4", "5"], 3) `shouldBe` (["11", "11", "23", "45"], 2)
        it "Checks Next level index" $ do
            nextLevelIndex (["11", "11", "23", "45"], 1) `shouldBe` (["1111", "2345"], 0)
        it "Checks Merkle Root" $ do
            merkleRoot tree `shouldBe` "11112345"
        it "Checks merklePath" $ do
            getMerklePath tree element  `shouldBe` [(0, "2"), (1, "45"), (0, "1111")]
        it "Checks merklePath" $ do
            getMerklePath tree1 element1 `shouldBe` [(0, "6"), (0, "45"), (0, "1123")]
        it "Checks merklePath" $ do
            getMerklePath tree2 element2 `shouldBe` [(1, "3"), (1, "45"), (0, "1111"), (1, "678910111213")]

tree = merkletree $ take 5 $ map show [1..]
element = "3"

tree1 = merkletree $ take 7 $ map show [1..]
element1 = "7"

tree2 = merkletree $ take 13 $ map show [1..]
element2 = "2"
