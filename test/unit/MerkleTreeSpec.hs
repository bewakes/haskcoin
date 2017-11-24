module MerkleTreeSpec where

import MerkleTree

import Control.Exception (evaluate)

import Test.Hspec

spec :: Spec
spec = do
    describe "MerkleTree" $ do
        it "Checks Merkle Root" $ do
            merkleRoot tree `shouldBe` "11112345"
        it "Checks equality" $ do
            getMerklePath tree element  `shouldBe` [(0, "2"), (1, "45"), (0, "1111")]

tree = merkletree $ take 5 $ map show [1..]
element = "3"
