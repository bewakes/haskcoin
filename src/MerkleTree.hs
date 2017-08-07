module MerkleTree where

import Crypto.Hash
import Crypto.Hash.Algorithms
import Data.ByteString.UTF8 as B
import Helpers

data MerkleTree a = MerkleNode a (MerkleTree a) (MerkleTree a)
    | Empty
    deriving (Show)

constructMTree :: [String] -> MerlkeTree String
constructMTree dataHashes = mergetree (map makeleaf dataHashes)
    where makeleaf = \x -> (MerkleTree x) Empty Empty
          mergeTree (x:[]) = [MerkleTree (hash x x) x x]
          merteTree ls = hash (take 2 ls)
          getParents [] = []
          getParents (x:[]) = [hash [x x]]
          getParents trees = (hash (take 2 trees)) ++ getParents $ drop 2 trees
