{-# LANGUAGE FlexibleInstances #-}
module MerkleTree where

import Crypto.Hash
import Crypto.Hash.Algorithms
import Data.ByteString.UTF8 as B
import Helpers
import Prelude as P

class Combinable a where
    combine :: a -> a -> a

instance Combinable String where
    combine =  (++)

data MerkleTree a = MerkleTree {
      merkleRoot :: a
    , leaves :: [a]
    --, getMerklePath :: a -> [a]
    --, matchTree :: a -> [a]
    } deriving(Show)


merkleCombine ::(Combinable a) => a-> a -> a
merkleCombine a b = combine a b

-- function to retun upper layer nodes after merging pairs
-- assumes list has non-zero even number of elements
combinePairs :: (Combinable a) => [a] -> [a]
combinePairs [a, b] = [merkleCombine a b]
combinePairs lst = (combinePairs (P.take 2 lst)) ++ ( combinePairs (P.drop 2 lst))

-- makes the list even elment by adding first element
makeEven :: [a] -> [a]
makeEven l | P.length l `mod` 2 == 0 = l
           | otherwise = (P.head l) : l

constructMerkleTree :: [String] -> MerkleTree String
constructMerkleTree leaves  = MerkleTree {
          merkleRoot = root
        , leaves = leaves
    }
    where root = getRoot leaves
          getRoot [x] = x
          getRoot nodes = getRoot $ combinePairs $ makeEven nodes

-- return merkle path formed by a leaf
-- each element in path is of form (0, e) or (1,e) determining which side to concatenate
-- example, if the node we seek is x and list has [(1, y), (0, z)] then, we concatenate like this:
--   z ++ (x++y)
--   i.e 1 stands for normal concatenation, 0 for reversing order
getMerklePath :: (Combinable a, Eq a) => MerkleTree a -> a -> [(Int, a)]
getMerklePath tree element
    | index >= 0 = getPath (makeEven (leaves tree)) element
    | otherwise = []
    where index = element `indexIn` ( makeEven (leaves tree))
          getPath [root] _ = [] -- no need to include root, header has root which can be verified upon
          getPath nodes element = let
                        evennodes = makeEven nodes
                        index = element `indexIn` evennodes
                        sibling | index `mod` 2 == 0 = (1, evennodes !! (index+1))
                                | otherwise = (0, evennodes !! (index-1))
                        parentnodes = combinePairs evennodes
                        combinedHash | fst sibling == 1 = merkleCombine element (snd sibling)
                                     | otherwise = merkleCombine (snd sibling) element
                        in sibling : (getPath parentnodes combinedHash)
