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
merkleCombine = combine

{-- function to retun upper layer nodes after merging pairs
-- assumes list has non-zero even number of elements -}
combinePairs :: (Combinable a) => [a] -> [a]
combinePairs l = _combinePairs $ makeEven l
    where _combinePairs [a, b] = [merkleCombine a b]
          _combinePairs lst = _combinePairs (P.take 2 lst) ++ _combinePairs (P.drop 2 lst)

{-- makes the list even elment by adding first element -}
makeEven :: [a] -> [a]
makeEven l | P.length l `mod` 2 == 0 = l
           | otherwise = P.head l : l

merkletree:: [String] -> MerkleTree String
merkletree leaves  = MerkleTree {
          merkleRoot = root
        , leaves = leaves
    }
    where root = getRoot leaves
          getRoot [x] = x
          getRoot nodes = getRoot $ combinePairs $ makeEven nodes

{-- return merkle path formed by a leaf
-- each element in path is of form (0, e) or (1,e) determining which side to concatenate
-- example, if the node we seek is x and list has [(1, y), (0, z)] then, we concatenate like this:
--   z ++ (x++y)
--   i.e 1 stands for normal concatenation, 0 for reversing order -}
getMerklePath :: (Combinable a, Eq a) => MerkleTree a -> a -> [(Int, a)]
getMerklePath tree element
  | index >= 0 = map elementFromLevelIndex $ applyFunctionNTimes nextLevelIndex (evenLeaves,index) treeHeight
  | otherwise = []
  where index = element `indexIn` evenLeaves
        evenLeaves = makeEven (leaves tree)
        treeHeight = ceiling (logBase 2 (fromIntegral (P.length evenLeaves))) :: Int
        elementFromLevelIndex (level, ind) | ind `mod` 2 == 1 = ( 0, level !! (ind - 1))
                                              | otherwise = (1, level !! (ind+1))

nextLevelIndex :: (Combinable a) => ([a], Int) -> ([a], Int)
nextLevelIndex ([a, b], currInd) = ([combine a b], 0)
nextLevelIndex (currLev, currInd) = if len `mod` 2 == 0 then (combinePairs currLev, halveInd) else ( combinePairs (makeEven currLev), halveInd + 1)
    where len = P.length currLev
          halveInd = currInd `div` 2
