module MerkleTree where

import Crypto.Hash
import Crypto.Hash.Algorithms
import Data.ByteString.UTF8 as B
import Helpers

data Child = L | R | ROOT
    deriving (Show)

data MerkleTree a = MerkleNode a (MerkleTree a) (MerkleTree a) Child
    | Empty Child
    deriving (Show)

-- get the value of the merkle tree root
getRootVal :: MerkleTree a -> a
getRootVal (MerkleNode x l r _) = x

-- make the root left child
makeleft :: MerkleTree a -> MerkleTree a
makeleft (MerkleNode x l r _) = MerkleNode x l r L
makeleft (Empty _) = Empty L

-- make the root right child
makeright:: MerkleTree a -> MerkleTree a
makeright  (MerkleNode x l r _) = MerkleNode x l r R
makeright (Empty _) = Empty R

-- construct merkle tree from hashed transaction data
constructMTree :: [String] -> MerkleTree String
constructMTree dataHashes = mergeTrees (map makeleaf dataHashes)
    where makeleaf = \x -> (MerkleNode x) (Empty L) (Empty R) ROOT

          mergeTrees [] = Empty ROOT
          mergeTrees (x:[]) = x
          mergeTrees ls = mergeTrees (getParents ls)

          getParents [] = []
          getParents (x:[]) = [(hash [x,x])]
          getParents trees = hash (Prelude.take 2 trees) : (getParents $ Prelude.drop 2 trees)

          hash [] = Empty ROOT
          hash (lc:rc:[]) = MerkleNode (merkleHashChildren (getRootVal lc) (getRootVal rc)) l r  ROOT
            where l = makeleft lc
                  r = makeright rc
