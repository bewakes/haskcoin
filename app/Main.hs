module Main where

import Block
import Helpers
import MerkleTree
import ThreadManager
--import PeerManagerThread

blocktest :: IO ()
blocktest = do
    blk <- getGenesisBlock
    putStrLn "Genesis Block: "
    print blk
    tstamp <- getTimestampMilli
    block <- generateNewBlock blk "New block 1"
    putStrLn "First Block: "
    print block
    putStr "Validity of block 1: "
    isvalid <- isValidBlock block blk
    print isvalid

threadFunction :: Int -> IO ()
threadFunction index = do
    putStrLn $ "Hey!! I am printing from a thread: " ++ show index
    l <- getLine
    putStrLn $ "Got Line " ++ l

{- threadTest :: IO ()
threadTest = do
    manager <- newManager
    threadid <- forkManaged manager httptest
    putStrLn $ "Created thread_ID" ++ show threadid
    waitAll manager
-}

merkleTest :: IO ()
merkleTest = let
        tree = merkletree ["bibek", "pandey", "merkle", "test"]
    in do
        print tree
        print $ getMerklePath tree "pandey"

main = merkleTest
