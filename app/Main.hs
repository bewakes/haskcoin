module Main where

import Block
import Helpers
import MerkleTree
import ThreadManager
import PeerManagerThread

blocktest :: IO ()
blocktest = do
    blk <- getGenesisBlock
    putStrLn "Genesis Block: "
    putStrLn  $ show blk
    tstamp <- getTimestampMilli
    block <- generateNewBlock blk "New block 1"
    putStrLn "First Block: "
    putStrLn $ show block
    putStr "Validity of block 1: "
    isvalid <- isValidBlock block blk
    putStrLn $ show isvalid

thread_function :: Int -> IO ()
thread_function index = do
    putStrLn $ "Hey!! I am printing from a thread: " ++ (show index)
    l <- getLine
    putStrLn $ "Got Line " ++ l

threadTest :: IO ()
threadTest = do
    manager <- newManager
    threadid <- forkManaged manager $ httptest
    putStrLn $ "Created thread_ID" ++ (show threadid)
    waitAll manager

merkleTest :: IO ()
merkleTest = let
        tree = merkletree ["bibek", "pandey", "merkle", "test"]
    in do
        putStrLn $ show tree
        putStrLn $ show $ getMerklePath tree "pandey"

main = merkleTest
