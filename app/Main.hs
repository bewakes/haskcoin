module Main where

import Block
import Helpers
import MerkleTree
import ThreadManager

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

threadTest :: IO ()
threadTest = do
    manager <- newManager
    threadid <- forkManaged manager $ thread_function 1
    putStrLn $ "Created thread_ID" ++ (show threadid)
    threadid <- forkManaged manager $ thread_function 2
    putStrLn $ "Created thread_ID" ++ (show threadid)

main = threadTest
