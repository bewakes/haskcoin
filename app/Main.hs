module Main where

import Block
import Helpers
import MerkleTree

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

thread_function :: IO ()
thread_function = do
    putStrLn "Hey!! I am printing from a thread"

threadTest :: IO ()
threadTest = do
    threadid <- forkManaged manager thread_function
    putStrLn "Created thread_ID" ++ (show threadid)
    where manager = newManager

main = threadTest
