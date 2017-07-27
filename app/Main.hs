module Main where

import Block
import Helpers

main = do
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
