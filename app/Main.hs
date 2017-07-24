module Main where

import Block
import Helpers

main = do
    blk <- getGenesisBlock
    putStr "Genesis Block: "
    putStrLn  $ show blk
    tstamp <- getTimestampMilli
    block <- generateNewBlock blk "New block 1"
    putStr "First Block: "
    putStrLn $ show block

{-main = do-}
  {-putStr "Enter something to hash: " >> hFlush stdout-}
  {-line1 <- fmap B.fromString getLine-}
  {-let hash1 = hash line1 :: Digest SHA512-}
  {-putStrLn $ "First hash is " ++ show hash1-}
  {-putStr "Enter something else to hash: " >> hFlush stdout-}
  {-line2 <- fmap B.fromString getLine-}
  {-let-}
    {-ctx1 :: Context SHA512-}
    {-ctx1 = hashInit-}
    {-ctx2 = hashUpdate ctx1 hash1-}
    {-ctx3 = hashUpdate ctx2 line2-}
    {-hash2 = hashFinalize ctx3-}
  {-putStrLn $ "Second hash is " ++ show hash2-}
