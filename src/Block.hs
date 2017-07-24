module Block where

import Crypto.Hash
import Crypto.Hash.Algorithms
import Data.ByteString.UTF8 as B
import Helpers


data Block = Block {
      index :: Int
    , blockData :: String -- this might change
    , blockHash :: String
    , previousBlockHash :: String
    , timestamp :: Int -- this is unix timestamp in milliseconds
}

instance Show(Block) where
    show (Block ind blkData blkHash prevBlkHash tstamp) = "INDEX: "++show ind ++ "\n" ++
        "DATA: "++ (show blkData) ++ "\n" ++ "TIMESTAMP: " ++ (show tstamp) ++ "\n" ++
        "HASH:" ++ (show blkHash) ++ "\n\n"

-- calculation of hash from index, prevhash, timestamp and hash data
calculateHash :: Int -> String -> Int -> String -> IO String
calculateHash index prevHash timestamp hashData = do
    bytearr <- fmap B.fromString (return plain)
    let hsh = hash bytearr:: Digest SHA512
    return $ show hsh
    where plain = (show index) ++ prevHash ++ (show timestamp) ++ hashData


generateNewBlock :: Block -> String -> IO Block
generateNewBlock block newdata = do
    tstamp <- getTimestampMicro
    newhash <- calculateHash newind prevhash tstamp newdata
    return $ Block {
          index = newind
        , blockData = newdata
        , blockHash = newhash
        , previousBlockHash = prevhash
        , timestamp = tstamp -- TODO: update timestamp
    }
    where newind = (index block) + 1
          prevhash = blockHash block

getGenesisBlock :: IO Block
getGenesisBlock = do
    tstamp <- getTimestampMicro
    let prevhash = "44f359306cfdbcb62e489b55edb465b4e33c12d49e39c7e3063d7fb596b2e27b61e67f56bd0dc0aefa9ccc760abb18b6727c9117d23201f556a31d771deee5b5"
        blockdata = "Beginning of Transaction"
    blockhash <- calculateHash 0 prevhash tstamp blockdata
    return $ Block 0 blockdata blockhash prevhash tstamp
