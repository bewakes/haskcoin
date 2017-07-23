module Block where

import Crypto.Hash
import Crypto.Hash.Algorithms
import Data.ByteString.UTF8 as B

data Block = Block {
      index :: Int
    , blockData :: String -- this might change
    , blockHash :: String
    , previousBlockHash :: String
    , timestamp :: String -- this will change
}

-- calculation of hash from index, prevhash, timestamp and hash data
calculateHash :: Int -> String -> String -> String -> IO String
calculateHash index prevHash timestamp hashData = do
    bytearr <- fmap B.fromString (return plain)
    let hsh = hash bytearr:: Digest SHA512
    return $ show hsh
    where plain = (show index) ++ prevHash ++ timestamp ++ hashData


generateNewBlock :: Block -> String -> IO Block
generateNewBlock block newdata = do
    newhash <- calculateHash newind prevhash tstamp newdata
    return $ Block {
          index = newind
        , blockData = newdata
        , blockHash = newhash
        , previousBlockHash = prevhash
        , timestamp = tstamp -- TODO: update timestamp
    }
    where tstamp = timestamp block
          newind = (index block) + 1
          prevhash = blockHash block
