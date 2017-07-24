module Helpers where

import Data.Time.Clock.POSIX

getTimestampMilli :: IO Int
getTimestampMilli = do
    tstamp <- fmap (round . (*1000)) getPOSIXTime
    let str = show tstamp
        intval = read str :: Int
    return intval


getTimestampMicro :: IO Int
getTimestampMicro = do
    tstamp <- fmap (round . (*1000000)) getPOSIXTime
    let str = show tstamp
        intval = read str :: Int
    return intval
