module Helpers where

import Data.Time.Clock.POSIX

-- get timestamp in seconds multiplied by factor
-- previously, Int was passed instead of POSIXTime, but cmpiler threw error
--  however using integer value in place of 'n' worked as compiler inferred that
--  value as POSIXTime
--  now again, Int is being passed and used fromIntegral below, because
--   fromIntegral converts to Num which poses no problem
-- getTimestampX :: POSIXTime -> IO Int
getTimestampX :: Int -> IO Int
getTimestampX n = fmap (round . (* fromIntegral n)) getPOSIXTime

-- get timestamp in milliseconds
getTimestampMilli :: IO Int
getTimestampMilli = getTimestampX 1000

-- get timestamp in microseconds
getTimestampMicro :: IO Int
getTimestampMicro = getTimestampX 1000000
