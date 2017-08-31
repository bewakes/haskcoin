module PeerManagerThread where

import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)

httptest = do
  manager <- newManager defaultManagerSettings

  putStrLn $ "hello from peer manager"
  request <- parseRequest "http://bewakes.com:8000"
  response <- httpLbs request manager

  putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
  print $ responseBody response
