module Main where

import Network
import Network.SSDP

discover :: IO ()
discover = do
  results <- sendSearch ssdp callback
  mapM_ put results
 where
  ssdp = ssdpSearch UpnpRootDevice Nothing Nothing
  callback from msg = return (from, msg)
  put (from, msg) =
    putStrLn $ "\nMsg [ " ++ show from ++ " ]:\n\n"
               ++ render msg

main :: IO ()
main = withSocketsDo $ do
  discover
